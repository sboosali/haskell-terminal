{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module System.Terminal.TerminalT
  ( TerminalT ()
  , runTerminalT
  )
where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Foldable                   (forM_)
import qualified Data.Text                       as Text
import qualified Data.Text.Prettyprint.Doc       as PP
import           Prelude                     hiding (putChar)

import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadTerminal
import           System.Terminal.Terminal

-- | This monad transformer represents terminal applications.
--
-- It implements all classes in this module and should serve as a good
-- foundation for most use cases.
--
-- Note that it is not necessary nor recommended to have this type in
-- every signature. Keep your application abstract and mention `TerminalT`
-- only once at the top level.
--
-- Example:
--
-- @
-- main :: IO ()
-- main = `withTerminal` (`runTerminalT` myApplication)
--
-- myApplication :: (`MonadPrinter` m) => m ()
-- myApplication = do
--     `putTextLn` "Hello world!"
--     `flush`
-- @
newtype TerminalT t m a
  = TerminalT (ReaderT t m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Run a `TerminalT` application on the given terminal.
runTerminalT :: (MonadIO m, MonadMask m, Terminal t) => TerminalT t m a -> t -> m a
runTerminalT (TerminalT action) t = runReaderT action t

instance MonadTrans (TerminalT t) where
  lift = TerminalT . lift

instance (MonadIO m, Terminal t) => MonadInput (TerminalT t m) where
  waitMapSignalAndEvents f = TerminalT $ do
    t <- ask
    liftIO $ atomically $ f (termSignal t) (termEvent t)

instance (MonadIO m, MonadThrow m, Terminal t) => MonadPrinter (TerminalT t m) where
  putChar c =
      command $ PutText $ Text.singleton c
  putString cs = forM_ (filter safeChar cs) $ \c->
      command $ PutText $ Text.singleton c
  putText t = loop (Text.filter safeChar t)
    where
      loop t0
        | Text.null t0 = pure ()
        | otherwise    = let (t1,t2) = Text.splitAt 80 t0
                         in  command (PutText t1) >> loop t2
  flush = TerminalT $ do
      t <- ask
      liftIO  $ termFlush t
  getLineWidth = TerminalT $ do
      ansi <- ask
      snd <$> liftIO (atomically $ termScreenSize ansi)

instance (MonadIO m, MonadThrow m, Terminal t) => MonadPrettyPrinter (TerminalT t m) where
  data Annotation (TerminalT t m) = A SimpleAnnotation deriving (Eq, Ord, Show)
  putDocLn doc = putDoc doc >> putLn
  putDoc doc = do
    w <- getLineWidth
    resetAnnotations
    render [] (sdoc w)
    resetAnnotations
    flush
    where
      options w   = PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine w 1.0 }
      sdoc w      = PP.layoutSmart (options w) doc
      oldFG []                   = Nothing
      oldFG (A (Foreground c):_) = Just c
      oldFG (_:xs)               = oldFG xs
      oldBG []                   = Nothing
      oldBG (A (Background c):_) = Just c
      oldBG (_:xs)               = oldBG xs
      render anns = \case
        PP.SFail           -> pure ()
        PP.SEmpty          -> pure ()
        PP.SChar c ss      -> putChar c >> render anns ss
        PP.SText _ t ss    -> putText t >> render anns ss
        PP.SLine n ss      -> putLn >> putText (Text.replicate n " ") >> render anns ss
        PP.SAnnPush ann ss -> setAnnotation ann >> render (ann:anns) ss
        PP.SAnnPop ss      -> case anns of
          []                     -> render [] ss
          (A Bold         :anns')
            | A Bold       `elem` anns' -> pure ()
            | otherwise                 -> resetAnnotation (A Bold)       >> render anns' ss
          (A Italic       :anns')
            | A Italic     `elem` anns' -> pure ()
            | otherwise                 -> resetAnnotation (A Italic)     >> render anns' ss
          (A Underlined   :anns')
            | A Underlined `elem` anns' -> pure ()
            | otherwise                 -> resetAnnotation (A Underlined) >> render anns' ss
          (A Inverted     :anns')
            | A Inverted   `elem` anns' -> pure ()
            | otherwise                 -> resetAnnotation (A Inverted)   >> render anns' ss
          (A (Foreground c) :anns') -> case oldFG anns' of
            Just d  -> setAnnotation   (A (Foreground d)) >> render anns' ss
            Nothing -> resetAnnotation (A (Foreground c)) >> render anns' ss
          (A (Background c) :anns') -> case oldBG anns' of
            Just d  -> setAnnotation   (A (Background d)) >> render anns' ss
            Nothing -> resetAnnotation (A (Background c)) >> render anns' ss

  setAnnotation   (A a) = command (SetAnnotation   a)
  resetAnnotation (A a) = command (ResetAnnotation a)
  resetAnnotations      = command ResetAnnotations

instance (MonadIO m, MonadThrow m, Terminal t) => MonadFormatPrinter (TerminalT t m) where
  bold       = A Bold
  italic     = A Italic
  underlined = A Underlined

instance (MonadIO m, MonadThrow m, Terminal t) => MonadColorPrinter (TerminalT t m) where
  inverted   = A Inverted
  foreground = A . Foreground
  background = A . Background

instance (MonadIO m, MonadMask m, Terminal t) => MonadTerminal (TerminalT t m) where
  moveCursorUp                           = command . MoveCursorUp
  moveCursorDown                         = command . MoveCursorDown
  moveCursorLeft                         = command . MoveCursorLeft
  moveCursorRight                        = command . MoveCursorRight
  getCursorPosition = undefined
    {-
    write "\ESC[6n"
    flush
    waitForCursorPositionReport
    where
      -- Swallow all incoming events until either a cursor position report
      -- arrives or an Interrupt event occurs.
      -- An interrupt event will cause an `E.UserInterrupt` exception to be thrown.
      waitForCursorPositionReport = waitEvent >>= \case
        SignalEvent InterruptSignal            -> throwM E.UserInterrupt
        DeviceEvent (CursorPositionReport pos) -> pure pos
        _ -> waitForCursorPositionReport
     FIXME  -}
  setCursorPosition                      = command . SetCursorPosition
  setCursorPositionVertical              = command . SetCursorPositionVertical
  setCursorPositionHorizontal            = command . SetCursorPositionHorizontal
  saveCursorPosition                     = command SaveCursorPosition
  restoreCursorPosition                  = command RestoreCursorPosition
  showCursor                             = command ShowCursor
  hideCursor                             = command HideCursor

  clearLine                              = command ClearLine
  clearLineLeft                          = command ClearLineLeft
  clearLineRight                         = command ClearLineRight
  clearScreen                            = command ClearScreen
  clearScreenAbove                       = command ClearScreenAbove
  clearScreenBelow                       = command ClearScreenBelow

  getScreenSize = TerminalT $ do
    ansi <- ask
    liftIO $ atomically $ termScreenSize ansi

  withAlternateScreenBuffer = bracket_
    (command $ UseAlternateScreenBuffer True)
    (command $ UseAlternateScreenBuffer False)

-- | See https://en.wikipedia.org/wiki/List_of_Unicode_characters
safeChar :: Char -> Bool
safeChar c
  | c == '\n'   = True  -- Newline
  | c == '\t'   = True  -- Horizontal tab
  | c  < '\SP'  = False -- All other C0 control characters.
  | c  < '\DEL' = True  -- Printable remainder of ASCII. Start of C1.
  | c  < '\xa0' = False -- C1 up to start of Latin-1.
  | otherwise   = True

command :: (MonadIO m, Terminal t) => Command -> TerminalT t m ()
command c = TerminalT $ do
  t <- ask
  liftIO $ termCommand t c
