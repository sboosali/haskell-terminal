{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Control.Monad.Terminal.TerminalT
  ( TerminalT ()
  , runTerminalT
  )
where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception               as E
import           Control.Monad                   (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Foldable                   (forM_)
import qualified Data.Text                       as Text
import qualified Data.Text.Prettyprint.Doc       as PP

import qualified Control.Monad.Terminal.Decoder  as T
import qualified Control.Monad.Terminal.Input    as T
import qualified Control.Monad.Terminal.Monad    as T
import qualified Control.Monad.Terminal.Printer  as T
import qualified Control.Monad.Terminal.Terminal as T

newtype TerminalT t m a
  = TerminalT (ReaderT t m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runTerminalT :: (MonadIO m, MonadMask m, T.Terminal t) => TerminalT t m a -> t -> m a
runTerminalT (TerminalT action) t = runReaderT action t

instance MonadTrans (TerminalT t) where
  lift = TerminalT . lift

instance (MonadIO m, T.Terminal t) => T.MonadInput (TerminalT t m) where
  waitMapInterruptAndEvents f = TerminalT $ do
    ansi <- ask
    liftIO $ atomically $ f (T.termSignal ansi >> pure ()) (T.termEvent ansi)

instance (MonadIO m, MonadThrow m, T.Terminal t) => T.MonadPrinter (TerminalT t m) where
  putChar c = TerminalT $ do
    ansi <- ask
    when (safeChar c) $
      liftIO $ atomically $ T.termCommand ansi $! Text.singleton c
  putString cs = TerminalT $ do
    ansi <- ask
    liftIO $ forM_ (filter safeChar cs) $ \c->
      atomically $ T.termCommand ansi $! Text.singleton c
  putText t = TerminalT $ do
    ansi <- ask
    liftIO $ loop (atomically . T.termCommand ansi) (Text.filter safeChar t)
    where
      loop out t0
        | Text.null t0 = pure ()
        | otherwise    = let (t1,t2) = Text.splitAt 80 t0
                         in  out t1 >> loop out t2
  flush = TerminalT $ do
    ansi <- ask
    liftIO  $ atomically $ T.termFlush ansi
  getLineWidth = snd <$> T.getScreenSize

instance (MonadIO m, MonadThrow m, T.Terminal t) => T.MonadPrettyPrinter (TerminalT t m) where
  data Annotation (TerminalT t m)
    = Bold
    | Italic
    | Underlined
    | Inverted
    | Foreground T.Color
    | Background T.Color deriving (Eq, Ord, Show)
  putDocLn doc = T.putDoc doc >> T.putLn
  putDoc doc = do
    w <- T.getLineWidth
    T.resetAnnotations
    render [] (sdoc w)
    T.resetAnnotations
    T.flush
    where
      options w   = PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine w 1.0 }
      sdoc w      = PP.layoutSmart (options w) doc
      oldFG []               = Nothing
      oldFG (Foreground c:_) = Just c
      oldFG (_:xs)           = oldFG xs
      oldBG []               = Nothing
      oldBG (Background c:_) = Just c
      oldBG (_:xs)           = oldBG xs
      render anns = \case
        PP.SFail           -> pure ()
        PP.SEmpty          -> pure ()
        PP.SChar c ss      -> T.putChar c >> render anns ss
        PP.SText _ t ss    -> T.putText t >> render anns ss
        PP.SLine n ss      -> T.putLn >> T.putText (Text.replicate n " ") >> render anns ss
        PP.SAnnPush ann ss -> T.setAnnotation ann >> render (ann:anns) ss
        PP.SAnnPop ss      -> case anns of
          []                     -> render [] ss
          (Bold         :anns')
            | Bold       `elem` anns' -> pure ()
            | otherwise               -> T.resetAnnotation Bold       >> render anns' ss
          (Italic       :anns')
            | Italic     `elem` anns' -> pure ()
            | otherwise               -> T.resetAnnotation Italic     >> render anns' ss
          (Underlined   :anns')
            | Underlined `elem` anns' -> pure ()
            | otherwise               -> T.resetAnnotation Underlined >> render anns' ss
          (Inverted     :anns')
            | Inverted   `elem` anns' -> pure ()
            | otherwise               -> T.resetAnnotation Inverted   >> render anns' ss
          (Foreground c :anns') -> case oldFG anns' of
            Just d  -> T.setAnnotation   (Foreground d) >> render anns' ss
            Nothing -> T.resetAnnotation (Foreground c) >> render anns' ss
          (Background c :anns') -> case oldBG anns' of
            Just d  -> T.setAnnotation   (Background d) >> render anns' ss
            Nothing -> T.resetAnnotation (Background c) >> render anns' ss

  setAnnotation Bold                                      = write "\ESC[1m"
  setAnnotation Italic                                    = pure ()
  setAnnotation Underlined                                = write "\ESC[4m"
  setAnnotation Inverted                                  = write "\ESC[7m"
  setAnnotation (Foreground (T.Color T.Dull   T.Black  )) = write "\ESC[30m"
  setAnnotation (Foreground (T.Color T.Dull   T.Red    )) = write "\ESC[31m"
  setAnnotation (Foreground (T.Color T.Dull   T.Green  )) = write "\ESC[32m"
  setAnnotation (Foreground (T.Color T.Dull   T.Yellow )) = write "\ESC[33m"
  setAnnotation (Foreground (T.Color T.Dull   T.Blue   )) = write "\ESC[34m"
  setAnnotation (Foreground (T.Color T.Dull   T.Magenta)) = write "\ESC[35m"
  setAnnotation (Foreground (T.Color T.Dull   T.Cyan   )) = write "\ESC[36m"
  setAnnotation (Foreground (T.Color T.Dull   T.White  )) = write "\ESC[37m"
  setAnnotation (Foreground (T.Color T.Bright T.Black  )) = write "\ESC[90m"
  setAnnotation (Foreground (T.Color T.Bright T.Red    )) = write "\ESC[91m"
  setAnnotation (Foreground (T.Color T.Bright T.Green  )) = write "\ESC[92m"
  setAnnotation (Foreground (T.Color T.Bright T.Yellow )) = write "\ESC[93m"
  setAnnotation (Foreground (T.Color T.Bright T.Blue   )) = write "\ESC[94m"
  setAnnotation (Foreground (T.Color T.Bright T.Magenta)) = write "\ESC[95m"
  setAnnotation (Foreground (T.Color T.Bright T.Cyan   )) = write "\ESC[96m"
  setAnnotation (Foreground (T.Color T.Bright T.White  )) = write "\ESC[97m"
  setAnnotation (Background (T.Color T.Dull   T.Black  )) = write "\ESC[40m"
  setAnnotation (Background (T.Color T.Dull   T.Red    )) = write "\ESC[41m"
  setAnnotation (Background (T.Color T.Dull   T.Green  )) = write "\ESC[42m"
  setAnnotation (Background (T.Color T.Dull   T.Yellow )) = write "\ESC[43m"
  setAnnotation (Background (T.Color T.Dull   T.Blue   )) = write "\ESC[44m"
  setAnnotation (Background (T.Color T.Dull   T.Magenta)) = write "\ESC[45m"
  setAnnotation (Background (T.Color T.Dull   T.Cyan   )) = write "\ESC[46m"
  setAnnotation (Background (T.Color T.Dull   T.White  )) = write "\ESC[47m"
  setAnnotation (Background (T.Color T.Bright T.Black  )) = write "\ESC[100m"
  setAnnotation (Background (T.Color T.Bright T.Red    )) = write "\ESC[101m"
  setAnnotation (Background (T.Color T.Bright T.Green  )) = write "\ESC[102m"
  setAnnotation (Background (T.Color T.Bright T.Yellow )) = write "\ESC[103m"
  setAnnotation (Background (T.Color T.Bright T.Blue   )) = write "\ESC[104m"
  setAnnotation (Background (T.Color T.Bright T.Magenta)) = write "\ESC[105m"
  setAnnotation (Background (T.Color T.Bright T.Cyan   )) = write "\ESC[106m"
  setAnnotation (Background (T.Color T.Bright T.White  )) = write "\ESC[107m"
  resetAnnotation Bold           = write "\ESC[22m"
  resetAnnotation Italic         = pure ()
  resetAnnotation Underlined     = write "\ESC[24m"
  resetAnnotation Inverted       = write "\ESC[27m"
  resetAnnotation (Foreground _) = write "\ESC[39m"
  resetAnnotation (Background _) = write "\ESC[49m"
  resetAnnotations               = write "\ESC[m"

instance (MonadIO m, MonadThrow m, T.Terminal t) => T.MonadFormatPrinter (TerminalT t m) where
  bold       = Bold
  italic     = Italic
  underlined = Underlined

instance (MonadIO m, MonadThrow m, T.Terminal t) => T.MonadColorPrinter (TerminalT t m) where
  inverted   = Inverted
  foreground = Foreground
  background = Background

instance (MonadIO m, MonadThrow m, T.Terminal t) => T.MonadTerminal (TerminalT t m) where
  moveCursorUp i                         = write $ "\ESC[" <> Text.pack (show i) <> "A"
  moveCursorDown i                       = write $ "\ESC[" <> Text.pack (show i) <> "B"
  moveCursorLeft i                       = write $ "\ESC[" <> Text.pack (show i) <> "D"
  moveCursorRight i                      = write $ "\ESC[" <> Text.pack (show i) <> "C"
  getCursorPosition = do
    write "\ESC[6n"
    T.flush
    waitForCursorPositionReport
    where
      -- Swallow all incoming events until either a cursor position report
      -- arrives or an Interrupt event occurs.
      -- An interrupt event will cause an `E.UserInterrupt` exception to be thrown.
      waitForCursorPositionReport = T.waitEvent >>= \case
        T.SignalEvent T.InterruptSignal            -> throwM E.UserInterrupt
        T.DeviceEvent (T.CursorPositionReport pos) -> pure pos
        _ -> waitForCursorPositionReport
  setCursorPosition (x,y)                = write $ "\ESC[" <> Text.pack (show $ x + 1) <> ";" <> Text.pack (show $ y + 1) <> "H"
  setCursorPositionVertical i            = write $ "\ESC[" <> Text.pack (show $ i + 1) <> "d"
  setCursorPositionHorizontal i          = write $ "\ESC[" <> Text.pack (show $ i + 1) <> "G"
  saveCursorPosition                     = write "\ESC7"
  restoreCursorPosition                  = write "\ESC8"
  showCursor                             = write "\ESC[?25h"
  hideCursor                             = write "\ESC[?25l"

  clearLine                              = write "\ESC[2K"
  clearLineLeft                          = write "\ESC[1K"
  clearLineRight                         = write "\ESC[0K"
  clearScreen                            = write "\ESC[2J"
  clearScreenAbove                       = write "\ESC[1J"
  clearScreenBelow                       = write "\ESC[0J"

  getScreenSize = TerminalT $ do
    ansi <- ask
    liftIO $ atomically $ T.termScreenSize ansi

  useAlternateScreenBuffer          True = write "\ESC[?1049h"
  useAlternateScreenBuffer         False = write "\ESC[?1049l"

-- | See https://en.wikipedia.org/wiki/List_of_Unicode_characters
safeChar :: Char -> Bool
safeChar c
  | c == '\n'   = True  -- Newline
  | c == '\t'   = True  -- Horizontal tab
  | c  < '\SP'  = False -- All other C0 control characters.
  | c  < '\DEL' = True  -- Printable remainder of ASCII. Start of C1.
  | c  < '\xa0' = False -- C1 up to start of Latin-1.
  | otherwise   = True

write :: (MonadIO m, T.Terminal t) => Text.Text -> TerminalT t m ()
write t = TerminalT $ do
  ansi <- ask
  liftIO $ atomically $ T.termCommand ansi t
