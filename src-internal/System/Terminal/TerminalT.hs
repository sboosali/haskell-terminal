{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module System.Terminal.TerminalT
  ( TerminalT ()
  , runTerminalT
  )
where

import           Control.Applicative                ((<|>))
import           Control.Concurrent.STM.TVar
import           Control.Monad                      (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Foldable                      (forM_)
import qualified Data.Text                       as Text
import qualified Data.Text.Prettyprint.Doc       as PP
import           Prelude                     hiding (putChar)
import           Control.Exception                  (AsyncException (..))

import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadScreen
import           System.Terminal.MonadTerminal
import qualified System.Terminal.Terminal        as T

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
    = TerminalT (StateT ScreenState (ReaderT t m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Run a `TerminalT` application on the given terminal.
runTerminalT :: (MonadIO m, MonadMask m, T.Terminal t) => TerminalT t m a -> t -> m a
runTerminalT ma t = runReaderT (evalStateT action sseDefault) t
    where
        TerminalT action = bracket_ before after ma
        before = do
            command (T.UseAutoWrap False)
        after = do
            command (T.UseAutoWrap True)

instance MonadTrans (TerminalT t) where
    lift = TerminalT . lift . lift

instance (MonadIO m, T.Terminal t) => MonadInput (TerminalT t m) where
    waitInterruptOrEvent f = TerminalT do
        t <- lift ask
        liftIO $ atomically $ f (T.termInterrupt t) (T.termEvent t)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadPrinter (TerminalT t m) where
    putLn = do
        command T.PutLn
        sseLn
    putChar c = do
        command (T.PutText $ Text.singleton $ sanitizeChar c)
        ssePut 1
    putString cs = do
        forM_ cs (command . T.PutText . Text.singleton . sanitizeChar)
        ssePut (length cs)
    putText t = do
        loop (sanitizeText t)
        ssePut (Text.length t)
        where
        loop t0
            | Text.null t0 = pure ()
            | otherwise    = let (t1,t2) = Text.splitAt 80 t0
                             in  command (T.PutText t1) >> loop t2
    flush = TerminalT do
        t <- lift ask
        liftIO $ T.termFlush t
    getLineWidth = TerminalT do
        t <- lift ask
        liftIO (snd <$> T.termGetScreenSize t)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadMarkupPrinter (TerminalT t m) where
    data Attribute (TerminalT t m) = AttributeT T.Attribute deriving (Eq, Ord, Show)
    setAttribute   (AttributeT a)  = command (T.SetAttribute   a)
    resetAttribute (AttributeT a)  = command (T.ResetAttribute a)
    resetAttributes                = command T.ResetAttributes
    resetsAttribute (AttributeT T.Bold       {}) (AttributeT T.Bold       {}) = True
    resetsAttribute (AttributeT T.Italic     {}) (AttributeT T.Italic     {}) = True
    resetsAttribute (AttributeT T.Underlined {}) (AttributeT T.Underlined {}) = True
    resetsAttribute (AttributeT T.Inverted   {}) (AttributeT T.Inverted   {}) = True
    resetsAttribute (AttributeT T.Foreground {}) (AttributeT T.Foreground {}) = True
    resetsAttribute (AttributeT T.Foreground {}) (AttributeT T.Background {}) = True
    resetsAttribute _                            _                            = False 

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadFormattingPrinter (TerminalT t m) where
    bold       = AttributeT T.Bold
    italic     = AttributeT T.Italic
    underlined = AttributeT T.Underlined
    inverted   = AttributeT T.Inverted

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadColorPrinter (TerminalT t m) where
    data Color (TerminalT t m) = ColorT T.Color deriving (Eq, Ord, Show)
    black                      = ColorT T.Black
    red                        = ColorT T.Red
    green                      = ColorT T.Green
    yellow                     = ColorT T.Yellow
    blue                       = ColorT T.Blue
    magenta                    = ColorT T.Magenta
    cyan                       = ColorT T.Cyan
    white                      = ColorT T.White
    bright (ColorT T.Black  )  = ColorT T.BrightBlack
    bright (ColorT T.Red    )  = ColorT T.BrightRed
    bright (ColorT T.Green  )  = ColorT T.BrightGreen
    bright (ColorT T.Yellow )  = ColorT T.BrightYellow
    bright (ColorT T.Blue   )  = ColorT T.BrightBlue
    bright (ColorT T.Magenta)  = ColorT T.BrightMagenta
    bright (ColorT T.Cyan   )  = ColorT T.BrightCyan
    bright (ColorT T.White  )  = ColorT T.BrightWhite
    bright (ColorT c        )  = ColorT c
    foreground (ColorT c)      = AttributeT (T.Foreground c)
    background (ColorT c)      = AttributeT (T.Background c)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadScreen (TerminalT t m) where
    showCursor               = command T.ShowCursor
    hideCursor               = command T.HideCursor

    saveCursor = do
        command T.SaveCursor
        sseSaveCursorPosition
    restoreCursor = do
        command T.RestoreCursor
        sseRestoreCursorPosition

    moveCursorUp i
        | i > 0     = command (T.MoveCursorUp i) >> sseMoveUp i
        | i < 0     = moveCursorDown i
        | otherwise = pure ()
    moveCursorDown i
        | i > 0     = command (T.MoveCursorDown i) >> sseMoveDown i
        | i < 0     = moveCursorUp i
        | otherwise = pure ()
    moveCursorLeft i
        | i > 0     = command (T.MoveCursorLeft i) >> sseMoveLeft i
        | i < 0     = moveCursorRight i
        | otherwise = pure ()
    moveCursorRight i
        | i > 0     = command (T.MoveCursorRight i) >> sseMoveRight i
        | i < 0     = moveCursorLeft i
        | otherwise = pure ()

    setCursorPosition pos = do
        command (T.SetCursorPosition pos)
        sseSetCursorPosition pos
    setCursorPositionVertical row = do
        command (T.SetCursorPositionVertical row)
        sseSetCursorPositionVertical row
    setCursorPositionHorizontal col = do
        command (T.SetCursorPositionHorizontal col)
        sseSetCursorPositionVertical col

    clearLine                = command T.ClearLine
    clearLineLeft            = command T.ClearLineLeft
    clearLineRight           = command T.ClearLineRight
    clearScreen              = command T.ClearScreen
    clearScreenAbove         = command T.ClearScreenAbove
    clearScreenBelow         = command T.ClearScreenBelow

    useAlternateScreenBuffer x = do
        command (T.UseAlternateScreenBuffer x)
        sseSetUnreliable

    getScreenSize = sseGetScreenSize
    getCursorPosition = \case
        Report     -> report
        Estimation -> maybe report pure =<< sseGetCursorPosition
        where
            report = do
                pos <- TerminalT (liftIO . T.termGetCursorPosition =<< lift ask)
                sseSetCursorPosition pos
                pure pos

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadTerminal (TerminalT t m) where

-- | See https://en.wikipedia.org/wiki/List_of_Unicode_characters
safeChar :: Char -> Bool
safeChar c
  | c  < '\SP'  = False -- All other C0 control characters.
  | c  < '\DEL' = True  -- Printable remainder of ASCII. Start of C1.
  | c  < '\xa0' = False -- C1 up to start of Latin-1.
  | otherwise   = True

sanitizeChar :: Char -> Char
sanitizeChar c = if safeChar c then c else '�'

sanitizeText :: Text.Text -> Text.Text
sanitizeText t
    | Text.any (not . safeChar) t = Text.map sanitizeChar t
    | otherwise                   = t

command :: (MonadIO m, MonadThrow m, T.Terminal t) => T.Command -> TerminalT t m ()
command c = TerminalT do
  t <- lift ask
  liftIO $ T.termCommand t c

---------------------------------------------------------------------------------------------------
-- SCREEN STATE
---------------------------------------------------------------------------------------------------

data ScreenState
  = ScreenState
  { sseScreenRows     :: {-# UNPACK #-} !Rows
  , sseScreenCols     :: {-# UNPACK #-} !Cols
  , sseCursorRow      :: {-# UNPACK #-} !Row
  , sseCursorCol      :: {-# UNPACK #-} !Col
  , sseCursorRowSaved :: {-# UNPACK #-} !Row
  , sseCursorColSaved :: {-# UNPACK #-} !Col
  , sseReliable       :: {-# UNPACK #-} !Bool
  }

sseDefault :: ScreenState
sseDefault = ScreenState 0 0 0 0 0 0 False

sseSetUnreliable :: (Monad m, T.Terminal t) => TerminalT t m ()
sseSetUnreliable = TerminalT do
    sse <- get
    put $! sse { sseReliable = False }

sseGetScreenSize :: (MonadIO m, T.Terminal t) => TerminalT t m (Rows,Cols)
sseGetScreenSize = do
    (rows,cols) <- TerminalT (liftIO . T.termGetScreenSize =<< lift ask)
    TerminalT do
        sse <- get
        when (sseScreenRows sse /= rows || sseScreenCols sse /= cols) do
            put $! sse { sseReliable = False, sseScreenRows = rows, sseScreenCols = cols }
    pure (rows,cols)

sseGetCursorPosition :: (Monad m, T.Terminal t) => TerminalT t m (Maybe (Row,Col))
sseGetCursorPosition = TerminalT do
    sse <- get
    pure if sseReliable sse
        then Just (sseCursorRow sse, sseCursorCol sse)
        else Nothing

sseSetCursorPosition :: (Monad m, T.Terminal t) => (Row, Col) -> TerminalT t m ()
sseSetCursorPosition (row, col) = TerminalT do
    sse <- get
    put $! sse { sseCursorRow = row, sseCursorCol = col }

sseSetCursorPositionVertical i = pure ()

ssetSetCursorPositionHorizontal = undefined

sseSaveCursorPosition = undefined

sseRestoreCursorPosition = undefined

sseLn :: (Monad m, T.Terminal t) => TerminalT t m ()
sseLn = pure ()

ssePut :: (Monad m, T.Terminal t) => Cols -> TerminalT t m ()
ssePut i = pure ()

sseMoveUp = undefined

sseMoveDown i = pure ()

sseMoveRight = undefined

sseMoveLeft = undefined
