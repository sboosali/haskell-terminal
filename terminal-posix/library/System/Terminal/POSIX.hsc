{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase, RankNTypes #-}

--------------------------------------------------
--------------------------------------------------

module System.Terminal.POSIX

  ( withTerminal
  , LocalTerminal ()
  ) where

--------------------------------------------------
--------------------------------------------------

#include "Rts.h"
#include "hs_terminal.h"

--------------------------------------------------
--------------------------------------------------

import System.Terminal.POSIX.Types
import System.Terminal.POSIX.Foreign

--------------------------------------------------
--------------------------------------------------

import           "base" Control.Applicative
import           "base" Control.Concurrent
import           "base" Control.Monad                 (forM_, void, when)
import           "base" Control.Monad.IO.Class
import           "base" Data.Bits
import           "base" Data.Maybe
import           "base" Foreign.C.Types
import           "base" Foreign.Marshal.Alloc
import           "base" Foreign.Ptr
import           "base" Foreign.Storable
import           "base" System.Environment
import qualified "base" Control.Exception             as E
import qualified "base" Data.Dynamic                  as Dyn
import qualified "base" GHC.Conc                      as Conc
import qualified "base" System.IO                     as IO

--------------------------------------------------

import           "stm" Control.Concurrent.STM.TChan
import           "stm" Control.Concurrent.STM.TMVar
import           "stm" Control.Concurrent.STM.TVar
import           "stm" Control.Monad.STM

import           "stm" Control.Concurrent.STM.TChan (TChan)
import qualified "stm" Control.Concurrent.STM.TChan as TChan

--------------------------------------------------

import qualified "text" Data.Text.IO                  as Text

--------------------------------------------------

import qualified "bytestring" Data.ByteString        as BS
import qualified "bytestring" Data.ByteString.Char8  as BS8

--------------------------------------------------

import           "exceptions" Control.Monad.Catch hiding (handle)

--------------------------------------------------

import qualified "async" Control.Concurrent.Async      as A

--------------------------------------------------

import           System.Terminal.Terminal
import           System.Terminal.MonadInput
import           System.Terminal.Decoder
import           System.Terminal.Encoder

--------------------------------------------------
--------------------------------------------------

data LocalTerminal
    = LocalTerminal
    { localType              :: BS.ByteString
    , localEvent             :: STM Event
    , localInterrupt         :: STM ()
    , localGetWindowSize     :: IO (Rows, Cols)
    , localGetCursorPosition :: IO (Row, Col)
    }

instance Terminal LocalTerminal where
    termType              = localType
    termEvent             = localEvent
    termInterrupt         = localInterrupt
    termCommand _ c       = Text.hPutStr IO.stdout (ansiEncode c)
    termFlush _           = IO.hFlush IO.stdout
    termGetWindowSize     = localGetWindowSize
    termGetCursorPosition = localGetCursorPosition

--------------------------------------------------
--------------------------------------------------

withTerminal :: (MonadIO m, MonadMask m) => (LocalTerminal -> m a) -> m a
withTerminal action = do

    term           <- BS8.pack . fromMaybe "xterm" <$> liftIO (lookupEnv "TERM")
    mainThread     <- liftIO myThreadId
    interrupt      <- liftIO (newTVarIO False)
    events         <- liftIO newTChanIO
    windowSize     <- liftIO (newTVarIO =<< getWindowSize)
    cursorPosition <- liftIO newEmptyTMVarIO

    withTermiosSettings $ \termios->
        withResizeHandler (handleResize windowSize events) $
        withInputProcessing mainThread termios cursorPosition interrupt events $ 
        action LocalTerminal
            { localType              = term
            , localEvent             = readTChan events
            , localInterrupt         = swapTVar interrupt False >>= check
            , localGetWindowSize     = atomically (readTVar windowSize)
            , localGetCursorPosition = do
                -- Empty the result variable.
                atomically (void (takeTMVar cursorPosition) <|> pure ())
                -- Send cursor position report request.
                Text.hPutStr IO.stdout (ansiEncode GetCursorPosition)
                IO.hFlush IO.stdout
                -- Wait for the result variable to be filled by the input processor.
                atomically (takeTMVar cursorPosition)
            }

    where
        handleResize windowSize events = do
            ws <- getWindowSize
            atomically do
                writeTVar windowSize ws
                writeTChan events (WindowEvent $ WindowSizeChanged ws)

--------------------------------------------------

specialChar :: Termios -> Modifiers -> Char -> Maybe Event
specialChar t mods = \case
    c | c == termiosVINTR  t -> Just $ SignalEvent Interrupt
      | c == termiosVERASE t -> Just $ KeyEvent BackspaceKey mods
      | c == '\n'            -> Just $ KeyEvent EnterKey     mods
      | c == '\t'            -> Just $ KeyEvent TabKey       mods
      | c == '\b'            -> Just $ KeyEvent DeleteKey    mods
      | c == '\DEL'          -> Just $ KeyEvent DeleteKey    mods
      | otherwise            -> Nothing

withTermiosSettings :: (MonadIO m, MonadMask m) => (Termios -> m a) -> m a
withTermiosSettings fma = bracket before after between
  where
    before  = liftIO do
      termios <- getTermios
      let termios' = termios { termiosISIG = False, termiosICANON = False, termiosECHO = False }
      setTermios termios'
      pure termios
    after   = liftIO . setTermios
    between = fma

withResizeHandler :: (MonadIO m, MonadMask m) => IO () -> m a -> m a
withResizeHandler handler = bracket installHandler restoreHandler . const
  where
    installHandler = liftIO do
      Conc.ensureIOManagerIsRunning
      oldHandler <- Conc.setHandler (#const SIGWINCH) (Just (const handler, Dyn.toDyn handler))
      oldAction  <- stg_sig_install (#const SIGWINCH) (#const STG_SIG_HAN) nullPtr
      pure (oldHandler,oldAction)
    restoreHandler (oldHandler,oldAction) = liftIO do
      void $ Conc.setHandler (#const SIGWINCH) oldHandler
      void $ stg_sig_install (#const SIGWINCH) oldAction nullPtr
      pure ()

withInputProcessing :: (MonadIO m, MonadMask m) =>
    ThreadId -> Termios -> TMVar (Row, Col) -> TVar Bool -> TChan Event -> m a -> m a
withInputProcessing mainThread termios cursorPosition interrupt events =
    bracket (liftIO $ A.async $ run decoder) (liftIO . A.cancel) . const
    where
        run :: Decoder -> IO ()
        run d = do
            c <- IO.hGetChar IO.stdin
            case feedDecoder d mempty c of
              -- The decoder is not in final state.
              -- There are sequences depending on timing (escape either is literal
              -- escape or the beginning of a sequence).
              -- This block evaluates whether more input is available within
              -- a limited timespan. If this is the case it just recurses 
              -- with the decoder continuation.
              -- Otherwise, a NUL character is fed in order to tell the decoder
              -- that there is no more input belonging to the sequence.
              Left d' -> IO.hWaitForInput IO.stdin timeoutMilliseconds >>= \case
                  True  -> run d'
                  False -> case feedDecoder d' mempty '\NUL' of
                      Left d'' -> run d''
                      Right evs -> do
                          forM_ evs writeEvent
                          run decoder
              -- The decoder reached a final state.
              -- All recognized events are appended to the event stream.
              Right evs -> do
                  forM_ evs writeEvent
                  run decoder

        decoder :: Decoder
        decoder = ansiDecoder (specialChar termios)

        -- Adds events to the event stream and catches certain events
        -- that require special treatment.
        writeEvent :: Event -> IO ()
        writeEvent = \case
            SignalEvent Interrupt ->
                handleInterrupt
            ev@(DeviceEvent (CursorPositionReport pos)) -> atomically do
                -- One of the alternatives will succeed.
                -- The second one is not strict required but a fail safe in order
                -- to never block in case the terminal sends a report without request.
                putTMVar cursorPosition pos <|> void (swapTMVar cursorPosition pos)
                writeTChan events ev
            ev -> atomically (writeTChan events ev)

        -- This function is responsible for passing interrupt signals and
        -- eventually throwing an exception to the main thread in case it
        -- detects that the main thread is not serving its duty to process
        -- interrupt signals. It does this by setting a flag each time an interrupt
        -- occurs - if the flag is still set when a new interrupt occurs, it assumes
        -- the main thread is not responsive.
        handleInterrupt  :: IO ()
        handleInterrupt = do
            unhandledInterrupt <- atomically do
                writeTChan events (SignalEvent Interrupt)
                swapTVar interrupt True
            when unhandledInterrupt (E.throwTo mainThread E.UserInterrupt)

        -- The timeout duration has been choosen as a tradeoff between correctness
        -- (actual transmission or scheduling delays shall not be misinterpreted) and
        -- responsiveness for a human user (50 ms are barely noticable, but 1000 ms are).
        -- I.e. when the user presses the ESC key (as vim users sometimes do ;-)
        -- it shall be reflected in the application behavior quite instantly and
        -- certainly _before_ the user presses the next key (thereby assuming that the
        -- user is not able to type more than 20 characters per second).
        -- For escape sequences it shall also be taken into consideration that they are
        -- usually transmitted and received as chunks. Only on very rare occasions (buffer
        -- boundaries) it might happen that they are split right after the sequence
        -- introducer. In a modern environment with virtual terminals there is good
        -- reason to consider this more unlikely than a user that types so fast
        -- that his input might be misinterpreted as an escape sequence.
        timeoutMilliseconds :: Int
        timeoutMilliseconds  = 50

getWindowSize :: IO (Rows, Cols)
getWindowSize =
  alloca $ \ptr->
    unsafeIOCtl 0 (#const TIOCGWINSZ) ptr >>= \case
      0 -> peek ptr >>= \ws-> pure (fromIntegral $ wsRow ws, fromIntegral $ wsCol ws)
      _ -> undefined

getTermios :: IO Termios
getTermios = 
  alloca $ \ptr->
    unsafeGetTermios 0 ptr >>= \case
      0 -> peek ptr
      _ -> undefined

setTermios :: Termios -> IO ()
setTermios t =
  alloca $ \ptr->
    unsafeGetTermios 0 ptr >>= \case
      0 -> do 
        poke ptr t
        unsafeSetTermios 0 (#const TCSANOW) ptr >>= \case
          0 -> pure ()
          _ -> undefined
      _ -> undefined

--------------------------------------------------
--------------------------------------------------

data Winsize
  = Winsize
  { wsRow :: !CUShort
  , wsCol :: !CUShort
  } deriving (Eq, Ord, Show)

instance Storable Winsize where
  sizeOf    _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize)
  peek ptr    = Winsize
    <$> (#peek struct winsize, ws_row) ptr
    <*> (#peek struct winsize, ws_col) ptr
  poke ptr ws = do
    (#poke struct winsize, ws_row) ptr (wsRow ws)
    (#poke struct winsize, ws_col) ptr (wsCol ws)

--------------------------------------------------
--------------------------------------------------

data Termios
  = Termios
  { termiosVEOF   :: !Char
  , termiosVERASE :: !Char
  , termiosVINTR  :: !Char
  , termiosVKILL  :: !Char
  , termiosVQUIT  :: !Char
  , termiosISIG   :: !Bool
  , termiosICANON :: !Bool
  , termiosECHO   :: !Bool
  } deriving (Eq, Ord, Show)

instance Storable Termios where
  sizeOf    _ = (#size struct termios)
  alignment _ = (#alignment struct termios)
  peek ptr    = do
    lflag <- peekLFlag
    Termios
      <$> (toEnum . fromIntegral <$> peekVEOF)
      <*> (toEnum . fromIntegral <$> peekVERASE)
      <*> (toEnum . fromIntegral <$> peekVINTR)
      <*> (toEnum . fromIntegral <$> peekVKILL)
      <*> (toEnum . fromIntegral <$> peekVQUIT)
      <*> pure (lflag .&. (#const ISIG)   /= 0)
      <*> pure (lflag .&. (#const ICANON) /= 0)
      <*> pure (lflag .&. (#const ECHO)   /= 0)
    where
      peekVEOF       = (#peek struct termios, c_cc[VEOF])   ptr :: IO CUChar
      peekVERASE     = (#peek struct termios, c_cc[VERASE]) ptr :: IO CUChar
      peekVINTR      = (#peek struct termios, c_cc[VINTR])  ptr :: IO CUChar
      peekVKILL      = (#peek struct termios, c_cc[VKILL])  ptr :: IO CUChar
      peekVQUIT      = (#peek struct termios, c_cc[VQUIT])  ptr :: IO CUChar
      peekLFlag      = (#peek struct termios, c_lflag)      ptr :: IO CUInt 
  poke ptr termios = do
    pokeVEOF   $ fromIntegral $ fromEnum $ termiosVEOF   termios
    pokeVERASE $ fromIntegral $ fromEnum $ termiosVERASE termios
    pokeVINTR  $ fromIntegral $ fromEnum $ termiosVINTR  termios
    pokeVKILL  $ fromIntegral $ fromEnum $ termiosVKILL  termios
    pokeVQUIT  $ fromIntegral $ fromEnum $ termiosVQUIT  termios
    peekLFlag >>= \flag-> pokeLFlag (if termiosISIG   termios then flag .|. (#const ISIG)   else flag .&. complement (#const ISIG))
    peekLFlag >>= \flag-> pokeLFlag (if termiosICANON termios then flag .|. (#const ICANON) else flag .&. complement (#const ICANON))
    peekLFlag >>= \flag-> pokeLFlag (if termiosECHO   termios then flag .|. (#const ECHO)   else flag .&. complement (#const ECHO))
    where
      pokeVEOF       = (#poke struct termios, c_cc[VEOF])   ptr :: CUChar -> IO ()
      pokeVERASE     = (#poke struct termios, c_cc[VERASE]) ptr :: CUChar -> IO ()
      pokeVINTR      = (#poke struct termios, c_cc[VINTR])  ptr :: CUChar -> IO ()
      pokeVKILL      = (#poke struct termios, c_cc[VKILL])  ptr :: CUChar -> IO ()
      pokeVQUIT      = (#poke struct termios, c_cc[VQUIT])  ptr :: CUChar -> IO ()
      pokeLFlag      = (#poke struct termios, c_lflag)      ptr :: CUInt -> IO ()
      peekLFlag      = (#peek struct termios, c_lflag)      ptr :: IO CUInt

--------------------------------------------------
--------------------------------------------------
