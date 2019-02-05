{-# LANGUAGE LambdaCase, RankNTypes #-}

--------------------------------------------------

module System.Terminal.POSIX.Types where

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
import qualified "base" Control.Concurrent.Async      as A
import qualified "base" Control.Exception             as E
import qualified "base" Data.Dynamic                  as Dyn
import qualified "base" Data.Text.IO                  as Text
import qualified "base" GHC.Conc                      as Conc
import qualified "base" System.IO                     as IO

--------------------------------------------------

import           "stm" Control.Concurrent.STM.TChan
import           "stm" Control.Concurrent.STM.TMVar
import           "stm" Control.Concurrent.STM.TVar
import           "stm" Control.Monad.STM

--------------------------------------------------

import qualified "bytestring" Data.ByteString        as BS
import qualified "bytestring" Data.ByteString.Char8  as BS8

--------------------------------------------------

import           "exceptions" Control.Monad.Catch hiding (handle)

--------------------------------------------------

--import           "async" Control.Concurrent.Async

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

--------------------------------------------------

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

data Winsize
  = Winsize
  { wsRow :: !CUShort
  , wsCol :: !CUShort
  } deriving (Eq, Ord, Show)

--------------------------------------------------

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
