{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------
--------------------------------------------------

module System.Terminal.POSIX.Foreign where

--------------------------------------------------
--------------------------------------------------

#include "hs_terminal.h"

--------------------------------------------------
--------------------------------------------------

import System.Terminal.POSIX.Types

--------------------------------------------------
--------------------------------------------------

import           "base" Foreign.C.Types
import           "base" Foreign.Marshal.Alloc
import           "base" Foreign.Ptr
import           "base" Foreign.Storable

--------------------------------------------------
--------------------------------------------------

foreign import ccall unsafe "tcgetattr"
  unsafeGetTermios :: CInt -> Ptr Termios -> IO CInt

foreign import ccall unsafe "tcsetattr"
  unsafeSetTermios :: CInt -> CInt -> Ptr Termios -> IO CInt

--------------------------------------------------

foreign import ccall unsafe "ioctl"
  unsafeIOCtl :: CInt -> CInt -> Ptr a -> IO CInt

--------------------------------------------------

foreign import ccall unsafe
  stg_sig_install :: CInt -> CInt -> Ptr a -> IO CInt

--------------------------------------------------
--------------------------------------------------
