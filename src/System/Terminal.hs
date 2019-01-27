{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}
module System.Terminal
  ( -- * Getting started
    -- ** withTerminal
    withTerminal
    -- ** TerminalT
  , runTerminalT
  , TerminalT ()
    -- * Printing & Screen Modification
    -- ** MonadPrinter
  , MonadPrinter (..)
    -- ** MonadPrettyPrinter
  , MonadPrettyPrinter (..)
    -- ** MonadFormatPrinter
  , MonadFormatPrinter (..)
    -- ** MonadColorPrinter
  , MonadColorPrinter (..)
  , dull
  , bright
  , BasicColor (..)
  , Color (..)
  , ColorMode (..)
    -- ** MonadTerminal
  , MonadTerminal (..)
  , withAlternateScreenBuffer
    -- * Event Processing
    -- *** waitEvent
  , waitEvent
    -- *** waitEventOrElse
  , waitEventOrElse
    -- *** waitSignalOrElse
  , waitSignalOrElse
  , MonadInput (..)
    -- ** Events
  , Event (..)
    -- *** Keys & Modifiers
  , Key (..)
  , Direction (..)
  , Modifiers ()
  , shiftKey
  , ctrlKey
  , altKey
  , metaKey
    -- *** Mouse Events
  , MouseEvent (..)
  , MouseButton (..)
    -- *** Window Events
  , WindowEvent (..)
    -- *** Signal Events
  , Signal (..) 
    -- *** Device Events
  , DeviceEvent (..)
    -- * Low-Level
    -- ** Terminal
  , Terminal (..)
    -- ** Misc
  , Row, Rows, Column, Columns
  , Command (..)
  , Decoder (..)
  , ansiDecoder
  , ansiEncode
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           System.Terminal.Decoder
import           System.Terminal.Encoder
import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadTerminal
import           System.Terminal.Terminal
import           System.Terminal.TerminalT
import qualified System.Terminal.Platform

-- | Run the given handler with the locally connected terminal (`System.IO.stdin` / `System.IO.stdout`).
--
-- @
-- import System.Terminal
--
-- main :: IO ()
-- main = withTerminal $ `runTerminalT` do
--     `putTextLn` "Hello world!"
--     `flush`
-- @
withTerminal :: (MonadIO m, MonadMask m) => (forall t. Terminal t => t -> m a) -> m a
withTerminal = System.Terminal.Platform.withTerminal

