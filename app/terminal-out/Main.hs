{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Text.Prettyprint.Doc
import           System.Terminal

import           Prelude                   hiding (putString)

main :: IO ()
main = withTerminal $ runTerminalT do
    putText "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
    flush
    liftIO (threadDelay 5000000)
    putLn
    putText "ABC"
    flush
    liftIO (threadDelay 5000000)
    
        