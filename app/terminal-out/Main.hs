{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Text.Prettyprint.Doc
import           System.Terminal

import           Prelude                   hiding ((<>))

main :: IO ()
main = withTerminal $ runTerminalT foo

foo :: (MonadTerminal m, MonadIO m) => m ()
foo = printer >> flush

printer :: (MonadFormatPrinter m, MonadColorPrinter m) => m ()
printer = putDoc $ annotate (foreground $ bright Blue) "This is blue!" <> line
                <> annotate bold ("Just bold!" <+> otherDoc <+> "..just bold again")
                <> doc

doc :: (MonadFormatPrinter m, MonadColorPrinter m, Annotation m ~ ann) => Doc ann
doc = mconcat
  [ annotate (foreground $ bright Red) "Hallo Welt!"
  , hardline
  , hang 10 $ "ssdfhsjdfhksjdhfkjsdhfks" <+> "hdfjkshdfkjshddh" <+> "fjksdhfkshdfkjshdfjks"
            <+> "hdfkjshdfjhskdjfhsjksdhfjshdfjshdkj" <+> "fhsdkjfhskjdfhjksdhfjksdhfjks"
            <+> "hdfkjshdfkh" <+> annotate bold "jdhfkjshdfkjshdfksjhdkfjhsdkjfhs" <+> "dkjfhskjdhfkjshdfkjshdfj"
            <+> "中國哲學書電子化計劃"
            <+> "jfksdfjlksdfjls"
            <+> "\x1d11e"
  , line
  , line
  , annotate (background $ dull Blue) "FOOBAR"
  ]

otherDoc :: (MonadColorPrinter m, Annotation m ~ ann) => Doc ann
otherDoc = annotate (background $ dull Red) "BOLD ON RED BACKGROUND"
