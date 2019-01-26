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
main = withTerminal $ runTerminalT printer

printer :: (MonadFormatPrinter m, MonadColorPrinter m) => m ()
printer = do
    lw <- getLineWidth
    putDoc $ mconcat
        [ pretty $ "The line width is " ++ show lw ++ " columns."
        , hardline 
        , annotate (foreground $ bright Blue) "This is blue!"
        , hardline
        , annotate bold ("Just bold!" <+> annotate (background $ dull Red) "BOLD ON RED BACKGROUND" <+> "..just bold again")
        , hardline
        , annotate (foreground $ bright Red) "Hallo Welt!"
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
        , hardline
        ]
    flush
