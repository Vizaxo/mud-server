module Main where

import FRP

import Data.Monoid
import System.Environment
import Text.Read

main :: IO ()
main = do getArgs >>= \case
            []     -> runServer 8080
            [port] -> case readMaybe port of
                        Just p -> runServer p
                        Nothing -> usage
            _      -> usage

usage :: IO ()
usage = do name <- getProgName
           putStrLn $ "Usage: " <> name <> " [port]"
