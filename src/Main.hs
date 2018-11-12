module Main where

import Mud

import Data.Monoid
import System.Environment
import Text.Read

main :: IO ()
main = do getArgs >>= \case
            []     -> server 8080
            [port] -> case readMaybe port of
                        Just p -> server p
                        Nothing -> usage
            _      -> usage

usage :: IO ()
usage = do name <- getProgName
           putStrLn $ "Usage: " <> name <> " [port]"
