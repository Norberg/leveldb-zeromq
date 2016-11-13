{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (release)
import           Data.Default
import           Data.Monoid
import           Database.LevelDB
import qualified Database.LevelDB.Streaming   as S
import           System.ZMQ4.Monadic

main' :: IO ()
main' = runResourceT $ do
    db <- open "/tmp/leveltest"
               defaultOptions{ createIfMissing = True
                             , cacheSize= 2048
                             }
    put db def "foo" "bar"
    res <- get db def "foo"
    liftIO $ print res

main'' :: IO ()
main'' = runZMQ $ do
    responder <- socket Rep
    bind responder "tcp://*:5555"

    forever $ do
        buffer <- receive responder
        send responder [] "Answer"

main = runResourceT $ runZMQ $ do
    responder <- socket Rep
    bind responder "tcp://*:5555"
    db <- open "/tmp/leveltest"
               defaultOptions{ createIfMissing = True
                             , cacheSize= 2048
                             }
    forever $ do
        buffer <- receive responder
        res <- get db def "foo"
        case res of
            Just r -> send responder [] r
            Nothing -> send responder [] "empty"
