{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (release)
import           Data.Default
import           Data.Monoid
import           Database.LevelDB
import qualified Database.LevelDB.Streaming   as S
import           System.ZMQ4.Monadic
import Control.Monad.Trans.Reader

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

main = runApp server

makeConfigFromEnvironment :: IO Config
makeConfigFromEnvironment =
    return $ Config runZeroMQ'

runZeroMQ' :: forall a z. ZMQ a z -> App a
runZeroMQ' = undefined
--runZeroMQ' a z = runZMQ a z

type App = ReaderT Config IO --- Skipped LoggingT for now to simplify
runApp :: App a -> IO a
runApp action = do
    cfg <- makeConfigFromEnvironment
    runReaderT action cfg

data Config  = Config{
 runZeroMQ :: forall a z. ZMQ a z -> App a
}

server :: App ()
server = do
    cfg <- ask
    responder <- runZeroMQ cfg $ socket Rep :: App (Socket z Rep)
    runZeroMQ cfg $ bind responder "tcp://*:5555"
    runZeroMQ cfg $ forever $ do
         buffer <- receive responder
         send responder [] "Answer"
