module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import System.Environment (getEnv)
import Data.Time.Clock (getCurrentTime)
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Text (Text)
import Control.Concurrent.MVar
import System.Exit
import Control.Monad.IO.Class
import System.Posix.Process (exitImmediately)

main :: IO ()
main = do
    putStrLn "API starting...."

    -- env vars
    port <- read <$> getEnv "PORT" :: IO Int

    -- counter used to crash the application
    counter <- newMVar 0 :: IO (MVar Int)

    -- scotty handler with ONE route
    scotty port $
      get "/" $ do
        now <- liftIO getCurrentTime
        currentCounter <- liftIO $ takeMVar counter
        liftIO $ case currentCounter of

          -- crash process every 5th request
          5 -> do
            putStrLn "ERROR: Uh oh.. Haskell needs a break :("

            -- this type of exit successfully bypasses WAI trying to keep the
            -- process alive
            exitImmediately (ExitFailure 1)

          a -> putMVar counter (a + 1)

        -- return JSON object with current time value
        json $ object ["currentTime" .= show now]
