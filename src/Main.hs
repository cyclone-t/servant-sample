{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Network.Wai.Handler.Warp (run)
import           Servant

------------------------------
-- API Handler registration --
------------------------------

type MyAppAPI = "add" :> Capture "n1" Int :> Capture "n2" Int :> Get '[JSON] Int
           :<|> "div" :> Capture "n1" Int :> Capture "n2" Int :> Get '[JSON] Int

myAppServer :: Server MyAppAPI
myAppServer = addHandler
         :<|> divHandler

----------------------------
-- API Server application --
----------------------------

myAppApi :: Proxy MyAppAPI
myAppApi = Proxy

myAppApp :: Application
myAppApp = serve myAppApi myAppServer

------------------------
-- Running API Server --
------------------------

main :: IO ()
main = do
  let myAppPort = 8081
  run myAppPort myAppApp

---------------------------
-- API Handlers (sample) --
---------------------------

addHandler :: Int -> Int -> Handler Int
addHandler n1 n2 = do
  liftIO $ print "add"
  return $ n1 + n2

divHandler :: Int -> Int -> Handler Int
divHandler n1 n2 = do
  liftIO $ print "div"
  case n2 of
    0        -> throwError err400 {errBody = "Error division by Zero"}
    non_zero -> return $ n1 `div` non_zero
