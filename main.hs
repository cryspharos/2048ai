{-# LANGUAGE DeriveDataTypeable #-}

import System.Environment (getArgs)
import Network.HTTP
import Network.Browser (browse, request)
import Text.JSON.Generic

data Data2048 = Data2048 {
  grid :: [[Int]],
  score :: Int,
  points :: Int,
  moved :: Bool,
  over :: Bool,
  won :: Bool,
  session_id :: String,
  zen :: String
} deriving (Eq, Show, Data, Typeable)

responseToData :: Response String -> Data2048
responseToData rsp = decodeJSON $ rspBody rsp

gameLoop :: String -> Data2048 -> Int -> IO String
gameLoop host result rnd = if over result
  then return $ show $ score result
  else
    do
      putStrLn $ show (host, result)
      (newUrl, rsp) <- browse . request . getRequest $ host ++ "hi/state/" ++ session_id result ++ "/move/" ++ (show rnd) ++ "/json"
      gameLoop host (decodeJSON $ rspBody rsp) ((rnd + 1) `mod` 4)

startGameLoop :: String -> IO String
startGameLoop host = do
  (url,rsp) <- browse . request . getRequest $ host ++ "hi/start/json"
  gameLoop host (responseToData rsp) 0

main :: IO ()
main = do
  (host:_) <- getArgs
  result <- startGameLoop host
  putStrLn result
