{-# LANGUAGE DeriveDataTypeable #-}

import System.Environment (getArgs)
import Network.HTTP
import Network.Browser (browse, request)
import Text.JSON.Generic
type Grid = [[Int]]

data State2048 = State2048 {
  grid :: Grid,
  score :: Int,
  points :: Int,
  moved :: Bool,
  over :: Bool,
  won :: Bool,
  session_id :: String,
  zen :: String
} deriving (Eq, Show, Data, Typeable)

{- リストの右端から左端に詰めるように動かす -}
moveLastToFirst :: (Bool, [Int]) -> (Bool, [Int])
moveLastToFirst (b, [])       = (b, [])
moveLastToFirst (b, [x])      = (b, [x])
moveLastToFirst (b, 0:0:xs)   =
    let (b', moved) = (moveLastToFirst (b, 0:xs))
    in (b', moved ++ [0])
moveLastToFirst (b, 0:y:xs)   =
    let (b', moved) = (moveLastToFirst (b, y:xs))
    in (True, moved ++ [0])
moveLastToFirst (b, x:y:rest)
  | x == y                    =
    let (b', moved) = (moveLastToFirst (b, rest))
    in (True, [x + y] ++ moved ++ [0])
  | otherwise                 =
    let (b', moved) = (moveLastToFirst (b, y:rest))
    in (b', [x] ++ moved)

{- 4x4の入れ子整数リストを取って転置する -}
transposition :: Grid -> Grid
transposition = (foldr $ zipWith (:)) [[],[],[],[]]

{- 4x4の入れ子リストを左に動かす -}
{- TODO:Maybeとかで実装したい -}
moveLeft :: Grid -> Grid
moveLeft grid =
    let (b0, movedRow0) = moveLastToFirst(False, grid !! 0)
        (b1, movedRow1) = moveLastToFirst(b0, grid !! 1)
        (b2, movedRow2) = moveLastToFirst(b1, grid !! 2)
        (b3, movedRow3) = moveLastToFirst(b2, grid !! 3)
    in
      if b3 == True
      then [movedRow0,movedRow1,movedRow2,movedRow3]
      else []

moveRight :: Grid -> Grid
moveRight grid =
    map reverse $ moveLeft $ map reverse grid

moveUp :: Grid -> Grid
moveUp grid =
    transposition $ moveLeft $ transposition grid

moveDown :: Grid -> Grid
moveDown grid =
    transposition $ moveRight $ transposition grid

{- 盤面の評価値を返す -}
calcScore :: Grid -> Int
calcScore grid =
    let grid' = (map $ (map (\x -> if x == 0 then 1 else x))) grid
        log2 = (\x -> if x == 1 then 1 else 1 + log2(x `div` 2))
        logGrid = (map $ (map log2)) grid'
        logGridT = transposition logGrid
        diff (x:y:z:w:_) = (abs (x - y)) + (abs (y - z)) + (abs (z - w))
    in (+) (sum $ (map diff) logGrid) (sum $ (map diff) logGridT)

responseToData :: Response String -> State2048
responseToData rsp = decodeJSON $ rspBody rsp

gameLoop :: String -> State2048 -> Int -> IO String
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
