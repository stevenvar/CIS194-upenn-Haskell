{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)


import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Data.List

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret forig fenc = 
  do
    lorig <- BS.readFile forig
    lenc <- BS.readFile fenc
    let diff = BS.filter  (/=0) . BS.pack $ BS.zipWith xor lorig lenc in 
      return diff

    

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key file =
  do
    lfile <- BS.readFile (file++".enc")
    let diff = BS.pack $ BS.zipWith xor lfile (BS.cycle key) 
    BS.writeFile file diff

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file =
  do
    lfile <- BS.readFile file
    let dec = decode lfile in
      return dec

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions =
  do
    Just lvics <- parseFile victims
    ltransac <- parseFile transactions
    let isBad ::  Transaction -> Bool 
        isBad (Transaction { tid = x} ) = x `elem` lvics
    return $ fmap (filter isBad) ltransac

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = foldr (\(f,t,s) m -> Map.insertWith (+) f s (Map.insertWith (+) t (-s) m))  Map.empty (map aux ts) 
  where aux (Transaction { from = x, to = y , amount = z }) = (y,x,z)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ Map.foldrWithKey maxv ("",0) m 
  where
    maxv n v (mn,mv) = if v > mv then (n,v) else (mn,mv)
-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tid = let (payers,payee) = Map.partition (>0) m
                   lpayer = Map.toList payers
                   lpayee = Map.toList payee 
                   lpayersort = sortBy (\x y -> compare (snd x) (snd y)) lpayer  
                   lpayeesort = sortBy (\x y -> compare (snd y) (snd x)) lpayee
               in
                 undoTs' lpayersort lpayeesort tid 
  where undoTs' [] _ _ = []
        undoTs' _ [] _ = []
        undoTs' _ _ [] = []
        undoTs' (payer:payers) (payee:payees) (i:ids) =
          (t:(undoTs' newpayers newpayees ids)) 
          where t =  Transaction (fst payer) (fst payee) s i
                s = (min (snd payer) (-(snd payee) ) )
                newpayers = if (snd payer == s) then payers else (fst payer, snd payer - s):payers
                newpayees = if (snd payee == -s) then payees else (fst payee, snd payee + s):payees
-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f ts = BS.writeFile f $ encode ts

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

truc = do
  mts <- getBadTs "victims.json" "transactions.json"
  case mts of
    Just ts -> do
      let flow = getFlow ts
      return $ flow 
    Nothing -> error "nul" 
