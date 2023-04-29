{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Lens hiding ((.=))
import qualified Data.Text as T
import Control.Monad.IO.Class


data Tx = Tx
  { _address      :: T.Text
  , _address_type :: T.Text
  , _balance      :: Double
  } deriving (Show, Eq)

makeLenses ''Tx

data Vtx = Vtx
  { _address2      :: T.Text
  , _address_type2 :: T.Text
  , _balance2      :: Double
  , _bonus2        :: Double
  , _annonation2   :: T.Text
  } deriving (Show, Eq)

makeLenses ''Vtx

instance FromNamedRecord Tx where
  parseNamedRecord r = Tx
    <$> r .: "address"
    <*> r .: "address_type"
    <*> r .: "balance"

instance DefaultOrdered Vtx where
  headerOrder _ =
    header [ "address"
           , "address_type"
           , "balance"
           , "bonus"
           , "annonation"
           ]

instance ToNamedRecord Vtx where
  toNamedRecord Vtx{..} =
    namedRecord
    [ "address"      .= _address2
    , "address_type" .= _address_type2
    , "balance"      .= _balance2
    , "bonus"        .= _bonus2
    , "annonation"   .= _annonation2
    ]

type Vstack = [Vtx]

decodeTx :: BL.ByteString -> Either String (V.Vector Tx)
decodeTx = fmap snd . decodeByName

bVtx :: Tx -> Vtx
bVtx x  = Vtx
  (getAddress     x)
  (getAddressType x)
  (getBalance     x)
   0
  ""

getAddress :: Tx -> T.Text
getAddress = view address

getAddressType :: Tx -> T.Text
getAddressType = view address_type

getBalance :: Tx -> Double
getBalance = view balance

getAddress2 :: Vtx -> T.Text
getAddress2 = view address2

getAddressType2 :: Vtx -> T.Text
getAddressType2 = view address_type2

getBalance2 :: Vtx -> Double
getBalance2 = view balance2

parseVtx :: IO Vstack
parseVtx = do
  csv <- BL.readFile "2023Q1_TRUST_token_holders_snapshot-ethOnly.csv"
  let ptx = decodeTx csv
  let Right listTx = fmap V.toList ptx
  let b = fmap bVtx listTx

  return b

popTx :: S.StateT Vstack IO ()
popTx = do
  tx     <- popVtx
  stack  <- S.get

  if lastTx tx stack then do
    if noBonus tx then do
      let a = set annonation2 (annonation tx) tx
      io $ print tx
      io $ writeData a
      else do
        let a = getBalance2 tx * 16
        let b = set bonus2 a tx

        io $ print tx
        io $ writeData b

    else do
    if noBonus tx then do
      let a = set annonation2 (annonation tx) tx
      io $ print tx
      io $ writeData a
      popTx
      else do
        let a = set annonation2 (annonation tx) tx
        let b = getBalance2 tx * 16
        let c = set bonus2 b a

        io $ print tx
        io $ writeData c
        popTx


annonation :: Vtx -> T.Text
annonation x
  | getAddress2 x == "0xbaed5a2e8a9c3f2daa7332a90fea69f775ba9c80" = "DAO"
  | getAddress2 x == "0xf0b0b261635d6fb19aa028e5fe30eb732c855708" = "DAO"
  | getAddress2 x == "0xbaed5a2e8a9c3f2daa7332a90fea69f775ba9c80" = "DAO"
  | getAddress2 x == "0x9008d19f58aabd9ed0d60971565aa8510560ab41" = "GowSwap"
  | getAddress2 x == "0x65066a8b34c78b5270dacfce17e2d95ed8c6dd50" = "Execution"
  | getAddress2 x == "0xef13101c5bbd737cfb2bf00bbd38c626ad6952f7" = "Execution"
  | otherwise = ""

noBonus :: Vtx -> Bool
noBonus x
  | getAddress2 x == "0xbaed5a2e8a9c3f2daa7332a90fea69f775ba9c80"  = True
  | getAddress2 x == "0xf0b0b261635d6fb19aa028e5fe30eb732c855708"  = True
  | getAddress2 x == "0x65066a8b34c78b5270dacfce17e2d95ed8c6dd50"  = True
  | getAddress2 x == "0xef13101c5bbd737cfb2bf00bbd38c626ad6952f7"  = True
  | getAddressType2 x == "DEX" = True
  | otherwise = False

lastTx :: Vtx -> Vstack -> Bool
lastTx x y
  | null y = True
  | x == last y = True
  | otherwise = False

run :: Vstack -> IO ((), Vstack)
run = S.runStateT popTx

popVtx :: S.StateT Vstack IO Vtx
popVtx = do
  (x:xs) <- S.get
  S.put xs
  return x

io :: IO a -> S.StateT Vstack IO a
io = liftIO

wHeader :: IO ()
wHeader = BL.writeFile "TRUST-Holders-Bonus-2023Q1.csv"
  "address,address_type,balance,bonus,annonation\n"

writeData :: Vtx -> IO ()
writeData x = BL.appendFile "TRUST-Holders-Bonus-2023Q1.csv" (bytes x)

bytes :: Vtx -> BL.ByteString
bytes x = BL.drop 47 (encodeDefaultOrderedByName [x])

main :: IO ()
main = do
  wHeader
  txs <- parseVtx
  run txs
  print "done"
