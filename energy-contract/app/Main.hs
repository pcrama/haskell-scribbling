{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Data202303Row = Data202303Row
  { row_timestamp :: Int -- ^ seconds since unix epoch
  , row_pv2012_kWh :: Maybe Double -- ^ infrequent manual readings of PV installation's production (panels installed in 2012)
  , row_pv2022_kWh :: Maybe Double -- ^ automatic reading of second PV installation's production (panels installed in 2022)
  , row_peak_conso_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy consumption from the grid during peak hours
  , row_off_conso_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy consumption from the grid during off hours
  , row_peak_inj_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy injection into the grid during peak hours
  , row_off_inj_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy injection into the grid during off hours
  , row_gas_m3 :: Maybe Double -- ^ infrequent manual readings of cumulative gas consumption
  , row_water_m3 :: Maybe Double -- ^ infrequent manual readings of cumulative water consumption
  } deriving (Show)

instance FromRow Data202303Row where
  fromRow = Data202303Row <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  r <- query_ conn "SELECT * from data_202303 where pv2012_kWh is not null order by timestamp desc limit 10" :: IO [Data202303Row]
  mapM_ print r
  close conn
