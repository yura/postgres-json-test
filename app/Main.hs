{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import GHC.Generics
import Numeric.LinearAlgebra.Data

main :: IO ()
main = midLevel

lowLevel :: IO ()
lowLevel = do
  conn <- connectPostgreSQL "dbname=bricky"

  uuid <- nextRandom
  now <- getCurrentTime

  _ <- executeMany conn "INSERT INTO measurements (id, measurement_type, entity, created_at, updated_at) VALUES (?, ?, ?, ?, ?)" [(uuid, "floor" :: String, "{\"a\":\"b\"}" :: String, now, now)]

  return ()

data Measurement
  = Measurement
  { id              :: UUID
  , measurementType :: Text
  , entity          :: GeometryEntity
  , createdAt       :: UTCTime
  , updatedAt       :: UTCTime
  } deriving (Eq, Show, Generic)

data GeometryEntity
--  = Point   { point :: Vector Double }
  = Plane   { point :: Vector Double, normal :: Vector Double
  } deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance FromRow Measurement
instance ToRow   Measurement

instance FromField GeometryEntity where
  fromField = fromJSONField

instance ToField GeometryEntity where
  toField = toField . JSON.encode

midLevel :: IO ()
midLevel = do
  conn <- connectPostgreSQL "dbname=bricky"
 
  let plane = Plane (vector [1700, 745, -1324.547060451625]) (vector [-3.4972561521079736e-3,-4.4964721955685905e-3,0.9999837753369807])

  uuid <- nextRandom
  now <- getCurrentTime
  let measurement = Measurement uuid "floor" plane now now
  
  _ <- executeMany conn "INSERT INTO measurements (id, measurement_type, entity, created_at, updated_at) VALUES (?, ?, ?, ?, ?)" [measurement]

  ms <- findAll conn 
  putStrLn $ show ms

findAll :: Connection -> IO [Measurement]
findAll conn = query_ conn "SELECT * FROM measurements"

