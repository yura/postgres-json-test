{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
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

type Database = ByteString
type Collection = ByteString

main :: IO ()
main = do
  let ?dbName = "bricky" in midLevel

data Config = Config Text
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data Position 
  = Position 
  { x :: Double
  , y :: Double
  , z :: Double
  , w :: Double
  , p :: Double
  , r :: Double
  , config :: Config
  } deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data Entity = Entity GeometryEntity | Distances [(Position, Maybe Double)]
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data Measurement
  = Measurement
    { id              :: UUID
    , measurementType :: Text
    , entity          :: Entity 
    , createdAt       :: UTCTime
    , updatedAt       :: UTCTime
    }
  deriving (Eq, Show, Generic)

data GeometryEntity
  = Point   { point :: Vector Double }
  | Plane   { point :: Vector Double, normal :: Vector Double
  } deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance FromRow Measurement
instance ToRow   Measurement

instance FromField Entity where
  fromField = fromJSONField

instance ToField Entity where
  toField = toField . JSON.encode

createMeasurement :: (?dbName :: Database) => Entity -> Text -> IO Measurement
createMeasurement e mt = do
  conn <- connectPostgreSQL $ "dbname=" <> ?dbName

  uuid <- nextRandom
  now <- getCurrentTime
  let measurement = Measurement uuid mt e now now

  _ <- executeMany conn "INSERT INTO measurements (id, measurement_type, entity, created_at, updated_at) VALUES (?, ?, ?, ?, ?)" [measurement]

  close conn
  return measurement

countQuery :: Collection -> Query
countQuery c | c == "measurements" = "SELECT COUNT(*) FROM measurements"

countAll :: (?dbName :: Database) => Collection -> IO Int
countAll collection = do
  conn <- connectPostgreSQL $ "dbname=" <> ?dbName
  [Only i] <- query_ conn $ countQuery collection
  close conn
  return i

findMeasurements :: (?dbName :: Database) => IO [Measurement]
findMeasurements = do
  conn <- connectPostgreSQL $ "dbname=" <> ?dbName
  ms <- query_ conn "SELECT * FROM measurements"
  close conn
  return ms

midLevel :: (?dbName :: Database) => IO ()
midLevel = do
  let plane = Entity (Plane (vector [1700, 745, -1324.547060451625]) (vector [-3.4972561521079736e-3,-4.4964721955685905e-3,0.9999837753369807]))
  plane' <- createMeasurement plane "floor"
  putStrLn $ "Plane: " <> show plane'

  let point = Entity (Point (vector [1700, 745, -1324.547060451625]))
  point' <- createMeasurement point "some point"
  putStrLn $ "Point: " <> show point'

  let distances = Distances [(Position 1700 0 1000 180 0 0 (Config "NUT 000"), Nothing), (Position 1700 0 900 180 0 0 (Config "NUT 000"), Just 800)]
  distances' <- createMeasurement distances "distances"
  putStrLn $ "Distances: " <> show distances'

  count <- countAll "measurements"
  putStrLn $ "Found: " <> show count <> " measurements"

  ms <- findMeasurements
  putStrLn $ show ms

