{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Common.RuntimeMapping
  ( RuntimeMappings
  , RuntimeFieldType(..)
  , RuntimeMapping(..)
  ) where

import Bloodhound.Import
import qualified Data.Aeson.KeyMap as X


type RuntimeMappings = X.KeyMap RuntimeMapping

data RuntimeFieldType = BooleanRuntimeField
                      | CompositeRuntimeField
                      | DateRuntimeField
                      | DoubleRuntimeField
                      | GeoPointRuntimeField
                      | IpRuntimeField
                      | KeywordRuntimeField
                      | LongRuntimeField
                      | LookupRuntimeField
                      deriving (Eq, Show)

instance ToJSON RuntimeFieldType where
  toJSON BooleanRuntimeField = String "boolean"
  toJSON CompositeRuntimeField = String "composite"
  toJSON DateRuntimeField = String "date"
  toJSON DoubleRuntimeField = String "double"
  toJSON GeoPointRuntimeField = String "geo_point"
  toJSON IpRuntimeField = String "ip"
  toJSON KeywordRuntimeField = String "keyword"
  toJSON LongRuntimeField = String "long"
  toJSON LookupRuntimeField = String "lookup"

instance FromJSON RuntimeFieldType where
  parseJSON = withText "RuntimeFieldType" f
    where f "boolean" = pure BooleanRuntimeField
          f "composite" = pure CompositeRuntimeField
          f "date" = pure DateRuntimeField
          f "double" = pure DoubleRuntimeField
          f "geo_point" = pure GeoPointRuntimeField
          f "ip" = pure IpRuntimeField
          f "keyword" = pure KeywordRuntimeField
          f "long" = pure LongRuntimeField
          f "lookup" = pure LookupRuntimeField
          f x = fail $ "Unrecognised runtime field type: " ++ show x

data RuntimeMapping = RuntimeMapping
  { runtimeMappingType :: RuntimeFieldType,
    runtimeMappingScript :: Text
  } deriving (Eq, Show)

instance ToJSON RuntimeMapping where
  toJSON (RuntimeMapping t s) =
    omitNulls [ "type" .= t,
                "script" .= s
              ]


