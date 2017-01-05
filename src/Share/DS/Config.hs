{-# LANGUAGE OverloadedStrings #-}

module Share.DS.Config
  (
    setConfig
  , getConfig
  ) where

import           Database.MySQL.Simple (Connection, Only (..), execute, query)

import           Data.Int              (Int64)
import           Data.Maybe            (listToMaybe)
import           Data.String           (fromString)

import           Share.Types

setConfig :: String -> String -> TablePrefix -> Connection -> IO Int64
setConfig key value prefix conn = execute conn sql (key, value)

  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_config` "
                                  , "(`key`, `value`)"
                                  , " VALUES "
                                  , "(?, ?)"
                                  ]

getConfig :: String -> TablePrefix -> Connection -> IO String
getConfig key prefix conn = maybe "" fromOnly . listToMaybe <$> query conn sql (Only key)
  where sql = fromString $ concat [ "SELECT `value` FROM `", prefix, "_config` WHERE `key` = ?" ]
