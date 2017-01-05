{-# LANGUAGE OverloadedStrings #-}

module Share.DS.Table
  (
    createTable
  ) where

import           Database.MySQL.Simple (Connection, execute_)

import           Data.Int              (Int64)
import           Data.String           (fromString)

import           Share.Types


createShareTable :: TablePrefix -> Connection -> IO Int64
createShareTable prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_shares` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `father_id` int(10) unsigned DEFAULT 0,"
                                  , "  `score` int(10) unsigned DEFAULT 0,"
                                  , "  `count` int(10) unsigned DEFAULT 0,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `name` (`name`),"
                                  , "  KEY `father_id` (`father_id`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createShareHistoryTable :: TablePrefix -> Connection -> IO Int64
createShareHistoryTable prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_share_history` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `share_id` int(10) unsigned NOT NULL,"
                                  , "  `src_id` int(10) unsigned NOT NULL,"
                                  , "  `summary` varchar(1500) DEFAULT NULL,"
                                  , "  `score` int(10) unsigned DEFAULT 0,"
                                  , "  `depth` int(10) unsigned DEFAULT 0,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  KEY `share_id` (`share_id`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]
createConfigTable :: TablePrefix -> Connection -> IO Int64
createConfigTable prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_config` ("
                                  , "  `key` varchar(128) NOT NULL,"
                                  , "  `value` varchar(255) NOT NULL,"
                                  , "  PRIMARY KEY (`key`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTable :: TablePrefix -> Connection -> IO Int64
createTable prefix conn = sum <$> mapM (\o -> o prefix conn) [ createShareTable
                                                             , createShareHistoryTable
                                                             , createConfigTable
                                                             ]
