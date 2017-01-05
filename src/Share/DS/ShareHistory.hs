{-# LANGUAGE OverloadedStrings #-}

module Share.DS.ShareHistory
  (
    createShareHistory
  , getShareHistory
  , countShareHistory
  , getShareHistoryList
  ) where

import           Database.MySQL.Simple     (Connection, Only (..), execute,
                                            insertID, query, query_)

import           Data.Int                  (Int64)
import           Data.Maybe                (listToMaybe)
import           Data.String               (fromString)
import           Data.UnixTime

import           Dispatch.Types.ListResult (From, Size)
import           Dispatch.Types.OrderBy    (OrderBy)
import           Share.Types

createShareHistory :: ShareID -> ShareID -> Summary -> Score -> Depth -> TablePrefix -> Connection -> IO HistID
createShareHistory sid srcid summary score depth prefix conn = do
  t <- getUnixTime
  execute conn sql (sid, srcid, summary, score, depth, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_share_history` "
                                  , "(`share_id`, `src_id`, `summary`, `score`, `depth`, `created_at`)"
                                  , " VALUES "
                                  , "(?, ?, ?, ?, ?, ?)"
                                  ]

getShareHistory :: HistID -> TablePrefix -> Connection -> IO (Maybe ShareHistory)
getShareHistory hid prefix conn = listToMaybe <$> query conn sql (Only hid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_share_history` WHERE `id`=?"]

countShareHistory :: ShareID -> TablePrefix -> Connection -> IO Int64
countShareHistory sid prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only sid)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_share_history` WHERE `share_id`=?" ]

getShareHistoryList :: ShareID -> From -> Size -> OrderBy -> TablePrefix -> Connection -> IO [ShareHistory]
getShareHistoryList sid from size o prefix conn = query conn sql (sid, from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_share_history` "
                                  , "WHERE `share_id` = ? "
                                  , show o
                                  , " LIMIT ?,?"
                                  ]
