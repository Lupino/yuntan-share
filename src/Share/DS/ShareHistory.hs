{-# LANGUAGE OverloadedStrings #-}

module Share.DS.ShareHistory
  (
    createShareHistory
  , getShareHistory
  , countShareHistory
  , getShareHistoryList
  , statisticShareHistory
  , statisticShareHistoryList
  , countStatisticShareHistory
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

statisticShareHistory :: ShareID -> Int64 -> Int64 -> TablePrefix -> Connection -> IO (Maybe PatchResult)
statisticShareHistory sid start end prefix conn = do
  ret <- listToMaybe <$> query conn sql (sid, start, end)
  case ret of
    Just r  -> return . Just $ toPatchResult r
    Nothing -> return Nothing
  where sql = fromString $ concat [ "SELECT SUM(`score`),COUNT(`share_id`),`share_id` "
                                  , "FROM `", prefix, "_share_history` "
                                  , "WHERE `share_id` = ? AND `created_at` > ? AND `created_at` < ? "
                                  , "LIMIT 1"
                                  ]

toPatchResult :: (Double, Count, ShareID) -> PatchResult
toPatchResult (patchScore, patchCount, sid) = PatchResult { getPatchScore   = ceiling patchScore
                                                          , getPatchCount   = patchCount
                                                          , getPatchShareID = sid
                                                          , getPatchShare   = Nothing
                                                          }

statisticShareHistoryList :: Int64 -> Int64 -> From -> Size -> OrderBy
                          -> TablePrefix -> Connection -> IO [PatchResult]
statisticShareHistoryList start end from size o prefix conn = do
  ret <- query conn sql (start, end, from, size)
  return $ map toPatchResult ret
  where sql = fromString $ concat [ "SELECT SUM(`score`) as patch_score,COUNT(`share_id`) as patch_count,`share_id` "
                                  , "FROM `", prefix, "_share_history` "
                                  , "WHERE `created_at` > ? AND `created_at` < ? "
                                  , "GROUP BY `share_id` "
                                  , show o
                                  , " LIMIT ?, ?"
                                  ]

countStatisticShareHistory :: Int64 -> Int64 -> TablePrefix -> Connection -> IO Count
countStatisticShareHistory start end prefix conn = do
  maybe 0 fromOnly . listToMaybe <$> query conn sql (start, end)
  where sql = fromString $ concat [ "SELECT COUNT(*) FROM "
                                  , "(SELECT `share_id` FROM `", prefix, "_share_history` "
                                  , "WHERE `created_at` > ? AND `created_at` < ? "
                                  , "GROUP BY `share_id`) "
                                  , "AS gp"
                                  ]
