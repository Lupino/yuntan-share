{-# LANGUAGE OverloadedStrings #-}

module Share.DS.Share
  (
    createShare
  , updateShare
  , getShare
  , getShareByName
  , countShare
  , getShareList
  , countShareByFather
  , getShareListByFather
  , incrShareScore
  , incrShareCount
  , incrSharePatchCount
  ) where

import           Control.Monad           (void)
import           Database.MySQL.Simple   (Connection, Only (..), execute,
                                          insertID, query, query_)

import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.String             (fromString)
import           Data.UnixTime

import           Share.Types
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createShare :: UserName -> ShareID -> TablePrefix -> Connection -> IO ShareID
createShare name fid prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (name, fid, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_shares` "
                                  , "(`name`, `father_id`, `created_at`)"
                                  , " VALUES "
                                  , "(?, ?, ?)"
                                  ]

updateShare :: ShareID -> ShareID -> TablePrefix -> Connection -> IO ()
updateShare sid fid prefix conn = void $ execute conn sql (fid, sid)

  where sql = fromString $ concat [ "UPDATE `", prefix, "_shares` "
                                  , "SET `father_id` = ? "
                                  , "WHERE `id` = ?"
                                  ]

getShare :: ShareID -> TablePrefix -> Connection -> IO (Maybe Share)
getShare sid prefix conn = listToMaybe <$> query conn sql (Only sid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_shares` WHERE `id`=?"]

getShareByName :: UserName -> TablePrefix -> Connection -> IO (Maybe Share)
getShareByName name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_shares` WHERE `name`=?"]

countShare :: TablePrefix -> Connection -> IO Int64
countShare prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_shares`" ]

getShareList :: From -> Size -> OrderBy -> TablePrefix -> Connection -> IO [Share]
getShareList from size o prefix conn = query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_shares` ", show o, " LIMIT ?,?" ]

countShareByFather :: ShareID -> TablePrefix -> Connection -> IO Int64
countShareByFather fid prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only fid)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_shares` WHERE `father_id` = ?" ]

getShareListByFather :: ShareID -> From -> Size -> OrderBy -> TablePrefix -> Connection -> IO [Share]
getShareListByFather fid from size o prefix conn = query conn sql (fid, from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_shares` WHERE `father_id` = ? ", show o, " LIMIT ?,?" ]

incrShareScore :: ShareID -> Score -> TablePrefix -> Connection -> IO Int64
incrShareScore sid score prefix conn =
  execute conn sql (score, sid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_shares` "
                                  , "SET `score` = `score` + ? "
                                  , "WHERE `id`=?"
                                  ]

incrShareCount :: ShareID -> Count -> TablePrefix -> Connection -> IO Int64
incrShareCount sid count prefix conn =
  execute conn sql (abs count, sid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_shares` "
                                  , "SET `count` = `count` " ++ op ++ " ? "
                                  , "WHERE `id`=?"
                                  ]

        op | count < 0 = "-"
           | otherwise = "+"


incrSharePatchCount :: ShareID -> Count -> TablePrefix -> Connection -> IO Int64
incrSharePatchCount sid count prefix conn =
  execute conn sql (count, sid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_shares` "
                                  , "SET `patch_count` = `patch_count` + ? "
                                  , "WHERE `id`=?"
                                  ]
