{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Share.DS
  (
    ShareReq(..)
  , initShareState
  ) where

import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Haxl.Core                (BlockedFetch (..), DataSource,
                                           DataSourceName, Flags,
                                           PerformFetch (..), ShowP, State,
                                           StateKey, dataSourceName, fetch,
                                           putFailure, putSuccess, showp)

import           Share.DS.Config
import           Share.DS.Share
import           Share.DS.ShareHistory
import           Share.DS.Table
import           Share.Types
import           Yuntan.Types.HasMySQL    (HasMySQL, mysqlPool, tablePrefix)
import           Yuntan.Types.ListResult  (From, Size)
import           Yuntan.Types.OrderBy     (OrderBy)

import qualified Control.Exception        (SomeException, bracket_, try)
import           Data.Int                 (Int64)
import           Data.Pool                (withResource)
import           Database.MySQL.Simple    (Connection)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data ShareReq a where
  CreateTable                :: ShareReq Int64
  CreateShare                :: UserName -> ShareID -> ShareReq ShareID
  GetShare                   :: ShareID -> ShareReq (Maybe Share)
  GetShareByName             :: UserName -> ShareReq (Maybe Share)
  CountShare                 :: ShareReq Int64
  GetShareList               :: From -> Size -> OrderBy -> ShareReq [Share]
  CountShareByFather         :: ShareID -> ShareReq Int64
  GetShareListByFather       :: ShareID -> From -> Size -> OrderBy -> ShareReq [Share]
  IncrShareScore             :: ShareID -> Score -> ShareReq Int64
  IncrShareCount             :: ShareID -> Count -> ShareReq Int64
  IncrSharePatchCount        :: ShareID -> Count -> ShareReq Int64
  CreateShareHistory         :: ShareID -> ShareID -> Summary -> Score -> Depth -> ShareReq HistID
  GetShareHistory            :: HistID -> ShareReq (Maybe ShareHistory)
  CountShareHistory          :: ShareID -> ShareReq Int64
  GetShareHistoryList        :: ShareID -> From -> Size -> OrderBy -> ShareReq [ShareHistory]
  StatisticShareHistory      :: ShareID -> Int64 -> Int64 -> ShareReq (Maybe PatchResult)
  StatisticShareHistoryList  :: Int64 -> Int64 -> From -> Size -> OrderBy -> ShareReq [PatchResult]
  CountStatisticShareHistory :: Int64 -> Int64 -> ShareReq Count

  GetConfig                  :: String -> ShareReq String
  SetConfig                  :: String -> String -> ShareReq Int64

  deriving (Typeable)

deriving instance Eq (ShareReq a)
instance Hashable (ShareReq a) where
  hashWithSalt s CreateTable                              = hashWithSalt s ( 0::Int)
  hashWithSalt s (CreateShare un sid)                     = hashWithSalt s ( 1::Int, un, sid)
  hashWithSalt s (GetShare sid)                           = hashWithSalt s ( 2::Int, sid)
  hashWithSalt s (GetShareByName un)                      = hashWithSalt s ( 3::Int, un)
  hashWithSalt s CountShare                               = hashWithSalt s ( 4::Int)
  hashWithSalt s (GetShareList f si o)                    = hashWithSalt s ( 5::Int, f, si, o)
  hashWithSalt s (CountShareByFather fid)                 = hashWithSalt s ( 6::Int, fid)
  hashWithSalt s (GetShareListByFather fid f si o)        = hashWithSalt s ( 7::Int, fid, f, si, o)
  hashWithSalt s (IncrShareScore sid sc)                  = hashWithSalt s ( 8::Int, sid, sc)
  hashWithSalt s (IncrShareCount sid c)                   = hashWithSalt s ( 9::Int, sid, c)
  hashWithSalt s (IncrSharePatchCount sid c)              = hashWithSalt s (10::Int, sid, c)
  hashWithSalt s (CreateShareHistory sid rid sm sc d)     = hashWithSalt s (11::Int, sid, rid, sm, sc, d)
  hashWithSalt s (GetShareHistory hid)                    = hashWithSalt s (12::Int, hid)
  hashWithSalt s (CountShareHistory sid)                  = hashWithSalt s (13::Int, sid)
  hashWithSalt s (GetShareHistoryList sid f si o)         = hashWithSalt s (14::Int, sid, f, si, o)
  hashWithSalt s (StatisticShareHistory sid st ed)        = hashWithSalt s (15::Int, sid, st, ed)
  hashWithSalt s (StatisticShareHistoryList st ed f si o) = hashWithSalt s (16::Int, st, ed, f, si, o)
  hashWithSalt s (CountStatisticShareHistory st ed)       = hashWithSalt s (17::Int, st, ed)
  hashWithSalt s (GetConfig key)                          = hashWithSalt s (18::Int, key)
  hashWithSalt s (SetConfig key value)                    = hashWithSalt s (19::Int, key, value)

deriving instance Show (ShareReq a)
instance ShowP ShareReq where showp = show

instance StateKey ShareReq where
  data State ShareReq = ShareState { numThreads :: Int }

instance DataSourceName ShareReq where
  dataSourceName _ = "ShareDataSource"

instance HasMySQL u => DataSource u ShareReq where
  fetch = shareFetch

shareFetch
  :: HasMySQL u
  => State ShareReq
  -> Flags
  -> u
  -> PerformFetch ShareReq

shareFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch ShareReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mysqlPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch ShareReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: ShareReq a -> TablePrefix -> Connection -> IO a
fetchReq CreateTable                              = createTable
fetchReq (CreateShare un sid)                     = createShare un sid
fetchReq (GetShare sid)                           = getShare sid
fetchReq (GetShareByName un)                      = getShareByName un
fetchReq CountShare                               = countShare
fetchReq (GetShareList f si o)                    = getShareList f si o
fetchReq (CountShareByFather fid)                 = countShareByFather fid
fetchReq (GetShareListByFather fid f si o)        = getShareListByFather fid f si o
fetchReq (IncrShareScore sid sc)                  = incrShareScore sid sc
fetchReq (IncrShareCount sid c)                   = incrShareCount sid c
fetchReq (IncrSharePatchCount sid c)              = incrSharePatchCount sid c
fetchReq (CreateShareHistory sid rid sm sc d)     = createShareHistory sid rid sm sc d
fetchReq (GetShareHistory hid)                    = getShareHistory hid
fetchReq (CountShareHistory sid)                  = countShareHistory sid
fetchReq (GetShareHistoryList sid f si o)         = getShareHistoryList sid f si o
fetchReq (StatisticShareHistory sid st ed)        = statisticShareHistory sid st ed
fetchReq (StatisticShareHistoryList st ed f si o) = statisticShareHistoryList st ed f si o
fetchReq (CountStatisticShareHistory st ed)       = countStatisticShareHistory st ed
fetchReq (GetConfig key)                          = getConfig key
fetchReq (SetConfig key value)                    = setConfig key value

initShareState :: Int -> State ShareReq
initShareState = ShareState
