{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Share.DS
  (
    ShareReq(..)
  , initShareState
  ) where

import           Data.Hashable             (Hashable (..))
import           Data.Typeable             (Typeable)
import           Haxl.Core                 (BlockedFetch (..), DataSource,
                                            DataSourceName, Flags,
                                            PerformFetch (..), Show1, State,
                                            StateKey, StateStore,
                                            dataSourceName, fetch, putFailure,
                                            putSuccess, show1, stateEmpty,
                                            stateSet)

import           Dispatch.Types.ListResult (From, Size)
import           Dispatch.Types.OrderBy    (OrderBy)
import           Share.DS.Config
import           Share.DS.Share
import           Share.DS.ShareHistory
import           Share.DS.Table
import           Share.Types
import           Share.UserEnv             (UserEnv (..))

import qualified Control.Exception         (SomeException, bracket_, try)
import           Data.Int                  (Int64)
import           Data.Pool                 (Pool, withResource)
import           Database.MySQL.Simple     (Connection)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import           Data.Maybe                (fromJust, isJust)

-- Data source implementation.

data ShareReq a where
  CreateTable          :: ShareReq Int64
  CreateShare          :: UserName -> ShareID -> ShareReq ShareID
  GetShare             :: ShareID -> ShareReq (Maybe Share)
  GetShareByName       :: UserName -> ShareReq (Maybe Share)
  CountShare           :: ShareReq Int64
  GetShareList         :: From -> Size -> OrderBy -> ShareReq [Share]
  CountShareByFather   :: ShareID -> ShareReq Int64
  GetShareListByFather :: ShareID -> From -> Size -> OrderBy -> ShareReq [Share]
  IncrShareScore       :: ShareID -> Score -> ShareReq Score
  IncrShareCount       :: ShareID -> Count -> ShareReq Count
  CreateShareHistory   :: ShareID -> ShareID -> Summary -> Score -> Depth -> ShareReq HistID
  GetShareHistory      :: HistID -> ShareReq (Maybe ShareHistory)
  CountShareHistory    :: ShareID -> ShareReq Int64
  GetShareHistoryList  :: ShareID -> From -> Size -> OrderBy -> ShareReq [ShareHistory]

  GetConfig            :: String -> ShareReq String
  SetConfig            :: String -> String -> ShareReq Int64

  deriving (Typeable)

deriving instance Eq (ShareReq a)
instance Hashable (ShareReq a) where
  hashWithSalt s CreateTable                          = hashWithSalt s ( 0::Int)
  hashWithSalt s (CreateShare un sid)                 = hashWithSalt s ( 1::Int, un, sid)
  hashWithSalt s (GetShare sid)                       = hashWithSalt s ( 2::Int, sid)
  hashWithSalt s (GetShareByName un)                  = hashWithSalt s ( 3::Int, un)
  hashWithSalt s CountShare                           = hashWithSalt s ( 4::Int)
  hashWithSalt s (GetShareList f si o)                = hashWithSalt s ( 5::Int, f, si, o)
  hashWithSalt s (CountShareByFather fid)             = hashWithSalt s ( 6::Int, fid)
  hashWithSalt s (GetShareListByFather fid f si o)    = hashWithSalt s ( 7::Int, fid, f, si, o)
  hashWithSalt s (IncrShareScore sid sc)              = hashWithSalt s ( 8::Int, sid, sc)
  hashWithSalt s (IncrShareCount sid c)               = hashWithSalt s ( 9::Int, sid, c)
  hashWithSalt s (CreateShareHistory sid rid sm sc d) = hashWithSalt s (10::Int, sid, rid, sm, sc, d)
  hashWithSalt s (GetShareHistory hid)                = hashWithSalt s (11::Int, hid)
  hashWithSalt s (CountShareHistory sid)              = hashWithSalt s (12::Int, sid)
  hashWithSalt s (GetShareHistoryList sid f si o)     = hashWithSalt s (13::Int, sid, f, si, o)
  hashWithSalt s (GetConfig key)                      = hashWithSalt s (14::Int, key)
  hashWithSalt s (SetConfig key value)                = hashWithSalt s (15::Int, key, value)

deriving instance Show (ShareReq a)
instance Show1 ShareReq where show1 = show

instance StateKey ShareReq where
  data State ShareReq = ShareState { numThreads :: Int }

instance DataSourceName ShareReq where
  dataSourceName _ = "ShareDataSource"

instance DataSource UserEnv ShareReq where
  fetch = shareFetch

shareFetch
  :: State ShareReq
  -> Flags
  -> UserEnv
  -> [BlockedFetch ShareReq]
  -> PerformFetch

shareFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> UserEnv -> BlockedFetch ShareReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mySQLPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch ShareReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: ShareReq a -> TablePrefix -> Connection -> IO a
fetchReq CreateTable                          = createTable
fetchReq (CreateShare un sid)                 = createShare un sid
fetchReq (GetShare sid)                       = getShare sid
fetchReq (GetShareByName un)                  = getShareByName un
fetchReq CountShare                           = countShare
fetchReq (GetShareList f si o)                = getShareList f si o
fetchReq (CountShareByFather fid)             = countShareByFather fid
fetchReq (GetShareListByFather fid f si o)    = getShareListByFather fid f si o
fetchReq (IncrShareScore sid sc)              = incrShareScore sid sc
fetchReq (IncrShareCount sid c)               = incrShareCount sid c
fetchReq (CreateShareHistory sid rid sm sc d) = createShareHistory sid rid sm sc d
fetchReq (GetShareHistory hid)                = getShareHistory hid
fetchReq (CountShareHistory sid)              = countShareHistory sid
fetchReq (GetShareHistoryList sid f si o)     = getShareHistoryList sid f si o
fetchReq (GetConfig key)                      = getConfig key
fetchReq (SetConfig key value)                = setConfig key value

initShareState :: Int -> State ShareReq
initShareState threads = ShareState threads
