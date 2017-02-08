module Share.API
  (
    createTable
  , createShare
  , getShare
  , getShareByName
  , countShare
  , getShareList
  , countShareByFather
  , getShareListByFather
  , incrShareScore
  , incrShareCount
  , incrSharePatchCount
  , createShareHistory
  , getShareHistory
  , countShareHistory
  , getShareHistoryList
  , statisticShareHistory
  , statisticShareHistoryList
  , countStatisticShareHistory
  , setConfig
  , getConfig
  , getConfig_

  , fillFather
  ) where

import           Data.Int                  (Int64)
import           Data.Maybe                (catMaybes, fromMaybe)
import           Haxl.Core                 (dataFetch, uncachedRequest)

import           Data.Typeable
import           Dispatch.Types.ListResult (From, Size)
import           Dispatch.Types.OrderBy    (OrderBy)
import           Share.DS
import           Share.Types
import           Share.UserEnv             (ShareM)
import           Text.Read                 (readMaybe)

createTable                :: ShareM Int64
createShare                :: UserName -> ShareID -> ShareM ShareID
getShare                   :: ShareID -> ShareM (Maybe Share)
getShareByName             :: UserName -> ShareM (Maybe Share)
countShare                 :: ShareM Int64
getShareList               :: From -> Size -> OrderBy -> ShareM [Share]
countShareByFather         :: ShareID -> ShareM Int64
getShareListByFather       :: ShareID -> From -> Size -> OrderBy -> ShareM [Share]
incrShareScore             :: ShareID -> Score -> ShareM Int64
incrShareCount             :: ShareID -> Count -> ShareM Int64
incrSharePatchCount        :: ShareID -> Count -> ShareM Int64
createShareHistory         :: ShareID -> ShareID -> Summary -> Score -> Depth -> ShareM HistID
getShareHistory            :: HistID -> ShareM (Maybe ShareHistory)
countShareHistory          :: ShareID -> ShareM Int64
getShareHistoryList        :: ShareID -> From -> Size -> OrderBy -> ShareM [ShareHistory]
statisticShareHistory      :: ShareID -> Int64 -> Int64 -> ShareM PatchResult
statisticShareHistoryList  :: Int64 -> Int64 -> From -> Size -> OrderBy -> ShareM [PatchResult]
countStatisticShareHistory :: Int64 -> Int64 -> ShareM Count

getConfig                  :: Read a => String -> ShareM (Maybe a)
getConfig_                 :: String -> ShareM String
setConfig                  :: String -> String -> ShareM Int64

createTable                            = uncachedRequest CreateTable
createShare un sid                     = uncachedRequest (CreateShare un sid)
getShare sid                           = dataFetch (GetShare sid)
getShareByName un                      = dataFetch (GetShareByName un)
countShare                             = dataFetch CountShare
getShareList f si o                    = dataFetch (GetShareList f si o)
countShareByFather fid                 = dataFetch (CountShareByFather fid)
getShareListByFather fid f si o        = dataFetch (GetShareListByFather fid f si o)
incrShareScore sid sc                  = uncachedRequest (IncrShareScore sid sc)
incrShareCount sid c                   = uncachedRequest (IncrShareCount sid c)
incrSharePatchCount sid c              = uncachedRequest (IncrSharePatchCount sid c)
createShareHistory sid rid sm sc d     = uncachedRequest (CreateShareHistory sid rid sm sc d)
getShareHistory hid                    = dataFetch (GetShareHistory hid)
countShareHistory sid                  = dataFetch (CountShareHistory sid)
getShareHistoryList sid f si o         = dataFetch (GetShareHistoryList sid f si o)
statisticShareHistory sid st ed        = fromMaybe (patchResult sid) <$> dataFetch (StatisticShareHistory sid st ed)
statisticShareHistoryList st ed f si o = dataFetch (StatisticShareHistoryList st ed f si o)
countStatisticShareHistory st ed       = dataFetch (CountStatisticShareHistory st ed)

getConfig key                          = readMaybe <$> getConfig_ key
getConfig_ key                         = dataFetch (GetConfig key)
setConfig key value                    = uncachedRequest (SetConfig key value)

fillFather :: Depth -> Depth -> Maybe Share -> ShareM (Maybe Share)
fillFather _ _ Nothing          = return Nothing
fillFather depth maxDepth (Just share) =
  if depth < maxDepth && fid > 0 then do
    father <- fillFather (depth + 1) maxDepth =<< getShare fid
    return (Just share { getShareFather = father
                       , getShareDepth = depth
                       })
  else return (Just share { getShareDepth = depth })

  where fid = getShareFatherID share
