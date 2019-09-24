module Share.API
  (
    createTable
  , createShare
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

import           Data.Int                (Int64)
import           Data.Maybe              (fromMaybe)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)
import           Yuntan.Types.HasMySQL   (HasMySQL)

import           Share.DS
import           Share.Types
import           Text.Read               (readMaybe)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createTable                :: HasMySQL u => GenHaxl u w Int64
createShare                :: HasMySQL u => UserName -> ShareID -> GenHaxl u w ShareID
updateShare                :: HasMySQL u => ShareID -> ShareID -> GenHaxl u w ()
getShare                   :: HasMySQL u => ShareID -> GenHaxl u w (Maybe Share)
getShareByName             :: HasMySQL u => UserName -> GenHaxl u w (Maybe Share)
countShare                 :: HasMySQL u => GenHaxl u w Int64
getShareList               :: HasMySQL u => From -> Size -> OrderBy -> GenHaxl u w [Share]
countShareByFather         :: HasMySQL u => ShareID -> GenHaxl u w Int64
getShareListByFather       :: HasMySQL u => ShareID -> From -> Size -> OrderBy -> GenHaxl u w [Share]
incrShareScore             :: HasMySQL u => ShareID -> Score -> GenHaxl u w Int64
incrShareCount             :: HasMySQL u => ShareID -> Count -> GenHaxl u w Int64
incrSharePatchCount        :: HasMySQL u => ShareID -> Count -> GenHaxl u w Int64
createShareHistory         :: HasMySQL u => ShareID -> ShareID -> Summary -> Score -> Depth -> GenHaxl u w HistID
getShareHistory            :: HasMySQL u => HistID -> GenHaxl u w (Maybe ShareHistory)
countShareHistory          :: HasMySQL u => ShareID -> GenHaxl u w Int64
getShareHistoryList        :: HasMySQL u => ShareID -> From -> Size -> OrderBy -> GenHaxl u w [ShareHistory]
statisticShareHistory      :: HasMySQL u => ShareID -> Int64 -> Int64 -> GenHaxl u w PatchResult
statisticShareHistoryList  :: HasMySQL u => Int64 -> Int64 -> From -> Size -> OrderBy -> GenHaxl u w [PatchResult]
countStatisticShareHistory :: HasMySQL u => Int64 -> Int64 -> GenHaxl u w Count

getConfig                  :: (HasMySQL u, Read a) => String -> GenHaxl u w (Maybe a)
getConfig_                 :: HasMySQL u => String -> GenHaxl u w String
setConfig                  :: HasMySQL u => String -> String -> GenHaxl u w Int64

createTable                            = uncachedRequest CreateTable
createShare un sid                     = uncachedRequest (CreateShare un sid)
updateShare sid fid                    = uncachedRequest (UpdateShare sid fid)
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

fillFather :: HasMySQL u => Depth -> Depth -> Maybe Share -> GenHaxl u w (Maybe Share)
fillFather _ _ Nothing          = return Nothing
fillFather depth maxDepth (Just share) =
  if depth < maxDepth && fid > 0 then do
    father <- fillFather (depth + 1) maxDepth =<< getShare fid
    return (Just share { getShareFather = father
                       , getShareDepth = depth
                       })
  else return (Just share { getShareDepth = depth })

  where fid = getShareFatherID share
