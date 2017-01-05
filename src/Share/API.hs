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
  , createShareHistory
  , getShareHistory
  , countShareHistory
  , getShareHistoryList
  , setConfig
  , getConfig

  , fillFather
  ) where

import           Data.Int                     (Int64)
import           Data.Maybe                   (catMaybes)
import           Haxl.Core                    (dataFetch, uncachedRequest)

import           Data.Typeable
import           Database.MySQL.Simple.Result (Result)
import           Dispatch.Types.ListResult    (From, Size)
import           Dispatch.Types.OrderBy       (OrderBy)
import           Share.DS
import           Share.Types
import           Share.UserEnv                (ShareM)

createTable           :: ShareM Int64
createShare          :: UserName -> ShareID -> ShareM ShareID
getShare             :: ShareID -> ShareM (Maybe Share)
getShareByName       :: UserName -> ShareM (Maybe Share)
countShare           :: ShareM Int64
getShareList         :: From -> Size -> OrderBy -> ShareM [Share]
countShareByFather   :: ShareID -> ShareM Int64
getShareListByFather :: ShareID -> From -> Size -> OrderBy -> ShareM [Share]
incrShareScore       :: ShareID -> Score -> ShareM Score
incrShareCount       :: ShareID -> Count -> ShareM Count
createShareHistory   :: ShareID -> ShareID -> Summary -> Score -> Depth -> ShareM HistID
getShareHistory      :: HistID -> ShareM (Maybe ShareHistory)
countShareHistory    :: ShareID -> ShareM Int64
getShareHistoryList  :: ShareID -> From -> Size -> OrderBy -> ShareM [ShareHistory]

getConfig            :: (Result a, Typeable a, Show a) => String -> ShareM (Maybe a)
setConfig            :: String -> String -> ShareM Int64

createTable                        = uncachedRequest CreateTable
createShare un sid                 = uncachedRequest (CreateShare un sid)
getShare sid                       = dataFetch (GetShare sid)
getShareByName un                  = dataFetch (GetShareByName un)
countShare                         = dataFetch CountShare
getShareList f si o                = dataFetch (GetShareList f si o)
countShareByFather fid             = dataFetch (CountShareByFather fid)
getShareListByFather fid f si o    = dataFetch (GetShareListByFather fid f si o)
incrShareScore sid sc              = uncachedRequest (IncrShareScore sid sc)
incrShareCount sid c               = uncachedRequest (IncrShareCount sid c)
createShareHistory sid rid sm sc d = uncachedRequest (CreateShareHistory sid rid sm sc d)
getShareHistory hid                = dataFetch (GetShareHistory hid)
countShareHistory sid              = dataFetch (CountShareHistory sid)
getShareHistoryList sid f si o     = dataFetch (GetShareHistoryList sid f si o)

getConfig key                      = dataFetch (GetConfig key)
setConfig key value                = uncachedRequest (SetConfig key value)

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
