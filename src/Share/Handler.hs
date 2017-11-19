{-# LANGUAGE OverloadedStrings #-}
module Share.Handler
  (
    createShareHandler
  , createShareHistoryHandler
  , saveConfigHandler
  , getConfigHandler
  , getShareHandler
  , getShareChildrenHandler
  , getShareHistoryHandler
  , getSharePatchHandler
  , getShareListHandler
  , getStatisticShareHistoryHandler
  , graphqlHandler
  ) where

import           Control.Monad           (void, when)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (lift)

import           Haxl.Core               (GenHaxl)
import           Share
import           Web.Scotty.Trans        (json, param, rescue)
import           Yuntan.Types.HasMySQL   (HasMySQL)
import           Yuntan.Types.ListResult (From, ListResult (..), Size)
import           Yuntan.Types.OrderBy    (desc)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.Scotty     (errNotFound, maybeNotFound, ok,
                                          okListResult)

import           Data.Int                (Int64)
import           Data.Maybe              (catMaybes, fromMaybe)
import           Data.Traversable        (for)
import           Data.UnixTime

import           Data.GraphQL            (graphql)
import           Share.GraphQL           (schema)


-- POST /api/shares/
createShareHandler :: HasMySQL u => ActionH u ()
createShareHandler = do
  name <- param "name"
  fname <- param "sharename"

  father <- lift $ getShareByName fname
  case father of
    Nothing -> do
      fid <- lift $ createShare fname 0
      createShareHandler' name fid
    Just Share{getShareID = fid} -> createShareHandler' name fid

createShareHandler' :: HasMySQL u => UserName -> ShareID -> ActionH u ()
createShareHandler' name fid = do
  sid <- lift $ createShare name fid
  void $ lift $ incrShareCount fid 1
  json =<< lift (getShare sid)

-- POST /api/shares/:name/hists/
createShareHistoryHandler :: HasMySQL u => ActionH u ()
createShareHistoryHandler = do
  score <- param "score"
  sm <- param "summary"
  share <- paramShare'
  scoreList <- lift . mapM (calcShareScore score) . catMaybes $ getFatherList share
  case share of
    Nothing -> shareNotFound
    Just Share{getShareID = rid} -> do
      lift $ mapM_ (saveHistory rid sm) scoreList
      json scoreList


  where getFatherList :: Maybe Share -> [Maybe Share]
        getFatherList Nothing                                  = []
        getFatherList (Just Share{getShareFather = father}) = father : getFatherList father

        calcShareScore :: HasMySQL u => Score -> Share -> GenHaxl u Share
        calcShareScore ref share = do
          ratio <- fromMaybe 0.0 <$> getConfig ("ratio_" ++ show depth)
          let patchScore = ceiling $ fromIntegral ref * ratio

          return share { getSharePatchScore = patchScore
                       , getShareTotalScore = score + patchScore
                       , getSharePatchCount = patchCount + 1
                       , getShareFather     = Nothing
                       }
          where depth = getShareDepth share
                score = getShareTotalScore share
                patchCount = getSharePatchCount share

        saveHistory :: HasMySQL u => ShareID -> Summary -> Share -> GenHaxl u ()
        saveHistory rid sm share =
          when (score > 0) $ do
            void $ incrShareScore fid score
            void $ incrSharePatchCount fid 1
            void $ createShareHistory fid rid sm score depth

          where fid   = getShareID share
                score = getSharePatchScore share
                depth = getShareDepth share

-- POST /api/config/:key/
saveConfigHandler :: HasMySQL u => ActionH u ()
saveConfigHandler = do
  key <- param "key"
  value <- param "value"
  void $ lift $ setConfig key value
  resultOK

-- GET /api/config/:key/
getConfigHandler :: HasMySQL u => ActionH u ()
getConfigHandler = do
  key <- param "key"
  value <- lift $ getConfig_ key
  ok "value" value

-- GET /api/shares/:name/
getShareHandler :: HasMySQL u => ActionH u ()
getShareHandler = maybeNotFound "Share" =<< paramShare'

-- GET /api/shares/:name/childs/
getShareChildrenHandler :: HasMySQL u => ActionH u ()
getShareChildrenHandler = do
  (from, size) <- paramPage
  share <- paramShare
  case share of
    Nothing -> shareNotFound
    Just Share{getShareID = fid} -> do
      childs <- lift $ getShareListByFather fid from size (desc "id")
      total <- lift $ countShareByFather fid
      okListResult "childs" ListResult { getFrom   = from
                                       , getSize   = size
                                       , getTotal  = total
                                       , getResult = childs
                                       }

-- GET /api/shares/:name/hists/
getShareHistoryHandler :: HasMySQL u => ActionH u ()
getShareHistoryHandler = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  share <- paramShare
  case share of
    Nothing -> shareNotFound
    Just Share{getShareID = fid} -> do
      hists <- lift $ do
        hts <- getShareHistoryList fid from size (desc "id")
        for hts $ \ht -> do
          src <- getShare $ getHistSrcID ht
          return ht { getHistSrc = src }
      total <- lift $ countShareHistory fid
      okListResult "hists" ListResult { getFrom   = from
                                      , getSize   = size
                                      , getTotal  = total
                                      , getResult = hists
                                      }

-- GET /api/shares/:name/patch/
getSharePatchHandler :: HasMySQL u => ActionH u ()
getSharePatchHandler = do
  share <- paramShare
  startTime <- param "start_time" `rescue` (\_ -> return 0) :: ActionH u Int64
  endTime <- param "end_time" `rescue` (\_ -> liftIO $ read . show . toEpochTime <$> getUnixTime) :: ActionH u Int64
  case share of
    Nothing -> shareNotFound
    Just Share{getShareID = fid} -> json =<< lift (statisticShareHistory fid startTime endTime)

-- GET /api/shares/
getShareListHandler :: HasMySQL u => ActionH u ()
getShareListHandler = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  shares <- lift $ catMaybes <$> do
    shs <- getShareList from size (desc "id")
    for shs $ \sh -> do
      maxDepth <- fromMaybe 0 <$> getConfig "max_depth"
      fillFather 0 maxDepth $ Just sh

  total <- lift countShare
  okListResult "shares" ListResult { getFrom   = from
                                   , getSize   = size
                                   , getTotal  = total
                                   , getResult = shares
                                   }

-- GET /api/statistic/
getStatisticShareHistoryHandler :: HasMySQL u => ActionH u ()
getStatisticShareHistoryHandler = do
  (from, size) <- paramPage
  startTime <- param "start_time" `rescue` (\_ -> return 0) :: ActionH u Int64
  endTime <- param "end_time" `rescue` (\_ -> liftIO $ read . show . toEpochTime <$> getUnixTime) :: ActionH u Int64

  patchs <- lift $ do
    shs <- statisticShareHistoryList startTime endTime from size (desc "patch_score")
    for shs $ \sh@PatchResult{getPatchShareID = sid} -> do
      share <- getShare sid
      return sh { getPatchShare = share }


  total <- lift $ countStatisticShareHistory startTime endTime
  okListResult "patchs" ListResult { getFrom   = from
                                   , getSize   = size
                                   , getTotal  = total
                                   , getResult = patchs
                                   }

paramShare :: HasMySQL u => ActionH u (Maybe Share)
paramShare = do
  name <- param "name"
  lift $ getShareByName name

paramShare' :: HasMySQL u => ActionH u (Maybe Share)
paramShare' = do
  name <- param "name"
  maxDepth <- lift $ fromMaybe 0 <$> getConfig "max_depth"
  lift (fillFather 0 maxDepth =<< getShareByName name)

resultOK :: ActionH u ()
resultOK = ok "result" ("OK" :: String)

shareNotFound :: ActionH u ()
shareNotFound = errNotFound "Share not found."

paramPage :: ActionH u (From, Size)
paramPage = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  return (from, size)

graphqlHandler :: HasMySQL u => ActionH u ()
graphqlHandler = do
  query <- param "query"
  json =<< lift (graphql schema query)
