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

import           Control.Monad             (void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (lift)

import           Dispatch.Types.ListResult (From, ListResult (..), Size,
                                            fromListResult)
import           Dispatch.Types.OrderBy    (desc)
import           Dispatch.Types.Result     (err, ok)
import           Dispatch.Utils.Scotty     (maybeNotFound)
import           Network.HTTP.Types        (status404)
import           Share
import           Web.Scotty.Trans          (json, param, rescue, status)

import           Data.Aeson                (object, (.=))
import           Data.Int                  (Int64)
import           Data.Maybe                (catMaybes, fromMaybe)
import           Data.Traversable          (for)
import           Data.UnixTime

import           Data.GraphQL              (graphql)
import           Share.GraphQL             (schema)


-- POST /api/shares/
createShareHandler :: ActionM ()
createShareHandler = do
  name <- param "name"
  fname <- param "sharename"

  father <- lift $ getShareByName fname
  case father of
    Nothing -> do
      fid <- lift $ createShare fname 0
      createShareHandler' name fid
    Just (Share { getShareID = fid }) -> createShareHandler' name fid

createShareHandler' :: UserName -> ShareID -> ActionM ()
createShareHandler' name fid = do
  sid <- lift $ createShare name fid
  void $ lift $ incrShareCount fid 1
  json =<< lift (getShare sid)

-- POST /api/shares/:name/hists/
createShareHistoryHandler :: ActionM ()
createShareHistoryHandler = do
  score <- param "score"
  sm <- param "summary"
  share <- paramShare'
  scoreList <- lift . mapM (calcShareScore score) . catMaybes $ getFatherList share
  case share of
    Nothing -> shareNotFound
    Just (Share { getShareID = rid }) -> do
      lift $ mapM_ (saveHistory rid sm) scoreList
      json scoreList


  where getFatherList :: Maybe Share -> [Maybe Share]
        getFatherList Nothing                                  = []
        getFatherList (Just (Share {getShareFather = father})) = father : getFatherList father

        calcShareScore :: Score -> Share -> ShareM Share
        calcShareScore ref share = do
          ratio <- fromMaybe 0.0 <$> getConfig ("ratio_" ++ show depth) :: ShareM Float
          let patchScore = ceiling $ fromIntegral ref * ratio

          return share { getSharePatchScore = patchScore
                       , getShareTotalScore = score + patchScore
                       , getSharePatchCount = patchCount + 1
                       , getShareFather     = Nothing
                       }
          where depth = getShareDepth share
                score = getShareTotalScore share
                patchCount = getSharePatchCount share

        saveHistory :: ShareID -> Summary -> Share -> ShareM ()
        saveHistory rid sm share = do
          when (score > 0) $ do
            void $ incrShareScore fid score
            void $ incrSharePatchCount fid 1
            void $ createShareHistory fid rid sm score depth

          where fid   = getShareID share
                score = getSharePatchScore share
                depth = getShareDepth share

-- POST /api/config/:key/
saveConfigHandler :: ActionM ()
saveConfigHandler = do
  key <- param "key"
  value <- param "value"
  void $ lift $ setConfig key value
  resultOK

-- GET /api/config/:key/
getConfigHandler :: ActionM ()
getConfigHandler = do
  key <- param "key"
  value <- lift $ getConfig_ key
  json $ object [ "value" .= value ]

-- GET /api/shares/:name/
getShareHandler :: ActionM ()
getShareHandler = do
  maybeNotFound "Share" =<< paramShare'

-- GET /api/shares/:name/childs/
getShareChildrenHandler :: ActionM ()
getShareChildrenHandler = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  share <- paramShare
  case share of
    Nothing -> shareNotFound
    Just (Share {getShareID = fid}) -> do
      childs <- lift $ getShareListByFather fid from size (desc "id")
      total <- lift $ countShareByFather fid
      json . fromListResult "childs" $ ListResult { getFrom   = from
                                                  , getSize   = size
                                                  , getTotal  = total
                                                  , getResult = childs
                                                  }

-- GET /api/shares/:name/hists/
getShareHistoryHandler :: ActionM ()
getShareHistoryHandler = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  share <- paramShare
  case share of
    Nothing -> shareNotFound
    Just (Share {getShareID = fid}) -> do
      hists <- lift $ do
        hts <- getShareHistoryList fid from size (desc "id")
        for hts $ \ht -> do
          src <- getShare $ getHistSrcID ht
          return ht { getHistSrc = src }
      total <- lift $ countShareHistory fid
      json . fromListResult "hists" $ ListResult { getFrom   = from
                                                 , getSize   = size
                                                 , getTotal  = total
                                                 , getResult = hists
                                                 }

-- GET /api/shares/:name/patch/
getSharePatchHandler :: ActionM ()
getSharePatchHandler = do
  share <- paramShare
  startTime <- param "start_time" `rescue` (\_ -> return 0) :: ActionM Int64
  endTime <- param "end_time" `rescue` (\_ -> liftIO $ read . show . toEpochTime <$> getUnixTime) :: ActionM Int64
  case share of
    Nothing -> shareNotFound
    Just (Share {getShareID = fid}) -> json =<< lift (statisticShareHistory fid startTime endTime)

-- GET /api/shares/
getShareListHandler :: ActionM ()
getShareListHandler = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  shares <- lift $ catMaybes <$> do
    shs <- getShareList from size (desc "id")
    for shs $ \sh -> do
      maxDepth <- fromMaybe 0 <$> getConfig "max_depth" :: ShareM Depth
      fillFather 0 maxDepth $ Just sh

  total <- lift countShare
  json . fromListResult "shares" $ ListResult { getFrom   = from
                                              , getSize   = size
                                              , getTotal  = total
                                              , getResult = shares
                                              }

-- GET /api/statistic/
getStatisticShareHistoryHandler :: ActionM ()
getStatisticShareHistoryHandler = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  startTime <- param "start_time" `rescue` (\_ -> return 0) :: ActionM Int64
  endTime <- param "end_time" `rescue` (\_ -> liftIO $ read . show . toEpochTime <$> getUnixTime) :: ActionM Int64

  patchs <- lift $ do
    shs <- statisticShareHistoryList startTime endTime from size (desc "patch_score")
    for shs $ \sh@(PatchResult { getPatchShareID = sid }) -> do
      share <- getShare sid
      return sh { getPatchShare = share }


  total <- lift $ countStatisticShareHistory startTime endTime
  json . fromListResult "patchs" $ ListResult { getFrom   = from
                                              , getSize   = size
                                              , getTotal  = total
                                              , getResult = patchs
                                              }

paramShare :: ActionM (Maybe Share)
paramShare = do
  name <- param "name"
  lift $ getShareByName name

paramShare' :: ActionM (Maybe Share)
paramShare' = do
  name <- param "name"
  maxDepth <- lift $ fromMaybe 0 <$> getConfig "max_depth" :: ActionM Depth
  lift (fillFather 0 maxDepth =<< getShareByName name)

resultOK :: ActionM ()
resultOK = json $ ok "OK"

shareNotFound :: ActionM ()
shareNotFound = status status404 >> json (err "Share not found.")

graphqlHandler :: ActionM ()
graphqlHandler = do
  query <- param "query"
  json =<< lift (graphql schema query)
