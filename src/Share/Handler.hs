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
  , getShareListHandler
  ) where

import           Control.Monad             (void, when)
import           Control.Monad.Reader      (lift)

import           Dispatch.Types.ListResult (From, ListResult (..), Size,
                                            fromListResult)
import           Dispatch.Types.OrderBy    (desc)
import           Dispatch.Types.Result     (err, ok)
import           Dispatch.Utils.JSON       (differenceValue, unionValue)
import           Dispatch.Utils.Scotty     (maybeNotFound)
import           Network.HTTP.Types        (status400, status404)
import           Share
import           Web.Scotty.Trans          (body, json, param, rescue, status,
                                            text)

import           Data.Aeson                (ToJSON, Value (..), decode, object,
                                            (.=))
import qualified Data.ByteString.Lazy      as LB (empty)
import           Data.Maybe                (catMaybes, fromMaybe)
import           Data.Text                 (Text, pack, unpack)
import qualified Data.Text.Lazy            as LT (Text)
import           Data.Traversable          (for)

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
        calcShareScore ref share@(Share { getShareDepth = depth }) = do
          ratio <- fromMaybe 0.0 <$> getConfig ("ratio_" ++ show depth) :: ShareM Float
          return share { getSharePatchScore = ceiling $ fromIntegral ref * ratio }

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
