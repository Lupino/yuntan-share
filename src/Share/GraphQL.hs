{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Share.GraphQL
  (
    schema
  ) where

import           Control.Applicative    (Alternative (..))
import qualified Data.Aeson             as A (Value (..))
import           Data.GraphQL.AST       (Name)
import           Data.GraphQL.Schema    (Argument (..), Resolver, Schema (..),
                                         Value (..), arrayA', object', objectA',
                                         scalar, scalarA)
import           Data.Int               (Int32)
import           Data.Text              (unpack)
import           Data.UnixTime
import           Dispatch.Types.OrderBy (desc)
import           Dispatch.Utils.GraphQL (getValue, value, value')
import           Haxl.Core.Monad        (unsafeLiftIO)
import           Share.API
import           Share.Types
import           Share.UserEnv          (ShareM)

schema :: Schema ShareM
schema = Schema [config, share, statistic, statisticCount, shares, shareCount]

config :: Resolver ShareM
config = scalarA "config" $ \case
  (Argument "name" (ValueString name):_) -> getConfig_ $ unpack name
  (Argument "name" (ValueEnum name):_)   -> getConfig_ $ unpack name
  _ -> empty

share :: Resolver ShareM
share = objectA' "share" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] share_ <$> getShareByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] share_ <$> getShareByName name
  _ -> empty

share_ :: Share -> [Resolver ShareM]
share_ Share {..} = [ scalar        "id"             getShareID
                    , scalar        "name"           getShareName
                    , scalar        "father_id"      getShareFatherID
                    , father        "father"         getShareFatherID
                    , children      "children"       getShareID
                    , childrenCount "children_count" getShareID
                    , history       "history"        getShareID
                    , historyCount  "history_count"  getShareID
                    , patch         "statistic"      getShareID
                    , scalar        "total_score"    getShareTotalScore
                    , scalar        "count"          getShareCount
                    , scalar        "patch_count"    getSharePatchCount
                    , scalar        "created_at"     getShareCreatedAt
                    ]

father :: Name -> ShareID -> Resolver ShareM
father n fid = object' n $ maybe [] share_ <$> getShare fid

children :: Name -> ShareID -> Resolver ShareM
children n fid = arrayA' n $ \ argv -> do
  let from = fromIntegral . getInt 0  $ getValue "from" argv
      size = fromIntegral . getInt 10 $ getValue "size" argv

  map share_ <$> getShareListByFather fid from size (desc "id")

childrenCount :: Name -> ShareID -> Resolver ShareM
childrenCount n fid = scalarA n $ \case
  [] -> countShareByFather fid
  _ -> empty

history :: Name -> ShareID -> Resolver ShareM
history n fid = arrayA' n $ \ argv -> do
  let from = fromIntegral . getInt 0  $ getValue "from" argv
      size = fromIntegral . getInt 10 $ getValue "size" argv

  map record <$> getShareHistoryList fid from size (desc "id")

record :: ShareHistory -> [Resolver ShareM]
record ShareHistory {..} = [ scalar "id"         getHistID
                           , scalar "share_id"   getHistSID
                           , father "share"      getHistSID
                           , scalar "src_id"     getHistSrcID
                           , father "src"        getHistSrcID
                           , scalar "summary"    getHistSummary
                           , scalar "score"      getHistScore
                           , scalar "depth"      getHistDepth
                           , scalar "created_at" getHistCreatedAt
                           ]

historyCount :: Name -> ShareID -> Resolver ShareM
historyCount n fid = scalarA n $ \case
  [] -> countShareHistory fid
  _ -> empty

patch :: Name -> ShareID -> Resolver ShareM
patch n fid = objectA' n $ \ argv -> do
  defaultEndTime <- unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
  let startTime = fromIntegral . getInt 0 $ getValue "start_time" argv
      endTime   = fromIntegral . getInt defaultEndTime $ getValue "end_time" argv

  patch_ <$> statisticShareHistory fid startTime endTime

patch_ :: PatchResult -> [Resolver ShareM]
patch_ PatchResult {..} = [ scalar "patch_score" getPatchScore
                          , scalar "patch_count" getPatchCount
                          , scalar "share_id"    getPatchShareID
                          , father "share"       getPatchShareID
                          ]

statistic :: Resolver ShareM
statistic = arrayA' "statistic" $ \ argv -> do
  defaultEndTime <- unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
  let startTime = fromIntegral . getInt 0 $ getValue "start_time" argv
      endTime   = fromIntegral . getInt defaultEndTime $ getValue "end_time" argv
      from      = fromIntegral . getInt 0 $ getValue "from" argv
      size      = fromIntegral . getInt 10 $ getValue "size" argv

  map patch_ <$> statisticShareHistoryList startTime endTime from size (desc "patch_score")

statisticCount :: Resolver ShareM
statisticCount = scalarA "statistic_count" $ \ argv -> do
  defaultEndTime <- unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
  let startTime = fromIntegral . getInt 0 $ getValue "start_time" argv
      endTime   = fromIntegral . getInt defaultEndTime $ getValue "end_time" argv

  countStatisticShareHistory startTime endTime

shares :: Resolver ShareM
shares = arrayA' "shares" $ \ argv -> do
  let from = fromIntegral . getInt 0  $ getValue "from" argv
      size = fromIntegral . getInt 10 $ getValue "size" argv
  map share_ <$> getShareList from size (desc "id")

shareCount :: Resolver ShareM
shareCount = scalarA "share_count" $ \ case
  [] -> countShare
  _  -> empty

getInt :: Int32 -> Maybe Value -> Int32
getInt v Nothing             = v
getInt _ (Just (ValueInt v)) = v
getInt v _                   = v
