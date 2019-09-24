{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Share.GraphQL
  (
    schema
  ) where

import           Control.Applicative   (Alternative (..))
import           Data.GraphQL.AST      (Name)
import           Data.GraphQL.Schema   (Argument (..), Resolver, Schema,
                                        Value (..), arrayA', object', objectA',
                                        scalar, scalarA)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (unpack)
import           Data.UnixTime
import           Haxl.Core             (GenHaxl)
import           Haxl.Core.Monad       (unsafeLiftIO)
import           Share.API
import           Share.Types
import           Yuntan.Types.HasMySQL (HasMySQL)
import           Yuntan.Types.OrderBy  (desc)
import           Yuntan.Utils.GraphQL  (getIntValue)

--  type Query {
--    config(name: String!): String
--    share(name: String!): Share
--    statistic(start_time: Int, end_time: Int, from: Int, size: Int): [PatchResult]
--    statistic_count(start_time: Int, end_time: Int): Int
--    shares(from: Int, size: Int): [Share]
--    share_count: Int
--  }
--  type Share {
--    id: Int
--    name: String
--    father_id: Int
--    father: Share
--    children(from: Int, size: Int): [Share]
--    children_count: Int
--    history(from: Int, size: Int): [ShareHistory]
--    history_count: Int
--    statistic(start_time: Int, end_time: Int): PatchResult
--    total_score: Int
--    count: Int
--    patch_count: Int
--    created_at: Int
--  }
--  type ShareHistory {
--    id: Int
--    share_id: Int
--    share: Share
--    src_id: Int
--    src: Share
--    summary: String
--    score: Int
--    depth: Int
--    created_at: Int
--  }
--  type PatchResult {
--    patch_score: Int
--    patch_count: Int
--    share_id: Int
--    share: Share
--  }

schema :: HasMySQL u => Schema (GenHaxl u w)
schema = config :| [share, statistic, statisticCount, shares, shareCount]

config :: HasMySQL u => Resolver (GenHaxl u w)
config = scalarA "config" $ \case
  (Argument "name" (ValueString name):_) -> getConfig_ $ unpack name
  (Argument "name" (ValueEnum name):_)   -> getConfig_ $ unpack name
  _ -> empty

share :: HasMySQL u => Resolver (GenHaxl u w)
share = objectA' "share" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] share_ <$> getShareByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] share_ <$> getShareByName name
  _ -> empty

share_ :: HasMySQL u => Share -> [Resolver (GenHaxl u w)]
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

father :: HasMySQL u => Name -> ShareID -> Resolver (GenHaxl u w)
father n fid = object' n $ maybe [] share_ <$> getShare fid

children :: HasMySQL u => Name -> ShareID -> Resolver (GenHaxl u w)
children n fid = arrayA' n $ \ argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv

  map share_ <$> getShareListByFather fid from size (desc "id")

childrenCount :: HasMySQL u => Name -> ShareID -> Resolver (GenHaxl u w)
childrenCount n fid = scalarA n $ \case
  [] -> countShareByFather fid
  _ -> empty

history :: HasMySQL u => Name -> ShareID -> Resolver (GenHaxl u w)
history n fid = arrayA' n $ \ argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv

  map record <$> getShareHistoryList fid from size (desc "id")

record :: HasMySQL u => ShareHistory -> [Resolver (GenHaxl u w)]
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

historyCount :: HasMySQL u => Name -> ShareID -> Resolver (GenHaxl u w)
historyCount n fid = scalarA n $ \case
  [] -> countShareHistory fid
  _ -> empty

patch :: HasMySQL u => Name -> ShareID -> Resolver (GenHaxl u w)
patch n fid = objectA' n $ \ argv -> do
  endTime <- flip fromMaybe (getIntValue "end_time" argv) <$> now
  let startTime = fromMaybe 0 $ getIntValue "start_time" argv

  patch_ <$> statisticShareHistory fid startTime endTime

patch_ :: HasMySQL u => PatchResult -> [Resolver (GenHaxl u w)]
patch_ PatchResult {..} = [ scalar "patch_score" getPatchScore
                          , scalar "patch_count" getPatchCount
                          , scalar "share_id"    getPatchShareID
                          , father "share"       getPatchShareID
                          ]

statistic :: HasMySQL u => Resolver (GenHaxl u w)
statistic = arrayA' "statistic" $ \ argv -> do
  endTime <- flip fromMaybe (getIntValue "end_time" argv) <$> now
  let startTime = fromMaybe 0 $ getIntValue "start_time" argv
      from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv

  map patch_ <$> statisticShareHistoryList startTime endTime from size (desc "patch_score")

statisticCount :: HasMySQL u => Resolver (GenHaxl u w)
statisticCount = scalarA "statistic_count" $ \ argv -> do
  endTime <- flip fromMaybe (getIntValue "end_time" argv) <$> now
  let startTime = fromMaybe 0 $ getIntValue "start_time" argv

  countStatisticShareHistory startTime endTime

shares :: HasMySQL u => Resolver (GenHaxl u w)
shares = arrayA' "shares" $ \ argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv
  map share_ <$> getShareList from size (desc "id")

shareCount :: HasMySQL u => Resolver (GenHaxl u w)
shareCount = scalarA "share_count" $ \ case
  [] -> countShare
  _  -> empty

now :: Read a => (GenHaxl u w) a
now = unsafeLiftIO $ read . show . toEpochTime <$> getUnixTime
