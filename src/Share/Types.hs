{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Share.Types
  (
    ShareID
  , HistID
  , UserName
  , Depth
  , Score
  , Count
  , Summary
  , CreatedAt
  , TablePrefix
  , Share (..)
  , ShareHistory (..)
  , PatchResult (..)
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Data.Aeson                         (ToJSON (..), object, (.=))
import           Data.Hashable                      (Hashable (..))
import           Data.Int                           (Int64)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

type ShareID     = Int64
type HistID      = Int64
type UserName    = Text
type Depth       = Int64
type Score       = Int64
type Count       = Int64
type Summary     = Text
type CreatedAt   = Int64
type TablePrefix = String

data Share = Share { getShareID         :: ShareID
                   , getShareName       :: UserName
                   , getShareFatherID   :: ShareID
                   , getShareFather     :: Maybe Share
                   , getShareDepth      :: Depth
                   , getShareTotalScore :: Score
                   , getSharePatchScore :: Score
                   , getShareCount      :: Count
                   , getSharePatchCount :: Count
                   , getShareCreatedAt  :: CreatedAt
                   }

  deriving (Generic, Eq, Show)

instance Hashable Share

data ShareHistory = ShareHistory { getHistID        :: HistID
                                 , getHistSID       :: ShareID
                                 , getHistShare     :: Maybe Share
                                 , getHistSrcID     :: ShareID
                                 , getHistSrc       :: Maybe Share
                                 , getHistSummary   :: Summary
                                 , getHistScore     :: Score
                                 , getHistDepth     :: Depth
                                 , getHistCreatedAt :: CreatedAt
                                 }

  deriving (Generic, Eq, Show)

instance Hashable ShareHistory

instance QueryResults Share where
  convertResults [fa, fb, fc, fd, fe, ff, fg]
                 [va, vb, vc, vd, ve, vf, vg] = Share{..}
    where !getShareID          = convert fa va
          !getShareName        = convert fb vb
          !getShareFatherID    = convert fc vc
          !getShareFather      = Nothing
          !getShareDepth       = 0
          !getShareTotalScore  = convert fd vd
          !getSharePatchScore  = 0
          !getShareCount       = convert fe ve
          !getSharePatchCount  = convert ff vf
          !getShareCreatedAt   = convert fg vg
  convertResults fs vs  = convertError fs vs 2

instance QueryResults ShareHistory where
  convertResults [fa, fb, fc, fd, fe, ff, fg]
                 [va, vb, vc, vd, ve, vf, vg] = ShareHistory{..}
    where !getHistID        = convert fa va
          !getHistSID       = convert fb vb
          !getHistShare     = Nothing
          !getHistSrcID     = convert fc vc
          !getHistSrc       = Nothing
          !getHistSummary   = convert fd vd
          !getHistScore     = convert fe ve
          !getHistDepth     = convert ff vf
          !getHistCreatedAt = convert fg vg
  convertResults fs vs  = convertError fs vs 2


instance ToJSON Share where
  toJSON Share{..} = object [ "id"           .= getShareID
                            , "name"         .= getShareName
                            , "father_id"    .= getShareFatherID
                            , "father"       .= getShareFather
                            , "total_score"  .= getShareTotalScore
                            , "patch_score"  .= getSharePatchScore
                            , "count"        .= getShareCount
                            , "patch_count"  .= getSharePatchCount
                            , "created_at"   .= getShareCreatedAt
                            ]

instance ToJSON ShareHistory where
  toJSON ShareHistory{..} = object [ "id"         .= getHistID
                                   , "share_id"   .= getHistSID
                                   , "share"      .= getHistShare
                                   , "src_id"     .= getHistSrcID
                                   , "src"        .= getHistSrc
                                   , "summary"    .= getHistSummary
                                   , "score"      .= getHistScore
                                   , "depth"      .= getHistDepth
                                   , "created_at" .= getHistCreatedAt
                                   ]

data PatchResult = PatchResult { getPatchScore   :: Score
                               , getPatchCount   :: Count
                               , getPatchShareID :: ShareID
                               , getPatchShare   :: Maybe Share
                               }
  deriving (Generic, Eq, Show)

instance Hashable PatchResult

instance ToJSON PatchResult where
  toJSON PatchResult{..} = object [ "patch_score" .= getPatchScore
                                  , "patch_count"  .= getPatchCount
                                  , "share_id"    .= getPatchShareID
                                  , "share"       .= getPatchShare
                                  ]
