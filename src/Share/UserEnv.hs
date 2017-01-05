{-# LANGUAGE OverloadedStrings #-}

module Share.UserEnv
  (
    UserEnv(..)
  , ActionM
  , ScottyM
  , ShareM
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Pool              (Pool)
import qualified Data.Text.Lazy         as LT (Text)
import           Database.MySQL.Simple  (Connection)
import           Haxl.Core              (GenHaxl)
import           Haxl.Core.Monad        (unsafeLiftIO)
import           Web.Scotty.Trans       (ActionT, ScottyT)

data UserEnv = UserEnv { mySQLPool   :: Pool Connection
                       , tablePrefix :: String
                       }

type ShareM = GenHaxl UserEnv

instance MonadIO (GenHaxl u) where
  liftIO = unsafeLiftIO

type ActionM a = ActionT LT.Text ShareM a
type ScottyM a = ScottyT LT.Text ShareM a
