{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (get, middleware, post,
                                                       scottyOptsT, settings)
import           Yuntan.Types.HasMySQL                (HasMySQL, simpleEnv)
import           Yuntan.Types.Scotty                  (ScottyH)

import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)
import           Share
import           Share.Handler

import qualified Data.Yaml                            as Y
import qualified Share.Config                         as C

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options { getConfigFile  :: String
                       , getHost        :: String
                       , getPort        :: Int
                       , getTablePrefix :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "Share micro server config file."
                                <> value "config.yaml")
                 <*> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Share micro server hostname."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT"
                                <> help "Share micro server port."
                                <> value 3000)
                 <*> strOption (long "table_prefix"
                                <> metavar "TABLE_PREFIX"
                                <> help "table prefix."
                                <> value "test")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Share micro server"
     <> header "yuntan-user - Share micro server" )

program :: Options -> IO ()
program Options { getConfigFile  = confFile
                , getTablePrefix = prefix
                , getHost        = host
                , getPort        = port
                } = do
  (Right conf) <- Y.decodeFileEither confFile
  let mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig


  pool <- C.genMySQLPool mysqlConfig

  let state = stateSet (initShareState mysqlThreads) stateEmpty

  let u = simpleEnv pool prefix

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  _ <- runIO u state createTable
  scottyOptsT opts (runIO u state) application
  where
        runIO :: HasMySQL u => u -> StateStore -> GenHaxl u b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: HasMySQL u => ScottyH u ()
application = do
  middleware logStdout

  post "/api/shares/"              createShareHandler
  post "/api/shares/:name/hists/"  createShareHistoryHandler
  post "/api/config/:key/"         saveConfigHandler
  get  "/api/config/:key/"         getConfigHandler
  get  "/api/shares/"              getShareListHandler
  get  "/api/statistic/"           getStatisticShareHistoryHandler
  get  "/api/shares/:name/"        getShareHandler
  get  "/api/shares/:name/childs/" getShareChildrenHandler
  get  "/api/shares/:name/hists/"  getShareHistoryHandler
  get  "/api/shares/:name/patch/"  getSharePatchHandler

  get  "/api/graphql/" graphqlHandler
  post "/api/graphql/" graphqlHandler
