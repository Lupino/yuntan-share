{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Pool                            (createPool)
import           Database.MySQL.Simple                (ConnectInfo (..), close,
                                                       connect,
                                                       defaultConnectInfo)

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)

import           Control.Monad                        (when)
import           Haxl.Core                            (StateStore, initEnv,
                                                       runHaxl, stateEmpty,
                                                       stateSet)
import           Share
import           Share.Handler

import           Data.Text                            (pack)
import           Data.Yaml.Config                     as Y (load, lookupDefault,
                                                            subconfig)
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
     <> header "dispatch-user - Share micro server" )

program :: Options -> IO ()
program opts = do
  yamlConfig <- load $ getConfigFile opts
  mysqlConfig <- subconfig "mysql" yamlConfig
  let serverHost   = getHost opts
      serverPort   = getPort opts
      dbName       = Y.lookupDefault "db" "dispatch_user" mysqlConfig
      dbHost       = Y.lookupDefault "host" "127.0.0.1" mysqlConfig
      dbPort       = Y.lookupDefault "port" 3306 mysqlConfig
      dbUser       = Y.lookupDefault "user" "root" mysqlConfig
      dbPass       = Y.lookupDefault "pass" "" mysqlConfig
      numStripes   = Y.lookupDefault "numStripes" 1 mysqlConfig
      idleTime     = Y.lookupDefault "idleTime" 0.5 mysqlConfig
      maxResources = Y.lookupDefault "maxResources" 1 mysqlConfig
      numThreads   = Y.lookupDefault "numThreads" 1 mysqlConfig
      tablePrefix  = getTablePrefix opts

  let conn = connect defaultConnectInfo { connectDatabase = dbName
                                        , connectHost = dbHost
                                        , connectPort = dbPort
                                        , connectUser = dbUser
                                        , connectPassword = dbPass
                                        }

  pool <- createPool conn close numStripes idleTime maxResources

  let state = stateSet (initShareState numThreads) stateEmpty

  let userEnv = UserEnv { mySQLPool = pool, tablePrefix = tablePrefix }

  let opts = def { settings = setPort serverPort
                            $ setHost (Host serverHost) (settings def) }

  _ <- runIO userEnv state createTable
  scottyOptsT opts (runIO userEnv state) application
  where
        runIO :: UserEnv -> StateStore -> ShareM b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: ScottyM ()
application = do
  middleware logStdout

  post "/api/shares/"              createShareHandler
  post "/api/shares/:name/hists/"  createShareHistoryHandler
  post "/api/config/:key/"         saveConfigHandler
  get  "/api/config/:key/"         getConfigHandler
  get  "/api/shares/"              getShareListHandler
  get  "/api/shares/statistic/"    getStatisticShareHistoryHandler
  get  "/api/shares/:name/"        getShareHandler
  get  "/api/shares/:name/childs/" getShareChildrenHandler
  get  "/api/shares/:name/hists/"  getShareHistoryHandler
  get  "/api/shares/:name/patch/"  getSharePatchHandler
