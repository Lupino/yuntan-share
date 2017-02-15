{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)

import           Haxl.Core                            (StateStore, initEnv,
                                                       runHaxl, stateEmpty,
                                                       stateSet)
import           Share
import           Share.Handler

import qualified Data.Yaml                            as Y
import qualified Share.Config                         as C

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
  (Just conf) <- Y.decodeFile (getConfigFile opts) :: IO (Maybe C.Config)
  let serverHost   = getHost opts
      serverPort   = getPort opts

      mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig

      tablePrefix  = getTablePrefix opts


  mySQLPool <- C.genMySQLPool mysqlConfig

  let state = stateSet (initShareState mysqlThreads) stateEmpty

  let userEnv = UserEnv { mySQLPool = mySQLPool, tablePrefix = tablePrefix }

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
  get  "/api/statistic/"           getStatisticShareHistoryHandler
  get  "/api/shares/:name/"        getShareHandler
  get  "/api/shares/:name/childs/" getShareChildrenHandler
  get  "/api/shares/:name/hists/"  getShareHistoryHandler
  get  "/api/shares/:name/patch/"  getSharePatchHandler
