
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Shell
import           System.Directory
import Control.Monad.IO.Class

import Data.Configurator as Config
import Data.Text as T

-- {-# LANGUAGE OverloadedStrings #-}

getSqlServerCommand :: FilePath -> IO String
getSqlServerCommand config_filename = do
  config  <- Config.load [ Config.Required config_filename ]    
  hostname <- Config.require config "database.hostname"
  username <- Config.require config "database.username"
  database <- Config.require config "database.database"
  return $ mconcat ["psql -h " , hostname ,  " -d " , database , " -U " , username 
    , " -t -A -F\",\" -c "]


psqlRunCommand :: String -> FilePath -> IO String
psqlRunCommand query configFile = do 
  cmd <- (getSqlServerCommand configFile)
  return $ mconcat [ cmd, "\" ", query , "\"" ]

mkXargs :: String -> String
mkXargs inputStr = mconcat [ "xargs -I{} ", inputStr]


db_config = "/home/adamwespiser/elsen/platform/prod.cfg"

main =  do
  exists <- liftIO $ doesFileExist db_config
  if (exists)
  then do
    psqlCmd <- psqlRunCommand "select client_hostname,query from pg_stat_activity "   db_config
    putStrLn psqlCmd
    col <- run ( shell (psqlCmd)   $| shell (mkXargs "echo {}")
         -- $| shell "cut -f 1,2,3,4,5,6 -d, "  
         $| conduit (CL.map (\x -> mconcat [" > " ,x]))
    
        )
    putStrLn $ show col
   else  putStrLn $ show "file not found:"
