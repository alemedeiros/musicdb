-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Main file for musicdb -- ECS713 Functional Programming Project

import System.Environment
import System.IO

-- |The main function provides TODO different functionalities:
--
--  [@init@] will initialize the database musisbrainz.db
--
--  [@add artist@] will download the artist information with it's releases and
--  add to the database
main :: IO ()
main = do args <- getArgs
          case args of
                  [ "init" ] -> undefined -- TODO
                  [ "add", art ] -> undefined -- TODO
                  [ "help" ] -> printHelp
                  [ "usage" ] -> printHelp
                  _ -> do
                          printError $ "undifided arguments -- " ++ show args
                          printHelp

printError :: String -> IO ()
printError = hPutStrLn stderr . (++) "musicdb: "

printHelp :: IO ()
printHelp = putStrLn "Usage: MusicDB command [args]\n\
        \\n\
        \init               Initialize the database musisbrainz.db\n\
        \add artist         \n\
        \usage | help       Print this help message\n"
