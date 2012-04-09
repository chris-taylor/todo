import System.Environment
import System.IO
import System.Directory
import Control.Exception
import Data.List

getTasks :: FilePath -> IO [String]
getTasks path = do
    contents <- readFile path
    return (lines contents)

numberTasks :: [String] -> [String]
numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..]

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch arg      = doNotRecognize arg

doNotRecognize :: String -> t -> IO ()
doNotRecognize arg _ = putStrLn ("Did not recognize command: " ++ arg)

add :: [String] -> IO ()
add (filePath:tasks) = appendFile filePath (unlines tasks)

view :: [String] -> IO ()
view [filePath] = do
    tasks <- getTasks filePath
    mapM_ putStrLn (numberTasks tasks)
view _ = putStrLn "Exactly one argument required with command SHOW"

remove :: [String] -> IO ()
remove [filePath,numberStr] = do
    tasks <- getTasks filePath
    let newTasks = delete (tasks !! read numberStr) tasks
    bracketOnError (openTempFile "." "temp")
        (\(tempPath, tempHandle) -> do
            hClose tempHandle
            removeFile tempPath)
        (\(tempPath, tempHandle) -> do
            mapM_ (hPutStrLn tempHandle) newTasks
            hClose tempHandle
            removeFile filePath
            renameFile tempPath filePath)
remove _ = putStrLn "Exactly two argument required with command DELETE"

main :: IO ()
main = do
    (command:arglist) <- getArgs
    dispatch command arglist
