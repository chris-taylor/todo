import System.Environment
import System.IO
import System.Directory
import Control.Exception
import Data.List

getTasks :: FilePath -> IO [String]
getTasks path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    return (lines contents)

numberTasks :: [String] -> [String]
numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..]

dispatch :: String -> [String] -> IO ()
dispatch "add"    = addTask
dispatch "show"   = showTask
dispatch "delete" = deleteTask
dispatch arg      = doNotRecognize arg

doNotRecognize :: String -> t -> IO ()
doNotRecognize arg _ = putStrLn ("Did not recognize command: " ++ arg)

addTask :: [String] -> IO ()
addTask (filePath:tasks) = appendFile filePath (unlines tasks)

showTask :: [String] -> IO ()
showTask [filePath] = do
    tasks <- getTasks filePath
    mapM_ putStrLn (numberTasks tasks)
showTask _ = putStrLn "Exactly one argument required with command SHOW"

deleteTask :: [String] -> IO ()
deleteTask [filePath] = do
    tasks <- getTasks filePath
    mapM_ putStrLn (numberTasks tasks)
    putStrLn "Which one would you like to delete?"
    numberStr <- getLine
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
deleteTask _ = putStrLn "Exactly one argument required with command DELETE"

main :: IO ()
main = do
    (command:arglist) <- getArgs
    dispatch command arglist
