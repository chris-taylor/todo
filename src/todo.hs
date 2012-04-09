import System.Environment
import System.IO
import System.Directory
import Data.List

filePath :: FilePath
filePath = "../data/todo.txt"

getTasks :: FilePath -> IO [String]
getTasks path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    return (lines contents)

numberTasks :: [String] -> [String]
numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..]

dispatch :: String -> [String] -> IO ()
dispatch "add" = addTask
dispatch "show" = showTask
dispatch "delete" = deleteTask

addTask :: [String] -> IO ()
addTask tasks = do
    handle <- openFile filePath AppendMode
    mapM_ (hPutStrLn handle) tasks

showTask :: [String] -> IO ()
showTask _ = do
    tasks <- getTasks filePath
    mapM_ putStrLn (numberTasks tasks)

deleteTask :: [String] -> IO ()
deleteTask _ = do
    tasks <- getTasks filePath
    mapM_ putStrLn (numberTasks tasks)
    putStrLn "Which one would you like to delete?"
    numberStr <- getLine
    let newTasks = delete (tasks !! read numberStr) tasks
    (tempPath, tempHandle) <- openTempFile "." "temp"
    mapM_ (hPutStrLn tempHandle) newTasks
    hClose tempHandle
    removeFile filePath
    renameFile tempPath filePath

main :: IO ()
main = do
    (command:arglist) <- getArgs
    dispatch command arglist
