import System.Environment
import System.IO
import System.Directory
import Control.Exception
import Data.List

-- Types

type Task = String
type Tag = String
type TaggedTask = (Task, [Tag])

type Command = String

-- Configuration

config :: FilePath
config = "../data/config.txt"

-- Primary functions

main :: IO ()
main = do
    (command:arglist) <- getArgs
    filePath <- getFilePath
    dispatch command (filePath:arglist)

dispatch :: Command -> [String] -> IO ()
dispatch "add"  = add
dispatch "list" = list
dispatch "ls"   = list
dispatch "rm"   = rm
dispatch arg    = doNotRecognize arg

add :: [String] -> IO ()
add [filePath,task] = do
    appendFile filePath (task ++ "\n")
    tasks <- getTasks filePath
    let n = length tasks
    putStr $ "TODO: Added '" ++ task ++ "' on line " ++ show n ++ "\n"
add _ = putStrLn "Exactly two arguments required with command ADD"

list :: [String] -> IO ()
list (filePath:tags) = do
    tasks <- getTasks filePath
    let taggedTasks = map getTags tasks
    let filteredTasks = map fst $ filter (hasTags tags) taggedTasks
    putStr $ unlines $ numberTasks filteredTasks
    printFileInfo filePath
list _ = putStrLn "Exactly one argument required with command VIEW"

rm :: [String] -> IO ()
rm [filePath,numberStr] = do
    tasks <- getTasks filePath
    let oldTask = tasks !! (read numberStr -1)
    let newTasks = delete oldTask tasks
    bracketOnError (openTempFile "." "temp")
        (\(tempPath, tempHandle) -> do
            hClose tempHandle
            removeFile tempPath)
        (\(tempPath, tempHandle) -> do
            mapM_ (hPutStrLn tempHandle) newTasks
            hClose tempHandle
            removeFile filePath
            renameFile tempPath filePath)
    putStr $ "TODO: Removed '" ++ oldTask ++ "' on line " ++ numberStr ++ "\n"
rm _ = putStrLn "Exactly two arguments required with command REMOVE"

doNotRecognize :: Command -> t -> IO ()
doNotRecognize arg _ = putStrLn ("Did not recognize command: " ++ arg)

-- Helper functions

getFilePath :: IO String
getFilePath = do
    path <- readFile config
    return $ init path

printFileInfo :: String -> IO ()
printFileInfo filePath = do
    tasks <- getTasks filePath
    let len = length tasks
    putStrLn "--"
    putStrLn ("TODO: " ++ show len ++ " tasks in " ++ filePath)

getTasks :: FilePath -> IO [String]
getTasks path = do
    contents <- readFile path
    return (lines contents)

getTags :: Task -> (Task, [Tag])
getTags s = (str, words rest) where
    (str, rest) = splitOn "+@" s

hasTags :: [Tag] -> TaggedTask -> Bool
hasTags []    _        = True
hasTags tags (_,tags') = overlapping tags tags' where
    overlapping xs []     = False
    overlapping xs (y:ys) = if y `elem` xs
        then True
        else overlapping xs ys

numberTasks :: [String] -> [String]
numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [1..]

-- Utilities

splitOn :: String -> String -> (String, String)
splitOn xs []     = ([], [])
splitOn xs (c:cs) = if c `elem` xs
    then ([], c:cs)
    else let (l,r) = splitOn xs cs in (c:l,r)
