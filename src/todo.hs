import System.Environment
import System.IO
import System.Directory
import Control.Exception
import Data.List
import Data.Char

-- Types

type Task = String
type Tag = String
type NumberedTask = (Int, String)
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
    n <- countLines filePath
    putStr $ "TODO: Added '" ++ task ++ "' on line " ++ show n ++ "\n"
add _ = putStrLn "Exactly one argument required with command ADD"

list :: [String] -> IO ()
list (filePath:args) = do
    tasks <- getTasks filePath

    let taggedTasks = map getTags tasks
    let numberedTasks = zip [1..] taggedTasks

    let tags = filter isTag args
    let kwds = filter (not . isTag) args

    let filteredTasks = filter (hasTags tags . snd) numberedTasks
    let filteredTasks' = filter (containsKwds kwds . fst . snd) filteredTasks
    let tasks = map (\(n,(a,_)) -> (n,a)) filteredTasks'

    putStr $ unlines $ withNumbers tasks
    printFileInfo filePath

rm :: [String] -> IO ()
rm [filePath,numberStr] = do
    tasks <- getTasks filePath
    
    let oldTask = tasks !! (read numberStr - 1)
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
rm _ = putStrLn "Exactly one argument required with command REMOVE"

doNotRecognize :: Command -> t -> IO ()
doNotRecognize arg _ = putStrLn ("Did not recognize command: " ++ arg)

-- Helper functions

getFilePath :: IO String
getFilePath = do
    path <- readFile config
    return $ init path

printFileInfo :: FilePath -> IO ()
printFileInfo filePath = do
    tasks <- getTasks filePath
    let len = length tasks
    putStrLn "--"
    putStrLn ("TODO: " ++ show len ++ " tasks in " ++ filePath)

getTasks :: FilePath -> IO [Task]
getTasks path = do
    contents <- readFile path
    return $ lines contents

getTags :: Task -> (Task, [Tag])
getTags s = (s, words rest) where
    (_, rest) = splitOn "+" s

isTag :: String -> Bool
isTag []     = False
isTag (c:cs) = c == '+'

hasTags :: [Tag] -> TaggedTask -> Bool
hasTags []    _        = True
hasTags tags (_,tags') = not . null $ intersect tags tags'

containsKwds :: [String] -> Task -> Bool
containsKwds []   _    = True
containsKwds kwds task = or $ map (`isInfixOf` task_) kwds_ where
    task_ = toLowercase task
    kwds_ = map toLowercase kwds

withNumbers :: [NumberedTask] -> [Task]
withNumbers = map (\(n,line) -> show n ++ " - " ++ line)

-- Utilities

splitOn :: String -> String -> (String, String)
splitOn xs []     = ([], [])
splitOn xs (c:cs) = if c `elem` xs
    then ([], c:cs)
    else let (l,r) = splitOn xs cs in (c:l,r)

toLowercase :: String -> String
toLowercase = map toLower

countLines :: FilePath -> IO Int
countLines path = do
    contents <- readFile path
    return $ length $ lines contents
