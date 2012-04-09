import System.Environment
import System.IO
import System.Directory
import Data.List

filePath :: FilePath
filePath = "../data/todo.txt"

dispatch :: String -> [String] -> IO ()
dispatch "add" = addItem
dispatch "delete" = deleteItem

addItem :: [String] -> IO ()
addItem tasks = do
    handle <- openFile filePath AppendMode
    mapM_ (hPutStrLn handle) tasks

deleteItem :: [String] -> IO ()
deleteItem _ = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    
    let tasks = lines contents
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] tasks
    
    putStrLn "Your tasks are:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one would you like to delete?"
    
    numberStr <- getLine
    let newTasks = delete (tasks !! read numberStr) tasks
    
    (tempPath, tempHandle) <- openTempFile "." "temp"
    mapM_ (hPutStrLn tempHandle) newTasks
    hClose tempHandle

    removeFile filePath
    renameFile tempPath filePath

    return ()

main :: IO ()
main = do
    (command:arglist) <- getArgs
    dispatch command arglist
