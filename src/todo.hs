import System.Environment
import System.IO

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
deleteItem = undefined

main :: IO ()
main = do
    (command:arglist) <- getArgs
    dispatch command arglist
