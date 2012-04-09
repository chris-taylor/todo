import System.Environment

dispatch :: String -> [String] -> IO ()
dispatch "add" = addItem
dispatch "delete" = deleteItem

addItem :: [String] -> IO ()
addItem = undefined

deleteItem :: [String] -> IO ()
deleteItem = undefined

main :: IO ()
main = do
    (command:arglist) <- getArgs
    dispatch command arglist
