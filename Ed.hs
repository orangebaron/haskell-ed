import Control.Monad.State

data Mode = NormalMode | AppendMode [String] | GMode {- TODO -}
data EdState = EdState { buffer :: [String], line :: Int, mode :: Mode, file :: String, lastSearch:: String, lastCmd :: String, lastErr :: String, prompt :: String, promptOn :: Bool }

appendCmd :: StateT EdState IO ()
appendCmd = do
    state <- get
    put $ EdState (buffer state) (line state) (AppendMode []) (file state) (lastSearch state) (lastCmd state) (lastErr state) (prompt state) (promptOn state)

insertCmd :: StateT EdState IO ()
insertCmd = do
    state <- get
    put $ EdState (buffer state) (line state - 1) (AppendMode []) (file state) (lastSearch state) (lastCmd state) (lastErr state) (prompt state) (promptOn state)

deleteCmd :: (Int, Int) -> StateT EdState IO ()
deleteCmd (minL, maxL) = do
    state <- get
    put $ EdState [(buffer state) !! i | i <- [0 .. (length $ buffer state) - 1], (i >= minL) && (i <= maxL)] (line state - 1) (mode state) (file state) (lastSearch state) (lastCmd state) (lastErr state) (prompt state) (promptOn state)

changeCmd :: (Int, Int) -> StateT EdState IO ()
changeCmd range = do
    deleteCmd range
    insertCmd

printCmd :: (Int, Int) -> StateT EdState IO ()
printCmd (minL, maxL) = do
    state <- get
    lift $ putStrLn $ foldl (\s -> ((s ++ "\n") ++)) "" $ buffer state

promptCmd :: StateT EdState IO ()
promptCmd = do
    state <- get
    put $ EdState (buffer state) (line state) (mode state) (file state) (lastSearch state) (lastCmd state) (lastErr state) (prompt state) (not $ promptOn state)

main = putStrLn ""
