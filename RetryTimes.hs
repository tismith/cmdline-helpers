-- RetryTimes.hs a basic conman helper rewritten in haskell as an exercise 

module Main where
import System.Exit -- exitSuccess,exitFailure
import System.Posix.Process -- forkProcess
import System.Environment -- getProgName,getArgs

usage :: String -> IO Bool
usage p = do
	putStrLn $ p ++ " forever|[0-9]* command-to-test argument1 argument2 ..."
	return False

parseArgs :: [String] -> Maybe (Maybe Int)
parseArgs [] = Nothing
parseArgs (x:[]) = Nothing
parseArgs (x:_)
	| x == "forever" = Just Nothing
	| otherwise = case (reads x) of
		[] -> Nothing
		[(n,_)] -> if (n >= 0) then Just (Just n) else Nothing

parseStatus :: (Maybe ProcessStatus) -> Bool
parseStatus (Just (Exited ExitSuccess)) = True
parseStatus _ = False

forkAndRun :: String -> [String] -> IO Bool
forkAndRun program args = do
	putStrLn program
	childPid <- forkProcess (executeFile program True args Nothing) 
	status <- getProcessStatus True False childPid
	return $ parseStatus status

retryForever :: IO Bool -> IO Bool
retryForever f = do 
	val <- f
	if val then return True else retryForever f

retryTimes :: Int -> IO Bool -> IO Bool
retryTimes n f 
	| n <= 0 = do return False
	| otherwise = do 
		val <- f
		if val then return True else (retryTimes (n-1) f)

main :: IO ()
main =	do
	programName <- getProgName
	args <- getArgs
	let parsedArgs = parseArgs args 
	let command = head (tail args)
	let commandArgs = tail (tail args)
	commandSucceeded <- case parsedArgs of
		Just (Just n) -> retryTimes n (forkAndRun command commandArgs)
		Just Nothing -> retryForever (forkAndRun command commandArgs)
		_ -> usage programName
	if commandSucceeded then exitSuccess else exitFailure
	

