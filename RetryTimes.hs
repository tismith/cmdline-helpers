-- RetryTimes.hs a basic conman helper rewritten in haskell as an exercise 

module Main where
import System.Exit -- exitSuccess,exitFailure
import System.Posix.Process -- forkProcess
import System.Environment -- getProgName,getArgs
import Control.Monad -- liftM
import Control.Applicative -- <$>

usage :: String -> IO Bool
usage p = do
	putStrLn $ p ++ " forever|[0-9]* command-to-test argument1 argument2 ..."
	return False

data ParsedArgs = Forever | Times Int
parseArgs :: [String] -> Maybe ParsedArgs
parseArgs [] = Nothing
parseArgs (x:[]) = Nothing
parseArgs (x:_)
	| x == "forever" = Just Forever
	| otherwise = case reads x of
		[] -> Nothing
		[(n,_)] -> if n >= 0 then Just (Times n) else Nothing

parseStatus :: Maybe ProcessStatus -> Bool
parseStatus (Just (Exited ExitSuccess)) = True
parseStatus _ = False

forkAndRun :: String -> [String] -> IO Bool
forkAndRun program args = 
	parseStatus <$> (forkProcess (executeFile program True args Nothing) >>= getProcessStatus True False)

retryForever :: (Monad m) => m Bool -> m Bool
retryForever f = do 
       val <- f
       if val then return val else retryForever f

retryTimes :: (Monad m, Ord n, Num n) => n ->  m Bool -> m Bool
retryTimes n f 
       | n <= 0 = return False
       | otherwise = do 
		val <- f
		if val then return val else retryTimes (n-1) f

main :: IO ()
main =	do
	programName <- getProgName
	args <- getArgs
	let parsedArgs = parseArgs args 
	let command = head (tail args)
	let commandArgs = tail (tail args)
	commandSucceeded <- case parsedArgs of
		Just (Times n) -> retryTimes n (forkAndRun command commandArgs)
		Just Forever -> retryForever (forkAndRun command commandArgs)
		_ -> usage programName
	if commandSucceeded then exitSuccess else exitFailure
	

