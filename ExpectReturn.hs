-- ExpectReturn.hs a basic conman helper rewritten in haskell as an exercise 

module Main where
import System.Exit -- exitSuccess,exitFailure
import System.Posix.Process -- forkProcess
import System.Environment -- getProgName,getArgs
import Control.Monad -- liftM
import Control.Applicative -- <$>

usage :: String -> IO Bool
usage p = do
	putStrLn $ p ++ " any|none|[0-9]*|-[0-9]* command-to-test argument1 argument2 ...\n"
	return False

data ParsedArgs = Any | None | Exactly Int | NotExactly Int
parseArgs :: [String] -> Maybe ParsedArgs
parseArgs [] = Nothing
parseArgs (x:[]) = Nothing
parseArgs (x:_)
	| x == "any" = Just Any
	| x == "none" = Just None
	| otherwise = case reads x of
		[] -> Nothing
		[(n,_)] -> Just (if n >= 0 then Exactly n else NotExactly (-n))

parseStatus :: Maybe ProcessStatus -> Int
parseStatus (Just (Exited ExitSuccess)) = 0
parseStatus (Just (Exited (ExitFailure n))) = n
-- This is unsafe... with Signals being a CInt being an Int32 and as
-- such, wider than haskell's Int class
parseStatus (Just (Terminated s)) = fromIntegral s + 128
parseStatus _ = 1

forkAndRun :: String -> [String] -> IO Int
forkAndRun program args = 
	parseStatus <$> (forkProcess (executeFile program True args Nothing) >>= getProcessStatus True False)

main :: IO ()
main =	do
	programName <- getProgName
	args <- getArgs
	let parsedArgs = parseArgs args 
	let command = head (tail args)
	let commandArgs = tail (tail args)
	let returnCode = forkAndRun command commandArgs
	codeMatched <- case parsedArgs of
		Just Any -> returnCode >> return True
		Just None -> returnCode >> return False
		Just (Exactly n) -> liftM (== n) returnCode
		Just (NotExactly n) -> liftM (/= n) returnCode
		_ -> usage programName
	if codeMatched then exitSuccess else exitFailure
	

