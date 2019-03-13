import System.Environment
import System.Exit
import qualified Semver as S
import NanoParsec (runParser)

main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse ("validate":str:[]) = validate str >> exit
parse _ = usage >> exit

exit = exitWith ExitSuccess

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn ("Usage: ")
    putStrLn (" * " ++ name ++ " validate versionString  Validate given version string.")
    putStrLn (" * " ++ name ++ " -h                      Display this help and exit")
    putStrLn (" * " ++ name ++ " -v                      Output version information and exit")

version :: IO ()
version = do
    name <- getProgName
    putStrLn (name ++ " 0.1.0")

validate :: String -> IO ()
validate versionString =
    case runParser S.parseVersionString versionString of
        Left invalidVersion -> putStrLn "Invalid!"
        Right validVersion  -> putStrLn "Valid!"
