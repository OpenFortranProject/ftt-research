module Main where

import System.Console.GetOpt
import System.Environment ( getArgs )

import Control.Monad ( when, liftM )
import Control.Monad.Trans.RWS.Strict
import Control.Monad.IO.Class
import Control.Applicative

import Data.List ( foldl', isSuffixOf )
import Data.Maybe ( catMaybes )

-- Project specific imports
import ATerm.AbstractSyntax ( ATermTable )
import ATerm.Utilities hiding (foldl', mapM_, mapM, map)
import qualified ATerm.Utilities as U

---------------------------------------------------------------------
-- Command Line options
---------------------------------------------------------------------
data Options = Options
  { optSource :: Maybe FilePath -- ^ Nothing means read from stdin
                                --   otherwise read this file
  }
  deriving (Read, Show, Eq)

options :: [ OptDescr (Options -> Options) ]
options =
  [ Option "s" ["source"]
      (ReqArg (\arg opt -> opt { optSource = Just arg }) "FILE")
      "ATerm file to check"
  ]

defaultOptions :: Options
defaultOptions = Options { optSource = Nothing }

header :: String
header = unlines
  [ "usage: [-s FILE]"
  , "  If no file is given, input is taken from stdin"
  ]

---------------------------------------------------------------------
-- Main program
---------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let (flags, _, errors) = getOpt RequireOrder options args
      opts = foldl' (.) id flags defaultOptions
  when (not (null errors)) $ error (concat errors ++ usageInfo header options)
  cs <- removeTrailingDot <$> maybe getContents readFile (optSource opts)
  let aterms = U.readATerm cs
  warnings <- execDeclWarn aterms
  mapM_ putStrLn warnings
  where
  removeTrailingDot s
    |  length s >= 2 && ".\n" `isSuffixOf` s = init (init s)
    | otherwise = s

---------------------------------------------------------------------
-- Analysis
---------------------------------------------------------------------
isInitName :: ATermTable -> Bool
isInitName = isNamed "initialized_name"

isVariableDeclaration :: ATermTable -> Bool
isVariableDeclaration = isNamed "variable_declaration"

execDeclWarn :: ATermTable -> IO [String]
execDeclWarn at = do
  (_, w) <- execRWST (everywhere_ declWarn) at ()
  return w

declWarn :: (MonadIO m) => CheckM [String] st m (Maybe ())
declWarn = satisfy isVariableDeclaration $ inSubtree_ checkInitNames
  where
  checkInitNames = do
    kids <- childrenM
    bs   <- mapM (appM isInitName) kids
    when (length (filter id bs) > 1) $ do
      mfis <- inSubtree (extractFileInfo `liftM` currentTerm)
      let fis = catMaybes (concat mfis)
      tell (map format fis)
  -- Utility functions
  format (s,l,c) = "Multiple Declarations: " ++ (unquoteL . unquoteR) s
                                             ++ ":" ++ show l ++ "." ++ show c
  unquoteL ('"':s) = s
  unquoteL s       = s
  unquoteR s = case reverse s of
               '"':s' -> reverse s'
               _      -> s
