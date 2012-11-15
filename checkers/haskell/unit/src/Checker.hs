module Main where

import System.Console.GetOpt
import System.Environment ( getArgs )

import Control.Monad ( when, guard )
import Control.Monad.Trans.RWS.Strict
import Control.Monad.IO.Class
import Control.Applicative

import Data.List ( foldl', isSuffixOf )

-- Project specific imports
import ATerm.AbstractSyntax -- ( ATermTable, getATermByIndex1, getATerm )
import ATerm.Utilities hiding (foldl', mapM_, mapM, map, concatMap)
import ATerm.Matching
import ATerm.Pretty
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
  rs <- exec aterms
  putStrLn "Results: "
  mapM_ putStrLn rs
  return ()
  where
  removeTrailingDot s
    | length s >= 2 && ".\n" `isSuffixOf` s = init (init s)
    | otherwise = s

---------------------------------------------------------------------
-- Analysis
---------------------------------------------------------------------
variableTypes :: ATermTable -> [(ATermTable, ATermTable)]
variableTypes t = do
  _ <- exactlyNamed "variable_declaration" t
  l <- getATermFromTable t <$> U.children t
  guard . isList . getATerm $ l
  c <- getATermFromTable l <$> U.children l
  _ <- exactlyNamed "initialized_name" c
  annot <- getATermFromTable c <$> U.children c
  _ <- exactlyNamed "initialized_name_annotation" annot
  (ty:name:_) <- return $ U.children annot
  return ( getATermFromTable annot ty
         , getATermFromTable annot name)
  where
  isList (ShAList {}) = True
  isList _            = False

unit :: (MonadIO m) => CheckM [String] st m ()
unit = do
  t <- currentTerm
  case variableTypes t of
    [] -> return ()
    xs -> tell (map (\(x,y) -> show (ppATerm x, ppATerm y)) xs)

exec :: ATermTable -> IO [String]
exec at = do
  (_, w) <- execRWST (everywhere_ unit) at ()
  return w
