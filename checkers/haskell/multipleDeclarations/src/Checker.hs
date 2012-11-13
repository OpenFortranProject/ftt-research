module Main where

import System.Console.GetOpt
import System.Environment ( getArgs )

import Control.Monad ( when )
import Control.Monad.Trans.RWS.Strict
import Control.Monad.IO.Class
import Control.Applicative

import Data.List ( foldl', isSuffixOf )
import Data.Maybe ( catMaybes, isJust )

-- Project specific imports
import ATerm.AbstractSyntax -- ( ATermTable, getATermByIndex1, getATerm )
import ATerm.Utilities hiding (foldl', mapM_, mapM, map, concatMap)
import ATerm.Matching
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
initialized_name :: ATermTable -> Maybe Binding
initialized_name t = exactlyNamed "initialized_name" t

isInitName :: ATermTable -> Binding -> Bool
isInitName at (BoundTerm i) = isJust (initialized_name (getATermByIndex1 i at))
isInitName at (BoundAppl i) = isJust (initialized_name (getATermByIndex1 i at))
isInitName _  _             = False

extractFileInfo' :: ATermTable -> Binding -> Maybe (String, Integer, Integer)
extractFileInfo' at (BoundTerm i) = extractFileInfo (getATermByIndex1 i at)
extractFileInfo' at (BoundAppl i) = extractFileInfo (getATermByIndex1 i at)
extractFileInfo' _  _             = Nothing


isVariableDeclaration :: ATermTable -> Maybe [Binding]
isVariableDeclaration t = fmap concat (varDecl t)
  where
  varDecl a = do
    _ <- exactlyNamed "variable_declaration" a
    contains [ containsL [bindA]
             , fmap return . bindA
             ] a

execDeclWarn :: ATermTable -> IO [String]
execDeclWarn at = do
  (_, w) <- execRWST (everywhere_ declWarn) at ()
  return w

declWarn :: (MonadIO m) => CheckM [String] st m ()
declWarn = do
  t <- currentTerm
  case isVariableDeclaration t of
    Just xs -> do
      let initNames = map (isInitName t) xs
          fis       = catMaybes (map (extractFileInfo' t) xs)
      when (length (filter id initNames) > 1) $ do
        tell (map format fis)
    _      -> return ()
  where
  -- Utility functions
  format (s,l,c) = "Multiple Declarations: " ++ (unquoteL . unquoteR) s
                                             ++ ":" ++ show l ++ "." ++ show c
  unquoteL ('"':s) = s
  unquoteL s       = s
  unquoteR s = case reverse s of
               '"':s' -> reverse s'
               _      -> s
