{-# Language OverloadedStrings #-}
module Cryptol(
    Cryptol(..)
  , CryptolError
  , ParsedExpr
  , CheckedExpr

  , runCryptol'
  , mkModuleInput
  , withDefaultModuleInput
  , loadPrelude
  , loadModuleByName
  , loadModuleByPath
  , parseExpr
  , checkExpr
  , evalExpr
  , evalExpr'

  , Value
  , GenValue(..)
  ) where


import qualified Data.ByteString as BS
import Control.Monad(void, ap, liftM, (>=>))
import Data.Text (Text)

import qualified Cryptol.ModuleSystem as MS
import Cryptol.Eval.Concrete(Value)
import qualified Cryptol.Parser as Parser
import qualified Cryptol.Eval as Eval
import qualified Cryptol.Utils.Logger as Logger
import qualified Cryptol.TypeCheck as TypeCheck
import qualified Cryptol.TypeCheck.Solver.SMT as SMT
import Cryptol.Utils.Ident (preludeName, floatName)
import Cryptol.Eval.Value (GenValue(..))
import qualified Control.Exception as Ex
import qualified Cryptol.Utils.Ident as Ident
import qualified Cryptol.Eval.Value as Value
import qualified Cryptol.Parser.AST as PAST
import qualified Cryptol.TypeCheck.AST as CAST

-- TODO: what to do about warnings?
newtype Cryptol a = Cryptol { runCryptol :: MS.ModuleInput IO -> IO (a, MS.ModuleEnv) }
type ParsedExpr = PAST.Expr PAST.PName
type CheckedExpr =  CAST.Expr

instance Functor Cryptol where
  fmap = liftM

instance Applicative Cryptol where
  (<*>) = ap
  pure a =
    Cryptol $ \mi -> pure (a, MS.minpModuleEnv mi)

instance Monad Cryptol where
  ma >>= fb =
    Cryptol $ \mi ->
      do  (a, env) <- runCryptol ma mi
          let mi' = mi { MS.minpModuleEnv = env }

          runCryptol (fb a) mi'

data CryptolError =
    ModuleErr MS.ModuleError
  | ParseErr Parser.ParseError
  deriving Show  -- TODO: is this going to give a nice display for uncaught stuff?

instance Ex.Exception CryptolError where

throw :: CryptolError -> Cryptol a
throw = Ex.throw

liftMS :: MS.ModuleCmd a -> Cryptol a
liftMS cmd =
  Cryptol $ \mi ->
      -- TODO: do something with warnings
    do  (result, _) <- cmd mi
        case result of
          Left err -> Ex.throw (ModuleErr err)
          Right (a, env) -> pure (a, env)

asks :: (MS.ModuleInput IO -> a) -> Cryptol a
asks f = Cryptol $ \mi -> pure (f mi, MS.minpModuleEnv mi)

throwLeft :: Either l a -> (l -> CryptolError) -> Cryptol a
throwLeft e hdl =
  case e of
    Left err -> throw (hdl err)
    Right a -> pure a



-------------------------------------------------------------------------------
-- API

-- | Build a ModuleInput with default settings - if you don't care much about
--   the settings in MS.ModuleInput consider using `withDefaultModuleInput`
--   or `runCryptol'`
mkModuleInput :: MS.ModuleEnv -> SMT.Solver -> MS.ModuleInput IO
mkModuleInput env solver =
  MS.ModuleInput  { MS.minpModuleEnv = env
                  , MS.minpEvalOpts =
                      pure $
                        Eval.EvalOpts { Eval.evalLogger = Logger.quietLogger
                                      , Eval.evalPPOpts = Eval.defaultPPOpts
                                      }
                  , MS.minpByteReader = BS.readFile
                  , MS.minpCallStacks = False
                  , MS.minpTCSolver = solver
                  }

-- | Execute a computation (usually some form of `runCryptol`) with a ModuleInput
--   with reasonable configuration defaults.
withDefaultModuleInput :: (MS.ModuleInput IO -> IO a) -> IO a
withDefaultModuleInput f =
  do  env <- MS.initialModuleEnv
      let solverConfig = TypeCheck.defaultSolverConfig []
      SMT.withSolver (pure ()) solverConfig (f . mkModuleInput env)

-- | Run a cryptol computation, returning only the result and using
--   the default configurations.  Also automatically loads the prelude.
runCryptol' :: Cryptol a -> IO a
runCryptol' c =
  fst <$> withDefaultModuleInput (runCryptol (loadPrelude >> loadFloatFuncs >> c))


-- | Load the prelude.  Most modules will also automatically load the prelude.
loadPrelude :: Cryptol ()
loadPrelude =  void $ liftMS (MS.loadModuleByName preludeName)

-- | Load in standard functions for working with floating point numbers.
loadFloatFuncs :: Cryptol ()
loadFloatFuncs = void $ liftMS (MS.loadModuleByName floatName)

-- | Load a module from a file.
loadModuleByPath :: FilePath -> Cryptol ()
loadModuleByPath fp = void $ liftMS (MS.loadModuleByPath fp)

-- | Load a module by name using the default search path (?)
loadModuleByName :: Text -> Cryptol ()
loadModuleByName name =
  let mname = Ident.textToModName name
  in void $ liftMS (MS.loadModuleByName mname)

-- | Parse a Text into an expression, throwing an error on failure.
--   To make an expression without parsing, consider using the
--   `Cryptol.Parser.AST` module
parseExpr :: Text -> Cryptol ParsedExpr
parseExpr src = Parser.parseExpr src `throwLeft` ParseErr

-- | Typecheck a parsed expression, throwing an error on failure
checkExpr :: ParsedExpr -> Cryptol CheckedExpr
checkExpr expr =
  do  (_, e', _) <- liftMS (MS.checkExpr expr)
      pure e'

-- | Evaluate a checked expression, throwing an error on failure
evalExpr :: CheckedExpr -> Cryptol Value
evalExpr expr = liftMS (MS.evalExpr expr)


evalExpr' :: Text -> Cryptol Value
evalExpr' = parseExpr >=> checkExpr >=> evalExpr
