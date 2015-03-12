{-# LANGUAGE LambdaCase, OverloadedStrings, NamedFieldPuns #-}
module Slick.Protocol where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson          hiding (Error)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V

type Pos  = (Int, Int)
type Span = (Pos, Pos)

data ToClient
  = Replace Span FilePath String
  | SetInfoWindow String
  | SetCursor Pos
  | CurrentHoleEnv HoleEnv
  | Ok
  | Error String
  | Stop
  deriving (Show)

instance ToJSON ToClient where
  toJSON = \case
    Replace sp p t     -> tag "Replace"        [toJSON sp, toJSON p, toJSON t]
    SetInfoWindow t    -> tag "SetInfoWindow"  [toJSON t]
    SetCursor pos      -> tag "SetCursor"      [toJSON pos]
    CurrentHoleEnv env -> tag "CurrentHoleEnv" [toJSON env]
    Ok                 -> tag "Ok"             []
    Error t            -> tag "Error"          [toJSON t]
    Stop               -> tag "Stop"           []
    where tag :: String -> [Value] -> Value
          tag name values = Array . V.fromList $ toJSON name : values
    


-- | A rich representation of a hole's environment which contains the
-- goal (ie expected type) and relevant visible bindings.
data HoleEnv = HoleEnv
  { goal :: (String, String)
  , bindings :: [(String, String)]
  }
  deriving (Show)

instance ToJSON HoleEnv where
  toJSON HoleEnv { goal, bindings } =
    Object $ HM.fromList [("goal", toJSON goal), ("bindings", toJSON bindings)]

type Var = String

data ClientState = ClientState { path :: FilePath, cursorPos :: (Int, Int) }
  deriving (Show)

instance FromJSON ClientState where
  parseJSON (Object v) = ClientState <$> v .: "path" <*> v .: "cursorPos"
  parseJSON _          = mzero

-- Things could be a bit more efficient if the client held on to which hole
-- they're in. Probably not necessary, but if things are slow, consider
-- it.

-- next big thing: in hole suggestions.
-- let's say my goal type is SrcLoc.
-- Functions are suggested whose target type is SrcLoc
-- and whose arguments are in the environment. Perhaps
-- do something linear. Also maybe use hoogle
--
data FromClient
  = Load FilePath
  | EnterHole ClientState
  | NextHole ClientState
  | PrevHole ClientState
  | GetEnv ClientState
  | GetEnvJSON ClientState
  | Refine String ClientState
  | GetType String
  | CaseFurther Var ClientState
  | CaseOn String
  | SendStop
  deriving (Show)

instance FromJSON FromClient where
  parseJSON = \case
    Array a                                     -> case V.toList a of
      [String "Load", String path]              -> return (Load (T.unpack path))
      [String "CaseFurther", String var, state] -> CaseFurther (T.unpack var) <$> parseJSON state
      [String "EnterHole", state]               -> EnterHole <$> parseJSON state
      [String "NextHole", state]                -> NextHole <$> parseJSON state
      [String "PrevHole", state]                -> PrevHole <$> parseJSON state
      [String "GetEnv", state]                  -> GetEnv <$> parseJSON state
      [String "GetEnvJSON", state]              -> GetEnvJSON <$> parseJSON state
      [String "Refine", String expr, state]     -> Refine (T.unpack expr) <$> parseJSON state
      [String "GetType", String e]              -> return . GetType $ T.unpack e
      [String "SendStop"]                       -> return SendStop
      _                                         -> mzero
    _                                           -> mzero

