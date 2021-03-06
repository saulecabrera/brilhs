{-# LANGUAGE OverloadedStrings #-}
module Id
  ( Ident
  , Arg
  , OptionalType
  , Ty(..)
  , Literal(..)
  , Dest(..)
  , optionalDest
  )
  where

import           Data.Aeson
import           Data.Aeson.Types  (Value (Bool, Number), typeMismatch)
import           Data.HashMap.Lazy as HM
import           Data.Scientific   (Scientific)
import           Data.Text

data Ty = Int | Bool | Float | Pointer Ty deriving (Show, Eq)
type OptionalType = Maybe Ty

type Ident = Text
type Arg = Text

data Dest = Dest Ty Ident deriving (Show, Eq)

data Literal = Boolean Bool
             | Number Scientific
             deriving (Show, Eq)

instance ToJSON Literal where
  toJSON (Boolean b)   = Data.Aeson.Types.Bool b
  toJSON (Id.Number n) = Data.Aeson.Types.Number n


pointerOf :: Ty -> Ty
pointerOf = Pointer

optionalDest :: Maybe Ty -> Maybe Ident -> Maybe Dest
optionalDest _ Nothing              = Nothing
optionalDest Nothing _              = Nothing
optionalDest (Just ty) (Just ident) = Just $ Dest ty ident


instance FromJSON Dest where
  parseJSON = withObject "dest" $ \o->
    Dest <$> o .: "type" <*> o .: "name"

instance ToJSON Dest where
  toJSON (Dest ty ident) = object ["type" .= ty, "name" .= ident]

instance FromJSON Ty where
  parseJSON (String "int") =
    return Int

  parseJSON (String "float") =
    return Float

  parseJSON (String "bool") =
    return Id.Bool

  parseJSON (Object o) =
    case HM.lookup "ptr" o of
       Just (String "int") -> return $ pointerOf Int
       Just (String "float") -> return $ pointerOf Float
       Just (String "bool") -> return $ pointerOf Id.Bool
       Just o -> do
         ty <- parseJSON o
         return $ pointerOf ty
       _  ->  typeMismatch "Object containing a ptr key" (Object o)

  parseJSON val =  typeMismatch "String (int, float, bool) or Object" val

instance ToJSON Ty where
  toJSON Int          = String "int"
  toJSON Float        = String "float"
  toJSON Id.Bool      = String "bool"
  toJSON (Pointer ty) = object ["ptr" .= toJSON ty]

instance FromJSON Literal where
  parseJSON (Data.Aeson.Types.Bool b) =
    return $ Boolean b

  parseJSON (Data.Aeson.Types.Number s) =
    return $ Id.Number s

  parseJSON val = typeMismatch "Bool or Number" val

