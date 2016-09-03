{-# LANGUAGE TemplateHaskell #-}

module Data.Bson.Serialization
    ( Bsonable (..)
    , mkBsonable, mkBsonables
    ) where

import           Data.Bson           (Document, Field, Label, Val, Value, at,
                                      look, lookup, (=:), label)
import           Data.Maybe          (isJust)
import           Data.List           ((!!))
import           Data.Text           (pack)
import           Instances.TH.Lift
import           Language.Haskell.TH
import           Prelude             hiding (lookup)

class (Show a, Eq a) => Bsonable a where
    toBSON :: a -> Document
    fromBSON :: Monad m => Document -> m a

mkBsonable :: Name -> Q [Dec]
mkBsonable typeName = do
  let dataName = copyName typeName
  doc <- newName "doc"
  fields <- fieldsOf typeName
  let bsonize = listE $ map getField fields
  let toBSON = [| \x -> map ($ x) $bsonize |]
  let checkBSON = [| (`checkFields` fields) |]
  dataData <- sequence $ sequence <$> fields `zip` (getValue doc <$> fields)
  let fromBSON = return $ LamE [VarP doc] (RecConE dataName dataData)
  [d|
    instance Bsonable $(conT typeName) where
        toBSON = $toBSON
        fromBSON doc = if $checkBSON doc then return $ $fromBSON doc else fail "Invalid document"|]

copyName :: Name -> Name
copyName = mkName . nameBase

fieldsOf :: Name -> Q [Name]
fieldsOf name = do TyConI (DataD _ _ _ [RecC _ fds] _) <- reify name
                   return $ map (\(name, _, _) -> name) fds

mkBsonables :: [Name] -> Q [Dec]
mkBsonables = (concat <$>) . mapM mkBsonable

getValue :: Name -> Name -> Q Exp
getValue doc field = [| fieldName `at` $(varE doc) |]
    where fieldName = mkLabel field

getField :: Name -> Q Exp
getField field = [| \d -> fieldName =: $(varE field) d |]
    where fieldName = mkLabel field

checkFields :: Document -> [Name] -> Bool
checkFields doc = all (hasLabel doc . mkLabel)

mkLabel :: Name -> Label
mkLabel = pack . nameBase

hasLabel :: Document -> Label -> Bool
hasLabel doc label = isJust $ label `look` doc
