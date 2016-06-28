{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | Some macroses used in code.

module Libtorrent.TH where

import qualified Crypto.Hash as CH
import qualified Data.Binary as Binary
import           Data.Word (Word32)
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr ( Ptr, nullPtr )
import           Language.C.Inline.Internal
import qualified Language.C.Inline as C
import           Language.Haskell.TH
import           System.Random (randomIO)

import           Libtorrent.Internal
import           Libtorrent.Types.ArrayLike
import           Libtorrent.Types

-- | Create type classes for 'StdVector' (C++ std::vector<ctype>)
--
-- @
-- $(defineStdvector "string" "VectorString" ''C'String ''C'VectorString ''StdString)
-- @
defineStdVector :: String -- ^ C type name
                -> String -- ^ std::vector alias created by typedef
                -> Name   -- ^ c-inline element type
                -> Name   -- ^ c-inline vector type
                -> Name   -- ^ element type, must be instance of 'Inlinable' and 'FromPtr'
                -> DecsQ
defineStdVector c'typeName c'arrTypeName c'elType c'arrType elType =
  defineVectorLike c'typeName c'arrTypeName c'elType c'arrType elType ''StdVector

-- | Create type classes for C++ container type
--
-- @
-- $(defineVectorLike "string" "VectorString" ''C'String| ''C'VectorString ''StdString ''StdVector)
-- @
defineVectorLike :: String -- ^ C type name
                 -> String -- ^ std::vector alias created by typedef
                 -> Name   -- ^ c-inline element type
                 -> Name   -- ^ c-inline vector type
                 -> Name   -- ^ element type, must be instance of 'Inlinable' and 'FromPtr'
                 -> Name   -- ^ container type
                 -> DecsQ
defineVectorLike c_el_type_name c_arr_type_name c'elType c'arrType elType arrType = do
  let typeInfo tc = do
        TyConI ty <- reify tc
        case ty of
          (DataD _ n _ cs _) ->
            return (n, cs)
          (NewtypeD _ n _ c _) ->
            return (n, [c])
          _ ->
            error $ "type of " ++ (show tc) ++ " must be data or newtype type "
      constrName _ [(RecC n [_])] = n
      constrName _ [(NormalC n [_])] = n
      constrName tc _ = error $ (show tc) ++ " must have only one constructor with one argument"
  (c'elTypeName, _)  <-  typeInfo c'elType
  (c'arrTypeName, _) <-  typeInfo c'arrType
  (elTypeName, _)    <-  typeInfo elType
  (arrTypeName, arrConstrs)   <-  typeInfo arrType
  let arrConstrName = constrName arrType arrConstrs

  destrFname <- uniqueCName ("delete_" ++ c_arr_type_name)
  elemFname <- uniqueCName ("get_elem_" ++ c_arr_type_name)
  constrFname <- uniqueCName ("new_" ++ c_arr_type_name)
  addElemFname <- uniqueCName ("add_elem_" ++ c_arr_type_name)
  fptr <- newName "fptr"

  let destrCode = Code Safe destrFtype
             destrFname
             ("void " ++ destrFname ++ "(" ++ c_arr_type_name ++ "* ptr) { delete ptr; }")
      elemCode = Code Safe elemFtype
             elemFname
             (c_el_type_name ++ " * " ++ elemFname ++ "(" ++ c_arr_type_name ++ " *ptr, size_t i) {\n" ++
              c_arr_type_name ++" v = *ptr;\n" ++
              "if (i >= v.size()) \n" ++
              "  return NULL;\n" ++
              "else\n" ++
              "return new " ++ c_el_type_name ++ "(v[i]);\n" ++
              "}\n")
      constrCode = Code Safe constrFtype
             constrFname
             (c_arr_type_name ++ " * "++ constrFname ++ "()" ++ " { return new " ++ c_arr_type_name ++"(); }")
      addelemCode = Code Safe addElemFtype
             addElemFname
              ("void " ++ addElemFname ++ "(" ++ c_arr_type_name ++ " * vPtr, " ++ c_el_type_name ++ " *ePtr) { vPtr->push_back(*ePtr); }")

      destrFtype = [t| Ptr $(conT c'arrTypeName) -> IO () |]
      elemFtype = [t| Ptr $(conT c'arrTypeName) -> C.CSize -> IO (Ptr $(conT c'elTypeName)) |]
      constrFtype = [t| IO (Ptr $(conT c'arrTypeName)) |]
      addElemFtype = [t| Ptr $(conT c'arrTypeName) -> Ptr $(conT c'elTypeName) -> IO ()|]

  [d| instance Inlinable $(appT (conT arrTypeName) $ conT elTypeName)  where
        type (CType $(appT (conT arrTypeName) $ conT elTypeName)) = $(conT c'arrTypeName)

      instance FromPtr $(appT (conT arrTypeName) $ conT elTypeName) where
        fromPtr = objFromPtr $(conE arrConstrName) $(inlineCode destrCode)

      instance WithPtr $(appT (conT arrTypeName) $ conT elTypeName) where
        withPtr $(conP arrConstrName [varP fptr]) = withForeignPtr $(varE fptr)

      instance ArrayLike $(appT (conT arrTypeName) $ conT elTypeName) where
        type ElemType $(appT (conT arrTypeName) $ conT elTypeName) = $(conT elTypeName)

        getElem v i =
          withPtr v $ \vPtr -> do
            ptr <- $(inlineCode elemCode) vPtr i
            if ptr == nullPtr
            then return Nothing
            else Just <$> fromPtr (pure ptr)

      instance VectorLike $(appT (conT arrTypeName) $ conT elTypeName) where
        newVector = fromPtr $(inlineCode constrCode)

        addElem v e =
          withPtr v $ \vPtr ->
          withPtr e $ \ePtr ->
          $(inlineCode addelemCode) vPtr ePtr |]

-- From c-inline

uniqueCName :: String -> Q String
uniqueCName x = do
  c' <- runIO randomIO :: Q Word32
  let unique = CH.hashlazy $ Binary.encode x :: CH.Digest CH.SHA1
  module_ <- loc_module <$> location
  let replaceDot '.' = '_'
      replaceDot c = c
  return $ "inline_c_" ++ map replaceDot module_ ++ "_" ++ show c' ++ "_" ++ show unique
