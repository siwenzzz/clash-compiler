{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for converting Core Type/Term to Netlist datatypes
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
#if !MIN_VERSION_ghc(8,8,0)
{-# LANGUAGE MonadFailDesugaring #-}
#endif
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist.Util where

import           Control.Error           (hush)
import           Control.Exception       (throw)
import           Control.Lens            ((.=),(%=))
import qualified Control.Lens            as Lens
import           Control.Monad           (unless, when, zipWithM, join)
import           Control.Monad.Reader    (ask, local)
import qualified Control.Monad.State as State
import           Control.Monad.State.Strict
  (State, evalState, get, modify, runState)
import           Control.Monad.Trans.Except
  (ExceptT (..), runExcept, runExceptT, throwE)
import           Data.Either             (partitionEithers)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.String             (fromString)
import           Data.List               (intersperse, unzip4, intercalate)
import qualified Data.List               as List
import qualified Data.List.Extra         as List
import           Data.Maybe
  (catMaybes, fromMaybe, isNothing, isJust, fromJust)
import           Data.Monoid             (First (..))
import           Text.Printf             (printf)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup          ((<>))
#endif
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Prettyprint.Doc (Doc)

import           Outputable              (ppr, showSDocUnsafe)

import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, ConstrRepr'(..), DataRepr'(..), getDataRepr,
   uncheckedGetConstrRepr)
import           Clash.Annotations.TopEntity (PortName (..), TopEntity (..))
import           Clash.Backend           (HWKind(..), hdlHWTypeKind)
import           Clash.Driver.Types      (ClashOpts (..))
import           Clash.Core.DataCon      (DataCon (..))
import           Clash.Core.FreeVars     (localIdOccursIn, typeFreeVars)
import qualified Clash.Core.Literal      as C
import           Clash.Core.Name
  (Name (..), appendToName, nameOcc)
import           Clash.Core.Pretty       (showPpr)
import           Clash.Core.Subst
  (Subst (..), extendIdSubst, extendIdSubstList, extendInScopeId,
   extendInScopeIdList, mkSubst, substTm)
import           Clash.Core.Term
  (Alt, LetBinding, Pat (..), Term (..), TickInfo (..), NameMod (..),
   collectArgsTicks, collectTicks, collectBndrs, PrimInfo(primName), mkTicks, stripTicks)
import           Clash.Core.TermInfo
import           Clash.Core.TyCon
  (TyConName, TyConMap, tyConDataCons)
import           Clash.Core.Type         (Type (..), TypeView (..),
                                          coreView1, splitTyConAppM, tyView, TyVar)
import           Clash.Core.Util
  (substArgTys, tyLitShow)
import           Clash.Core.Var
  (Id, Var (..), mkLocalId, modifyVarName, Attr')
import           Clash.Core.VarEnv
  (InScopeSet, extendInScopeSetList, uniqAway)
import {-# SOURCE #-} Clash.Netlist.BlackBox
import {-# SOURCE #-} Clash.Netlist.BlackBox.Util
import           Clash.Netlist.Id        (IdType (..), stripDollarPrefixes)
import           Clash.Netlist.Types     as HW
import           Clash.Primitives.Types
import           Clash.Unique
import           Clash.Util
import qualified Clash.Util.Interpolate  as I

-- | Throw away information indicating which constructor fields were filtered
-- due to being void.
stripFiltered :: FilteredHWType -> HWType
stripFiltered (FilteredHWType hwty _filtered) = hwty

-- | Strip as many "Void" layers as possible. Might still return a Void if the
-- void doesn't contain a hwtype.
stripVoid :: HWType -> HWType
stripVoid (Void (Just e)) = stripVoid e
stripVoid e = e

flattenFiltered :: FilteredHWType -> [[Bool]]
flattenFiltered (FilteredHWType _hwty filtered) = map (map fst) filtered

isVoidMaybe :: Bool -> Maybe HWType -> Bool
isVoidMaybe dflt Nothing = dflt
isVoidMaybe _dflt (Just t) = isVoid t

-- | Determines if type is a zero-width construct ("void")
isVoid :: HWType -> Bool
isVoid Void {} = True
isVoid _       = False

-- | Same as @isVoid@, but on @FilteredHWType@ instead of @HWType@
isFilteredVoid :: FilteredHWType -> Bool
isFilteredVoid = isVoid . stripFiltered

mkIdentifier :: IdType -> Identifier -> NetlistMonad Identifier
mkIdentifier typ nm = Lens.use mkIdentifierFn <*> pure typ <*> pure nm

extendIdentifier
  :: IdType
  -> Identifier
  -> Identifier
  -> NetlistMonad Identifier
extendIdentifier typ nm ext =
  Lens.use extendIdentifierFn <*> pure typ <*> pure nm <*> pure ext

-- | Split a normalized term into: a list of arguments, a list of let-bindings,
-- and a variable reference that is the body of the let-binding. Returns a
-- String containing the error if the term was not in a normalized form.
splitNormalized
  :: TyConMap
  -> Term
  -> (Either String ([Id],[LetBinding],Id))
splitNormalized tcm expr = case collectBndrs expr of
  (args, collectTicks -> (Letrec xes e, ticks))
    | (tmArgs,[]) <- partitionEithers args -> case stripTicks e of
        Var v -> Right (tmArgs, fmap (second (`mkTicks` ticks)) xes,v)
        _     -> Left ($(curLoc) ++ "Not in normal form: res not simple var")
    | otherwise -> Left ($(curLoc) ++ "Not in normal form: tyArgs")
  _ ->
    Left ($(curLoc) ++ "Not in normal form: no Letrec:\n\n" ++ showPpr expr ++
          "\n\nWhich has type:\n\n" ++ showPpr ty)
 where
  ty = termType tcm expr

-- | Same as @unsafeCoreTypeToHWType@, but discards void filter information
unsafeCoreTypeToHWType'
  :: SrcSpan
  -- ^ Approximate location in original source file
  -> String
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> TyConMap
  -> Type
  -> State HWMap HWType
unsafeCoreTypeToHWType' sp loc builtInTranslation reprs m ty =
  stripFiltered <$> (unsafeCoreTypeToHWType sp loc builtInTranslation reprs m ty)

-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Errors if the Core type is not translatable.
unsafeCoreTypeToHWType
  :: SrcSpan
  -- ^ Approximate location in original source file
  -> String
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> TyConMap
  -> Type
  -> State HWMap FilteredHWType
unsafeCoreTypeToHWType sp loc builtInTranslation reprs m ty =
  either (\msg -> throw (ClashException sp (loc ++ msg) Nothing)) id <$>
    coreTypeToHWType builtInTranslation reprs m ty

-- | Same as @unsafeCoreTypeToHWTypeM@, but discards void filter information
unsafeCoreTypeToHWTypeM'
  :: String
  -> Type
  -> NetlistMonad HWType
unsafeCoreTypeToHWTypeM' loc ty =
  stripFiltered <$> unsafeCoreTypeToHWTypeM loc ty

-- | Converts a Core type to a HWType within the NetlistMonad; errors on failure
unsafeCoreTypeToHWTypeM
  :: String
  -> Type
  -> NetlistMonad FilteredHWType
unsafeCoreTypeToHWTypeM loc ty = do
  (_,cmpNm) <- Lens.use curCompNm
  tt        <- Lens.use typeTranslator
  reprs     <- Lens.use customReprs
  tcm       <- Lens.use tcCache
  htm0      <- Lens.use htyCache
  let (hty,htm1) = runState (unsafeCoreTypeToHWType cmpNm loc tt reprs tcm ty) htm0
  htyCache Lens..= htm1
  return hty

-- | Same as @coreTypeToHWTypeM@, but discards void filter information
coreTypeToHWTypeM'
  :: Type
  -- ^ Type to convert to HWType
  -> NetlistMonad (Maybe HWType)
coreTypeToHWTypeM' ty =
  fmap stripFiltered <$> coreTypeToHWTypeM ty


-- | Converts a Core type to a HWType within the NetlistMonad; 'Nothing' on failure
coreTypeToHWTypeM
  :: Type
  -- ^ Type to convert to HWType
  -> NetlistMonad (Maybe FilteredHWType)
coreTypeToHWTypeM ty = do
  tt    <- Lens.use typeTranslator
  reprs <- Lens.use customReprs
  tcm   <- Lens.use tcCache
  htm0  <- Lens.use htyCache
  let (hty,htm1) = runState (coreTypeToHWType tt reprs tcm ty) htm0
  htyCache Lens..= htm1
  return (hush hty)

-- | Constructs error message for unexpected projections out of a type annotated
-- with a custom bit representation.
unexpectedProjectionErrorMsg
  :: DataRepr'
  -> Int
  -- ^ Constructor index
  -> Int
  -- ^ Field index
  -> String
unexpectedProjectionErrorMsg dataRepr cI fI =
     "Unexpected projection of zero-width type: " ++ show (drType dataRepr)
  ++ ". Tried to make a projection of field " ++ show fI ++ " of "
  ++ constrNm ++ ". Did you try to project a field marked as zero-width"
  ++ " by a custom bit representation annotation?"
 where
   constrNm = show (crName (drConstrs dataRepr !! cI))

-- | Helper function of 'maybeConvertToCustomRepr'
convertToCustomRepr
  :: HasCallStack
  => CustomReprs
  -> DataRepr'
  -> HWType
  -> HWType
convertToCustomRepr reprs dRepr@(DataRepr' name' size constrs) hwTy =
  if length constrs == nConstrs then
    if size <= 0 then
      Void (Just cs)
    else
      cs
  else
    error (unwords
      [ "Type", show name', "has", show nConstrs, "constructor(s), "
      , "but the custom bit representation only specified", show (length constrs)
      , "constructors."
      ])
 where
  cs = insertVoids $ case hwTy of
    Sum name conIds ->
      CustomSum name dRepr size (map packSum conIds)
    SP name conIdsAndFieldTys ->
      CustomSP name dRepr size (map packSP conIdsAndFieldTys)
    Product name maybeFieldNames fieldTys
      | [ConstrRepr' _cName _pos _mask _val fieldAnns] <- constrs ->
      CustomProduct name dRepr size maybeFieldNames (zip fieldAnns fieldTys)
    _ ->
      error
        ( "Found a custom bit representation annotation " ++ show dRepr ++ ", "
       ++ "but it was applied to an unsupported HWType: " ++ show hwTy ++ ".")

  nConstrs :: Int
  nConstrs = case hwTy of
    (Sum _name conIds) -> length conIds
    (SP _name conIdsAndFieldTys) -> length conIdsAndFieldTys
    (Product {}) -> 1
    _ -> error ("Unexpected HWType: " ++ show hwTy)

  packSP (name, tys) = (uncheckedGetConstrRepr name reprs, name, tys)
  packSum name = (uncheckedGetConstrRepr name reprs, name)

  -- Replace some "hwTy" with "Void (Just hwTy)" if the custom bit
  -- representation indicated that field is represented by zero bits. We can't
  -- simply remove them, as we'll later have to deal with an "overapplied"
  -- constructor. If we remove the arguments altogether, we wouldn't know which
  -- - on their own potentially non-void! - arguments to ignore.
  insertVoids :: HWType -> HWType
  insertVoids (CustomSP i d s constrs0) =
    CustomSP i d s (map go0 constrs0)
   where
    go0 (con@(ConstrRepr' _ _ _ _ fieldAnns), i0, hwTys) =
      (con, i0, zipWith go1 fieldAnns hwTys)
    go1 0 hwTy0 = Void (Just hwTy0)
    go1 _ hwTy0 = hwTy0
  insertVoids (CustomProduct i d s f fieldAnns) =
    CustomProduct i d s f (map go fieldAnns)
   where
    go (0, hwTy0) = (0, Void (Just hwTy0))
    go (n, hwTy0) = (n, hwTy0)
  insertVoids hwTy0 = hwTy0

-- | Given a map containing custom bit representation, a type, and the same
-- type represented as HWType, convert the HWType to a CustomSP/CustomSum if
-- it has a custom bit representation.
maybeConvertToCustomRepr
  :: CustomReprs
  -- ^ Map containing all custom representations index on its type
  -> Type
  -- ^ Custom reprs are index on type, so we need the clash core type to look
  -- it up.
  -> HWType
  -- ^ Type of previous argument represented as a HWType
  -> HWType
maybeConvertToCustomRepr reprs (coreToType' -> Right tyName) hwTy
  | Just dRepr <- getDataRepr tyName reprs =
    convertToCustomRepr reprs dRepr hwTy
maybeConvertToCustomRepr _reprs _ty hwTy = hwTy

-- | Same as @coreTypeToHWType@, but discards void filter information
coreTypeToHWType'
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> TyConMap
  -> Type
  -- ^ Type to convert to HWType
  -> State HWMap (Either String HWType)
coreTypeToHWType' builtInTranslation reprs m ty =
  fmap stripFiltered <$> coreTypeToHWType builtInTranslation reprs m ty


-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Returns a string containing the error message when the Core
-- type is not translatable.
coreTypeToHWType
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> TyConMap
  -> Type
  -- ^ Type to convert to HWType
  -> State HWMap (Either String FilteredHWType)
coreTypeToHWType builtInTranslation reprs m ty = do
  htyM <- HashMap.lookup ty <$> get
  case htyM of
    Just hty -> return hty
    _ -> do
      hty0M <- builtInTranslation reprs m ty
      hty1  <- go hty0M ty
      modify (HashMap.insert ty hty1)
      return hty1
 where
  -- Try builtin translation; for now this is hardcoded to be the one in ghcTypeToHWType
  go :: Maybe (Either String FilteredHWType)
     -> Type
     -> State (HashMap Type (Either String FilteredHWType))
              (Either String FilteredHWType)
  go (Just hwtyE) _ = pure $
    (\(FilteredHWType hwty filtered) ->
      (FilteredHWType (maybeConvertToCustomRepr reprs ty hwty) filtered)) <$> hwtyE
  -- Strip transparant types:
  go _ (coreView1 m -> Just ty') =
    coreTypeToHWType builtInTranslation reprs m ty'
  -- Try to create hwtype based on AST:
  go _ (tyView -> TyConApp tc args) = runExceptT $ do
    FilteredHWType hwty filtered <- mkADT builtInTranslation reprs m (showPpr ty) tc args
    return (FilteredHWType (maybeConvertToCustomRepr reprs ty hwty) filtered)
  -- All methods failed:
  go _ _ = return $ Left $ "Can't translate non-tycon type: " ++ showPpr ty

-- | Generates original indices in list before filtering, given a list of
-- removed indices.
--
-- >>> originalIndices [False, False, True, False]
-- [0,1,3]
originalIndices
  :: [Bool]
  -- ^ Were voids. Length must be less than or equal to n.
  -> [Int]
  -- ^ Original indices
originalIndices wereVoids =
  [i | (i, void) <- zip [0..] wereVoids, not void]

-- | Converts an algebraic Core type (split into a TyCon and its argument) to a HWType.
mkADT
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> CustomReprs
  -> TyConMap
  -- ^ TyCon cache
  -> String
  -- ^ String representation of the Core type for error messages
  -> TyConName
  -- ^ The TyCon
  -> [Type]
  -- ^ Its applied arguments
  -> ExceptT String (State HWMap) FilteredHWType
  -- ^ An error string or a tuple with the type and possibly a list of
  -- removed arguments.
mkADT _ _ m tyString tc _
  | isRecursiveTy m tc
  = throwE $ $(curLoc) ++ "Can't translate recursive type: " ++ tyString

mkADT builtInTranslation reprs m _tyString tc args = case tyConDataCons (m `lookupUniqMap'` tc) of
  []  -> return (FilteredHWType (Void Nothing) [])
  dcs -> do
    let tcName           = nameOcc tc
        substArgTyss     = map (`substArgTys` args) dcs
    argHTyss0           <- mapM (mapM (ExceptT . coreTypeToHWType builtInTranslation reprs m)) substArgTyss
    let argHTyss1        = map (\tys -> zip (map isFilteredVoid tys) tys) argHTyss0
    let areVoids         = map (map fst) argHTyss1
    let filteredArgHTyss = map (map snd . filter (not . fst)) argHTyss1

    -- Every alternative is annotated with some examples. Be sure to read them.
    case (dcs, filteredArgHTyss) of
      -- Type has one constructor and that constructor has a single field,
      -- modulo empty fields if keepVoid is False. Examples of such fields
      -- are:
      --
      -- >>> data ABC = ABC Int
      -- >>> data DEF = DEF Int ()
      --
      -- Notice that @DEF@'s constructor has an "empty" second argument. The
      -- second field of FilteredHWType would then look like:
      --
      -- >>> [[False, True]]
      (_:[],[[elemTy]]) ->
        return (FilteredHWType (stripFiltered elemTy) argHTyss1)

      -- Type has one constructor, but multiple fields modulo empty fields
      -- (see previous case for more thorough explanation). Examples:
      --
      -- >>> data GHI = GHI Int Int
      -- >>> data JKL = JKL Int () Int
      --
      -- In the second case the second field of FilteredHWType would be
      -- [[False, True, False]]
      ([dcFieldLabels -> labels0],[elemTys@(_:_)]) -> do
        labelsM <-
          if null labels0 then
            return Nothing
          else
            -- Filter out labels belonging to arguments filtered due to being
            -- void. See argHTyss1.
            let areNotVoids = map not (head areVoids) in
            let labels1     = filter fst (zip areNotVoids labels0) in
            let labels2     = map snd labels1 in
            return (Just labels2)
        let hwty = Product tcName labelsM (map stripFiltered elemTys)
        return (FilteredHWType hwty argHTyss1)

      -- Either none of the constructors have fields, or they have been filtered
      -- due to them being empty. Examples:
      --
      -- >>> data MNO = M    | N | O
      -- >>> data PQR = P () | Q | R ()
      -- >>> data STU = STU
      -- >>> data VWX
      (_, concat -> [])
        -- If none of the dataconstructors have fields, and there are 1 or less
        -- of them, this type only has one inhabitant. It can therefore be
        -- represented by zero bits, and is therefore empty:
        | length dcs <= 1 -> case argHTyss0 of
            [argHTys0] ->
              -- We need this to preserve constraint-tuples of `KnownDomains`
              let argHTys1 = map (stripVoid . stripFiltered) argHTys0
              in  return (FilteredHWType
                            (Void (Just (Product tcName Nothing argHTys1)))
                            argHTyss1)
            _ -> return (FilteredHWType (Void Nothing) argHTyss1)
        -- None of the dataconstructors have fields. This type is therefore a
        -- simple Sum type.
        | otherwise ->
          return (FilteredHWType (Sum tcName $ map (nameOcc . dcName) dcs) argHTyss1)

      -- A sum of product, due to multiple constructors, where at least one
      -- of the constructor has one or more fields modulo empty fields. Example:
      --
      -- >>> data YZA = Y Int | Z () | A
      (_,elemHTys) ->
        return $ FilteredHWType (SP tcName $ zipWith
          (\dc tys ->  ( nameOcc (dcName dc), tys))
          dcs (map stripFiltered <$> elemHTys)) argHTyss1

-- | Simple check if a TyCon is recursively defined.
isRecursiveTy :: TyConMap -> TyConName -> Bool
isRecursiveTy m tc = case tyConDataCons (m `lookupUniqMap'` tc) of
    []  -> False
    dcs -> let argTyss      = map dcArgTys dcs
               argTycons    = (map fst . catMaybes) $ (concatMap . map) splitTyConAppM argTyss
           in tc `elem` argTycons

-- | Determines if a Core type is translatable to a HWType given a function that
-- translates certain builtin types.
representableType
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> Bool
  -- ^ String considered representable
  -> TyConMap
  -> Type
  -> Bool
representableType builtInTranslation reprs stringRepresentable m =
    either (const False) isRepresentable .
    flip evalState HashMap.empty .
    coreTypeToHWType' builtInTranslation reprs m
  where
    isRepresentable hty = case hty of
      String            -> stringRepresentable
      Vector _ elTy     -> isRepresentable elTy
      RTree  _ elTy     -> isRepresentable elTy
      Product _ _ elTys -> all isRepresentable elTys
      SP _ elTyss       -> all (all isRepresentable . snd) elTyss
      BiDirectional _ t -> isRepresentable t
      Annotated _ ty    -> isRepresentable ty
      _                 -> True

-- | Determines the bitsize of a type. For types that don't get turned
-- into real values in hardware (string, integer) the size is 0.
typeSize :: HWType
         -> Int
typeSize (Void {}) = 0
typeSize FileType = 32 -- (ref. page 287 of IEEE 1364-2005)
typeSize String = 0
typeSize Integer = 0
typeSize (KnownDomain {}) = 0
typeSize Bool = 1
typeSize Bit = 1
typeSize (Clock _) = 1
typeSize (Reset {}) = 1
typeSize (BitVector i) = i
typeSize (Index 0) = 0
typeSize (Index 1) = 1
typeSize (Index u) = fromMaybe 0 (clogBase 2 u)
typeSize (Signed i) = i
typeSize (Unsigned i) = i
typeSize (Vector n el) = n * typeSize el
typeSize (RTree d el) = (2^d) * typeSize el
typeSize t@(SP _ cons) = conSize t +
  maximum (map (sum . map typeSize . snd) cons)
typeSize (Sum _ dcs) = fromMaybe 0 . clogBase 2 . toInteger $ length dcs
typeSize (Product _ _ tys) = sum $ map typeSize tys
typeSize (BiDirectional In h) = typeSize h
typeSize (BiDirectional Out _) = 0
typeSize (CustomSP _ _ size _) = fromIntegral size
typeSize (CustomSum _ _ size _) = fromIntegral size
typeSize (CustomProduct _ _ size _ _) = fromIntegral size
typeSize (Annotated _ ty) = typeSize ty

-- | Determines the bitsize of the constructor of a type
conSize :: HWType
        -> Int
conSize (SP _ cons) = fromMaybe 0 . clogBase 2 . toInteger $ length cons
conSize t           = typeSize t

-- | Gives the length of length-indexed types
typeLength :: HWType
           -> Int
typeLength (Vector n _) = n
typeLength _            = 0

-- | Gives the HWType corresponding to a term. Returns an error if the term has
-- a Core type that is not translatable to a HWType.
termHWType :: String
           -> Term
           -> NetlistMonad HWType
termHWType loc e = do
  m <- Lens.use tcCache
  let ty = termType m e
  stripFiltered <$> unsafeCoreTypeToHWTypeM loc ty

-- | Gives the HWType corresponding to a term. Returns 'Nothing' if the term has
-- a Core type that is not translatable to a HWType.
termHWTypeM
  :: Term
  -- ^ Term to convert to HWType
  -> NetlistMonad (Maybe FilteredHWType)
termHWTypeM e = do
  m  <- Lens.use tcCache
  let ty = termType m e
  coreTypeToHWTypeM ty

isBiSignalIn :: HWType -> Bool
isBiSignalIn (BiDirectional In _) = True
isBiSignalIn _                    = False

containsBiSignalIn
  :: HWType
  -> Bool
containsBiSignalIn (BiDirectional In _) = True
containsBiSignalIn (Product _ _ tys) = any containsBiSignalIn tys
containsBiSignalIn (SP _ tyss)       = any (any containsBiSignalIn . snd) tyss
containsBiSignalIn (Vector _ ty)     = containsBiSignalIn ty
containsBiSignalIn (RTree _ ty)      = containsBiSignalIn ty
containsBiSignalIn _                 = False

-- | Helper function of @collectPortNames@, which operates on a @PortName@
-- instead of a TopEntity.
collectPortNames'
  :: [String]
  -> PortName
  -> [Identifier]
collectPortNames' prefixes (PortName nm) =
  let prefixes' = reverse (nm : prefixes) in
  [fromString (intercalate "_" prefixes')]
collectPortNames' prefixes (PortProduct "" nms) =
  concatMap (collectPortNames' prefixes) nms
collectPortNames' prefixes (PortProduct prefix nms) =
  concatMap (collectPortNames' (prefix : prefixes)) nms

-- | Recursively get all port names from top entity annotations. The result is
-- a list of user defined port names, which should not be used by routines
-- generating unique function names. Only completely qualified names are
-- returned, as it does not (and cannot) account for any implicitly named ports
-- under a PortProduct.
collectPortNames
  :: TopEntity
  -> [Identifier]
collectPortNames TestBench {} = []
collectPortNames Synthesize { t_inputs, t_output } =
  concatMap (collectPortNames' []) t_inputs ++ (collectPortNames' []) t_output

-- | Remove ports having a void-type from user supplied PortName annotation
filterVoidPorts
  :: FilteredHWType
  -> PortName
  -> PortName
filterVoidPorts _hwty (PortName s) =
  PortName s
filterVoidPorts (FilteredHWType _hwty [filtered]) (PortProduct s ps)
  | length filtered > 1
  = PortProduct s [filterVoidPorts f p | (p, (void, f)) <- zip ps filtered, not void]
filterVoidPorts (FilteredHWType _hwty fs) (PortProduct s ps)
  | length (filter (not.fst) (concat fs)) == 1
  , length fs > 1
  , length ps == 2
  = PortProduct s ps
filterVoidPorts filtered pp@(PortProduct _s _ps) =
  -- TODO: Prettify errors
  error $ $(curLoc) ++ "Ports were annotated as product, but type wasn't one: \n\n"
                    ++ "   Filtered was: " ++ show filtered ++ "\n\n"
                    ++ "   Ports was: " ++ show pp

-- | Uniquely rename all the variables and their references in a normalized
-- term
mkUniqueNormalized
  :: HasCallStack
  => InScopeSet
  -> Maybe (Maybe TopEntity)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just Nothing: term is a top entity, but has no explicit annotation
  --     * Just (Just ..): term is a top entity, and has an explicit annotation
  -> ( [Id]
     , [LetBinding]
     , Id
     )
  -> NetlistMonad
      ([Bool]
      ,[(Identifier,HWType)]
      ,[Declaration]
      ,[(Identifier,HWType)]
      ,[Declaration]
      ,[LetBinding]
      ,Maybe Id)
mkUniqueNormalized is0 topMM (args,binds,res) = do
  -- Add user define port names to list of seen ids to prevent name collisions.
  let
    portNames =
      case join topMM of
        Nothing  -> []
        Just top -> collectPortNames top

  seenIds %= (HashMap.unionWith max (HashMap.fromList (map (,0) portNames)))

  let (bndrs,exprs) = unzip binds

  -- Make arguments unique
  let is1 = is0 `extendInScopeSetList` (args ++ bndrs)
  (wereVoids,iports,iwrappers,substArgs) <- mkUniqueArguments (mkSubst is1) topMM args

  -- Make result unique. This might yield 'Nothing' in which case the result
  -- was a single BiSignalOut. This is superfluous in the HDL, as the argument
  -- will already contain a bidirectional signal complementing the BiSignalOut.
  resM <- mkUniqueResult substArgs topMM res
  case resM of
    Just (oports,owrappers,res1,substRes) -> do
      -- Check whether any of the binders reference the result
      let resRead = any (localIdOccursIn res) exprs
      -- Rename some of the binders, see 'setBinderName' when this happens.
      ((res2,subst1,extraBndr),bndrs1) <-
        List.mapAccumLM (setBinderName substRes res resRead) (res1,substRes,[]) binds
      -- Make let-binders unique, the result binder is already unique, so we
      -- can skip it.
      let (bndrsL,r:bndrsR) = break ((== res2)) bndrs1
      (bndrsL1,substL) <- mkUnique subst1 bndrsL
      (bndrsR1,substR) <- mkUnique substL bndrsR
      -- Replace old IDs by updated unique IDs in the RHSs of the let-binders
      let exprs1 = map (substTm ("mkUniqueNormalized1" :: Doc ()) substR) exprs
      -- Return the uniquely named arguments, let-binders, and result
      return ( wereVoids
             , iports
             , iwrappers
             , oports
             , owrappers
             , zip (bndrsL1 ++ r:bndrsR1) exprs1 ++ extraBndr
             , Just res1)
    Nothing -> do
      (bndrs1, substArgs1) <- mkUnique substArgs bndrs
      return ( wereVoids
             , iports
             , iwrappers
             , []
             , []
             , zip bndrs1
                   (map (substTm ("mkUniqueNormalized2" :: Doc ()) substArgs1) exprs)
             ,Nothing)

-- | Set the name of the binder
--
-- Normally, it just keeps the existing name, but there are two exceptions:
--
-- 1. It's the binding for the result which is also referenced by another binding;
--    in this case it's suffixed with `_rec`
-- 2. The binding binds a primitive that has a name control field
--
-- 2. takes priority over 1. Additionally, we create an additional binder when
-- the return value gets a new name.
setBinderName
  :: Subst
  -- ^ Current substitution
  -> Id
  -- ^ The binder for the result
  -> Bool
  -- ^ Whether the result binder is referenced by another binder
  -> (Id, Subst, [(Id,Term)])
  -- ^ * The (renamed) binder for the result
  --   * The updated substitution in case the result binder is renamed
  --   * A new binding, to assign the result in case the original binder for
  --     the result got renamed.
  -> (Id,Term)
  -- ^ The binding
  -> NetlistMonad ((Id, Subst, [(Id,Term)]),Id)
setBinderName subst res resRead m@(resN,_,_) (i,collectArgsTicks -> (k,args,ticks)) = case k of
  Prim p -> let nm = primName p in extractPrimWarnOrFail nm >>= go nm
  _ -> goDef
 where
  go nm (BlackBox {resultName = Just (BBTemplate nmD)}) = withTicks ticks $ \_ -> do
    (bbCtx,_) <- preserveVarEnv (mkBlackBoxContext nm i args)
    be <- Lens.use backend
    let bbRetValName = case be of
          SomeBackend s -> toStrict ((State.evalState (renderTemplate bbCtx nmD) s) 0)
        i1 = modifyVarName (\n -> n {nameOcc = bbRetValName}) i
    if res == i1 then do
      ([i2],subst1) <- mkUnique subst [i1]
      return ((i2,subst1,[(resN,Var i2)]),i2)
    else
      return (m,i1)

  go _ _ = goDef

  goDef
    | i == res && resRead
    = do
      ([i1],subst1) <- mkUnique subst [modifyVarName (`appendToName` "_rec") res]
      return ((i1, subst1, [(resN,Var i1)]),i1)
    | i == res
    = return (m,resN)
    | otherwise
    = return (m,i)

mkUniqueArguments
  :: Subst
  -> Maybe (Maybe TopEntity)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just Nothing: term is a top entity, but has no explicit annotation
  --     * Just (Just ..): term is a top entity, and has an explicit annotation
  -> [Id]
  -> NetlistMonad
       ( [Bool]                 -- Were voids
       , [(Identifier,HWType)]  -- Arguments and their types
       , [Declaration]          -- Extra declarations
       , Subst                  -- Substitution with new vars in scope
       )
mkUniqueArguments subst0 Nothing args = do
  (args',subst1) <- mkUnique subst0 args
  ports <- mapM idToInPort args'
  return (map isNothing ports, catMaybes ports, [], subst1)

mkUniqueArguments subst0 (Just teM) args = do
  let iPortSupply = maybe (repeat Nothing) (extendPorts . t_inputs) teM
  ports0 <- zipWithM go iPortSupply args
  let (ports1, decls, subst) = unzip3 (catMaybes ports0)
  return ( map isNothing ports0
         , concat ports1
         , concat decls
         , extendInScopeIdList (extendIdSubstList subst0 (map snd subst))
                               (map fst subst))
  where
    go pM var = do
      let i     = varName var
          i'    = nameOcc i
          ty    = varType var
      fHwty <- unsafeCoreTypeToHWTypeM $(curLoc) ty
      let FilteredHWType hwty _ = fHwty
      (ports,decls,_,pN) <- mkInput (filterVoidPorts fHwty <$> pM) (i',hwty)
      let pId  = mkLocalId ty (repName pN i)
      if isVoid hwty
         then return Nothing
         else return (Just (ports,decls,(pId,(var,Var pId))))


mkUniqueResult
  :: Subst
  -> Maybe (Maybe TopEntity)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just Nothing: term is a top entity, but has no explicit annotation
  --     * Just (Just ..): term is a top entity, and has an explicit annotation
  -> Id
  -> NetlistMonad (Maybe ([(Identifier,HWType)],[Declaration],Id,Subst))
mkUniqueResult subst0 Nothing res = do
  ([res'],subst1) <- mkUnique subst0 [res]
  portM <- idToOutPort res'
  case portM of
    Just port -> return (Just ([port],[],res',subst1))
    _         -> return Nothing

mkUniqueResult subst0 (Just teM) res = do
  (_,sp)    <- Lens.use curCompNm
  let o     = varName res
      o'    = nameOcc o
      ty    = varType res
  fHwty <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  let FilteredHWType hwty _ = fHwty
      oPortSupply = fmap t_output teM
  when (containsBiSignalIn hwty)
    (throw (ClashException sp ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
  output <- mkOutput (filterVoidPorts fHwty <$> oPortSupply) (o',hwty)
  case output of
    Just (ports, decls, pN) -> do
      let pO = repName pN o
          pOId = mkLocalId ty pO
          subst1 = extendInScopeId (extendIdSubst subst0 res (Var pOId)) pOId
      return (Just (ports,decls,pOId,subst1))
    _ -> return Nothing

-- | Same as idToPort, but
--    * Throws an error if the port is a composite type with a BiSignalIn
idToInPort :: Id -> NetlistMonad (Maybe (Identifier,HWType))
idToInPort var = do
  (_, sp) <- Lens.use curCompNm
  portM <- idToPort var
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty && not (isBiSignalIn hty))
        (throw (ClashException sp ($(curLoc) ++ "BiSignalIn currently cannot be part of a composite type when it's a function's argument") Nothing))
      return portM
    _ -> return Nothing

-- | Same as idToPort, but:
--    * Throws an error if port is of type BiSignalIn
idToOutPort :: Id -> NetlistMonad (Maybe (Identifier,HWType))
idToOutPort var = do
  (_, srcspan) <- Lens.use curCompNm
  portM <- idToPort var
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty)
        (throw (ClashException srcspan ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
      return portM
    _ -> return Nothing

idToPort :: Id -> NetlistMonad (Maybe (Identifier,HWType))
idToPort var = do
  let i  = varName var
      ty = varType var
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
  if isVoid hwTy
    then return Nothing
    else return (Just (nameOcc i, hwTy))

id2type :: Id -> Type
id2type = varType

id2identifier :: Id -> Identifier
id2identifier = nameOcc . varName

repName :: Text -> Name a -> Name a
repName s (Name sort' _ i loc) = Name sort' s i loc

-- | Make a set of IDs unique; also returns a substitution from old ID to new
-- updated unique ID.
mkUnique
  :: Subst
  -- ^ Existing substitution
  -> [Id]
  -- ^ IDs to make unique
  -> NetlistMonad ([Id],Subst)
  -- ^ (Unique IDs, update substitution)
mkUnique = go []
  where
    go :: [Id] -> Subst -> [Id] -> NetlistMonad ([Id],Subst)
    go processed subst []     = return (reverse processed,subst)
    go processed subst@(Subst isN _ _ _) (i:is) = do
      iN <- mkUniqueIdentifier Extended (id2identifier i)
      let i' = uniqAway isN (modifyVarName (repName iN) i)
          subst' = extendInScopeId (extendIdSubst subst i (Var i')) i'
      go (i':processed)
         subst'
         is

mkUniqueIdentifier
  :: IdType
  -> Identifier
  -> NetlistMonad Identifier
mkUniqueIdentifier typ nm = do
  seen  <- Lens.use seenIds
  seenC <- Lens.use seenComps
  i     <- mkIdentifier typ nm
  let getCopyIter k = getFirst (First (HashMap.lookup k seen) <> First (HashMap.lookup k seenC))
  case getCopyIter i of
    Just n -> go n getCopyIter i
    Nothing -> do
      seenIds %= HashMap.insert i 0
      return i
 where
  go :: Word -> (Identifier -> Maybe Word) -> Identifier -> NetlistMonad Identifier
  go n g i = do
    i'  <- extendIdentifier typ i (Text.pack ('_':show n))
    case g i' of
      Just _  -> go (n+1) g i
      Nothing -> do
        seenIds %= HashMap.insert i (n+1)
        -- Don't forget to add the extended ID to the list of seen identifiers,
        -- in case we want to create a new identifier based on the extended ID
        -- we return in this function
        seenIds %= HashMap.insert i' 0
        return i'

-- | Preserve the complete state before running an action, and restore it
-- afterwards.
preserveState
  :: NetlistMonad a
  -> NetlistMonad a
preserveState action = do
  state <- State.get
  val <- action
  State.put state
  pure val

-- | Preserve the Netlist '_varCount','_curCompNm','_seenIds' when executing
-- a monadic action
preserveVarEnv
  :: NetlistMonad a
  -> NetlistMonad a
preserveVarEnv action = do
  -- store state
  vCnt  <- Lens.use varCount
  vComp <- Lens.use curCompNm
  vSeen <- Lens.use seenIds
  -- perform action
  val <- action
  -- restore state
  varCount  .= vCnt
  curCompNm .= vComp
  seenIds   .= vSeen
  return val

dcToLiteral :: HWType -> Int -> Literal
dcToLiteral Bool 1 = BoolLit False
dcToLiteral Bool 2 = BoolLit True
dcToLiteral _ i    = NumLit (toInteger i-1)

-- * TopEntity Annotations

extendPorts :: [PortName] -> [Maybe PortName]
extendPorts ps = map Just ps ++ repeat Nothing

portName
  :: String
  -> Identifier
  -> Identifier
portName [] i = i
portName x  _ = Text.pack x

-- | Prefix given string before portnames /except/ when this string is empty.
prefixParent :: String -> PortName -> PortName
prefixParent ""     p                   = p
prefixParent parent (PortName p)        = PortName (parent <> "_" <> p)
prefixParent parent (PortProduct "" ps) = PortProduct parent ps
prefixParent parent (PortProduct p ps)  = PortProduct (parent <> "_" <> p) ps

mkAssign :: Identifier -> HWType -> Expr -> [Declaration]
mkAssign id_ hwty expr = [NetDecl Nothing id_ hwty, Assignment id_ expr]

convPrimitiveType :: HWType -> a -> NetlistMonad a -> NetlistMonad a
convPrimitiveType hwty a action = do
  b <- Lens.use backend
  let kind = case b of {SomeBackend s -> State.evalState (hdlHWTypeKind hwty) s}
  case kind of
    UserType -> action
    SynonymType -> pure a
    PrimitiveType -> pure a

toPrimitiveType
  :: Identifier
  -> HWType
  -> NetlistMonad ([Declaration], Identifier, Expr, HWType)
toPrimitiveType id0 hwty0 = convPrimitiveType hwty0 dflt $ do
  id1 <- mkUniqueIdentifier Basic id0
  pure (mkAssign id1 hwty1 expr, id1, expr, hwty1)
 where
  dflt = ([], id0, Identifier id0 Nothing, hwty0)
  hwty1 = BitVector (typeSize hwty0)
  expr = ToBv Nothing hwty0 (Identifier id0 Nothing)

fromPrimitiveType
  :: Identifier
  -> HWType
  -> NetlistMonad ([Declaration], Identifier, Expr, HWType)
fromPrimitiveType id0 hwty0 = convPrimitiveType hwty0 dflt $ do
  id1 <- mkUniqueIdentifier Basic id0
  pure (mkAssign id1 hwty0 expr, id1, expr, hwty1)
 where
  dflt = ([], id0, Identifier id0 Nothing, hwty0)
  hwty1 = BitVector (typeSize hwty0)
  expr = FromBv Nothing hwty0 (Identifier id0 Nothing)

appendIdentifier
  :: (Identifier,HWType)
  -> Int
  -> NetlistMonad (Identifier,HWType)
appendIdentifier (nm,hwty) i =
  (,hwty) <$> extendIdentifier Extended nm (Text.pack ('_':show i))

-- | In addition to the original port name (where the user should assert that
-- it's a valid identifier), we also add the version of the port name that has
-- gone through the 'mkIdentifier Basic' process. Why? so that the provided port
-- name is copied verbatim into the generated HDL, but that in e.g.
-- case-insensitive HDLs, a case-variant of the port name is not used as one
-- of the signal names.
uniquePortName
  :: String
  -> Identifier
  -> NetlistMonad Identifier
uniquePortName [] i = mkUniqueIdentifier Extended i
uniquePortName x  _ = do
  let xT = Text.pack x
  xTB <- mkIdentifier Basic xT
  seenIds %= (\s -> List.foldl' (\m k -> HashMap.insert k 0 m) s [xT,xTB])
  return xT

mkInput
  :: Maybe PortName
  -> (Identifier,HWType)
  -> NetlistMonad ([(Identifier,HWType)],[Declaration],Expr,Identifier)
mkInput pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    -- No PortName given, infer names
    go
      :: (Identifier, HWType)
      -> NetlistMonad ([(Identifier, HWType)], [Declaration], Expr, Identifier)
    go (i,hwty) = do
      i' <- mkUniqueIdentifier Extended i
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..sz-1]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let netdecl  = NetDecl Nothing i' (Vector sz hwty'')
              vecExpr  = mkVectorChain sz hwty'' exprs
              netassgn = Assignment i' vecExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],vecExpr,i')
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..2^d-1]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let netdecl  = NetDecl Nothing i' (RTree d hwty'')
              trExpr   = mkRTreeChain d hwty'' exprs
              netassgn = Assignment i' trExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],trExpr,i')
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (i',) hwtys) [0..]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let netdecl  = NetDecl Nothing i' hwty
              dcExpr   = DataCon hwty (DC (hwty,0)) exprs
              netassgn = Assignment i' dcExpr

          if null attrs then
            return (concat ports,[netdecl,netassgn],dcExpr,i')
          else
            throwAnnotatedSplitError $(curLoc) "Product"

        _ -> do
          (decls, i'', expr, hwty1) <- fromPrimitiveType i' hwty
          return ([(i', hwty1)], decls, expr, i'')

    -- PortName specified by user
    go'
      :: PortName
      -> (Identifier, HWType)
      -> NetlistMonad ([(Identifier, HWType)], [Declaration], Expr, Identifier)
    go' (PortName p) (i0, hwty0) = do
      pN <- uniquePortName p i0 -- << Seems silly, remove?
      (decls, i1, expr, hwty1) <- fromPrimitiveType pN hwty0
      return ([(pN, hwty1)], decls, expr, i1)

    go' (PortProduct p ps) (i,hwty) = do
      pN <- uniquePortName p i
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (pN,hwty'')) [0..sz-1]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts $ map (prefixParent p) ps) arguments
          let netdecl  = NetDecl Nothing pN (Vector sz hwty'')
              vecExpr  = mkVectorChain sz hwty'' exprs
              netassgn = Assignment pN vecExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],vecExpr,pN)
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (pN,hwty'')) [0..2^d-1]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts $ map (prefixParent p) ps) arguments
          let netdecl  = NetDecl Nothing pN (RTree d hwty'')
              trExpr   = mkRTreeChain d hwty'' exprs
              netassgn = Assignment pN trExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],trExpr,pN)
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          let ps'            = extendPorts $ map (prefixParent p) ps
          (ports,_,exprs,_) <- unzip4 <$> uncurry (zipWithM mkInput) (ps', arguments)
          case exprs of
            [expr] ->
                 let netdecl  = NetDecl Nothing pN hwty'
                     dcExpr   = expr
                     netassgn = Assignment pN expr
                 in  return (concat ports,[netdecl,netassgn],dcExpr,pN)
            _ -> let netdecl  = NetDecl Nothing pN hwty'
                     dcExpr   = DataCon hwty' (DC (hwty',0)) exprs
                     netassgn = Assignment pN dcExpr
                 in  if null attrs then
                       return (concat ports,[netdecl,netassgn],dcExpr,pN)
                     else
                       throwAnnotatedSplitError $(curLoc) "Product"

        SP _ ((concat . map snd) -> [elTy]) -> do
          let hwtys = [BitVector (conSize hwty'),elTy]
          arguments <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          let ps'            = extendPorts $ map (prefixParent p) ps
          (ports,_,exprs,_) <- unzip4 <$> uncurry (zipWithM mkInput) (ps', arguments)
          case exprs of
            [conExpr,elExpr] -> do
              let netdecl  = NetDecl Nothing pN hwty'
                  dcExpr   = DataCon hwty' (DC (BitVector (typeSize hwty'),0))
                              [conExpr, ToBv Nothing elTy elExpr]
                  netassgn = Assignment pN dcExpr
              return (concat ports,[netdecl,netassgn],dcExpr,pN)
            _ -> error "Unexpected error for PortProduct"

        _ ->
          portProductError $(curLoc) hwty' (PortProduct p ps)

portProductError :: String -> HWType -> PortName -> a
portProductError loc hwty portProduct = error $ loc ++ [I.i|
  #{loc}PortProduct used, but did not see Vector, RTree, or Product. Saw the
  following instead:

    #{hwty}

  PortProduct used:

    #{portProduct}

  Note that the PortProduct as shown above might is only indicative, and might
  not correspond exactly to the one given in the Clash design. |]

-- | Create a Vector chain for a list of 'Identifier's
mkVectorChain :: Int
              -> HWType
              -> [Expr]
              -> Expr
mkVectorChain _ elTy []      = DataCon (Vector 0 elTy) VecAppend []
mkVectorChain _ elTy [e]     = DataCon (Vector 1 elTy) VecAppend
                                [e]
mkVectorChain sz elTy (e:es) = DataCon (Vector sz elTy) VecAppend
                                [ e
                                , mkVectorChain (sz-1) elTy es
                                ]

-- | Create a RTree chain for a list of 'Identifier's
mkRTreeChain :: Int
             -> HWType
             -> [Expr]
             -> Expr
mkRTreeChain _ elTy [e] = DataCon (RTree 0 elTy) RTreeAppend
                                  [e]
mkRTreeChain d elTy es =
  let (esL,esR) = splitAt (length es `div` 2) es
  in  DataCon (RTree d elTy) RTreeAppend
        [ mkRTreeChain (d-1) elTy esL
        , mkRTreeChain (d-1) elTy esR
        ]

genComponentName
  :: Bool
  -> HashMap Identifier Word
  -> (IdType -> Identifier -> Identifier)
  -> ComponentPrefix
  -> Id
  -> Identifier
genComponentName newInlineStrat seen mkIdFn prefixM nm =
  let nm' = Text.splitOn (Text.pack ".") (nameOcc (varName nm))
      fn  = mkIdFn Basic (stripDollarPrefixes (last nm'))
      fn' = if Text.null fn then Text.pack "Component" else fn
      prefix = maybe id (:) (componentPrefixOther prefixM) (if newInlineStrat then [] else init nm')
      nm2 = Text.concat (intersperse (Text.pack "_") (prefix ++ [fn']))
      nm3 = mkIdFn Basic nm2
  in  case HashMap.lookup nm3 seen of
        Just n  -> go n nm3
        Nothing -> nm3
  where
    go :: Word -> Identifier -> Identifier
    go n i =
      let i' = mkIdFn Basic (i `Text.append` Text.pack ('_':show n))
      in  case HashMap.lookup i' seen of
             Just _  -> go (n+1) i
             Nothing -> i'

genTopComponentName
  :: Bool
  -> (IdType -> Identifier -> Identifier)
  -> ComponentPrefix
  -> Maybe TopEntity
  -> Id
  -> Identifier
genTopComponentName _newInlineStrat _mkIdFn prefixM (Just ann) _nm =
  case componentPrefixTop prefixM of
    Just p -> p `Text.append` Text.pack ('_':t_name ann)
    _      -> Text.pack (t_name ann)
genTopComponentName newInlineStrat mkIdFn prefixM Nothing nm =
  genComponentName newInlineStrat HashMap.empty mkIdFn prefixM' nm
 where
   -- use the prefix for top-level components
   prefixM' = prefixM{componentPrefixOther = componentPrefixTop prefixM}


-- | Strips one or more layers of attributes from a HWType; stops at first
-- non-Annotated. Accumilates all attributes of nested annotations.
stripAttributes
  :: HWType
  -> ([Attr'], HWType)
-- Recursively strip type, accumulate attrs:
stripAttributes (Annotated attrs typ) =
  let (attrs', typ') = stripAttributes typ
  in (attrs ++ attrs', typ')
-- Not an annotated type, so just return it:
stripAttributes typ = ([], typ)

-- | Generate output port mappings
mkOutput
  :: Maybe PortName
  -> (Identifier,HWType)
  -> NetlistMonad (Maybe ([(Identifier,HWType)],[Declaration],Identifier))
mkOutput _pM (_o, (BiDirectional Out _)) = return Nothing
mkOutput _pM (_o, (Void _))  = return Nothing
mkOutput pM  (o,  hwty)      = Just <$> mkOutput' pM (o, hwty)

-- | Generate output port mappings. Will yield Nothing if the only output is
-- Void.
mkOutput'
  :: Maybe PortName
  -> (Identifier,HWType)
  -> NetlistMonad ([(Identifier,HWType)],[Declaration],Identifier)
mkOutput' pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    go
      :: (Identifier, HWType)
      -> NetlistMonad ([(Identifier, HWType)], [Declaration], Identifier)
    go (o,hwty) = do
      o' <- mkUniqueIdentifier Extended o
      let (attrs, hwty') = stripAttributes hwty
          oDecl = NetDecl Nothing o' hwty
      case hwty' of
        Vector sz hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "Vector")
          results <- mapM (appendIdentifier (o',hwty'')) [0..sz-1]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput' Nothing) results
          let assigns = zipWith (assignId o' hwty' 10) ids [0..]
          return (concat ports,oDecl:assigns ++ concat decls,o')

        RTree d hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "RTree")
          results <- mapM (appendIdentifier (o',hwty'')) [0..2^d-1]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput' Nothing) results
          let assigns = zipWith (assignId o' hwty' 10) ids [0..]
          return (concat ports,oDecl:assigns ++ concat decls,o')

        Product _ _ hwtys -> do
          results <- zipWithM appendIdentifier (map (o,) hwtys) [0..]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput' Nothing) results
          case ids of
            [i] ->
              let assign  = Assignment i (Identifier o' Nothing)
              in  return (concat ports,oDecl:assign:concat decls,o')
            _   ->
              let assigns = zipWith (assignId o' hwty 0) ids [0..]
              in  if null attrs then
                     return (concat ports,oDecl:assigns ++ concat decls,o')
                  else
                    throwAnnotatedSplitError $(curLoc) "Product"

        _ -> do
          (_, o'', bvExpr, hwty1) <- toPrimitiveType o' hwty
          if hwty == hwty1 then
            -- No type conversion happened, so we can request caller to assign
            -- to port directly
            return ([(o', hwty)], [], o')
          else
            -- Type conversion happened, so we need to use intermediate variable
            return ([(o'', hwty1)], [oDecl, Assignment o'' bvExpr], o')

    go'
      :: PortName
      -> (Identifier, HWType)
      -> NetlistMonad ([(Identifier, HWType)], [Declaration], Identifier)
    go' (PortName p) (o0, hwty0) = do
      o1 <- mkUniqueIdentifier Extended o0
      pN <- uniquePortName p o0
      (_, _, bvExpr, hwty1) <- toPrimitiveType o1 hwty0
      if hwty0 == hwty1 then
        -- No type conversion happened, so we can just request caller to assign
        -- to port name directly
        return ([(pN, hwty0)], [], pN)
      else
        -- Type conversion happened, so we must use intermediate variable.
        return ( [(pN, hwty1)]
               , [Assignment pN bvExpr, NetDecl Nothing o1 hwty0]
               , o1
               )

    go' (PortProduct p ps) (_,hwty) = do
      pN <- mkUniqueIdentifier Basic (Text.pack p)
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "Vector")
          results <- mapM (appendIdentifier (pN,hwty'')) [0..sz-1]
          (ports,decls,ids) <- unzip3 <$> zipWithM mkOutput' (extendPorts $ map (prefixParent p) ps) results
          let netdecl = NetDecl Nothing pN hwty'
              assigns = zipWith (assignId pN hwty' 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,pN)

        RTree d hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "RTree")
          results <- mapM (appendIdentifier (pN,hwty'')) [0..2^d-1]
          (ports,decls,ids) <- unzip3 <$> zipWithM mkOutput' (extendPorts $ map (prefixParent p) ps) results
          let netdecl = NetDecl Nothing pN hwty'
              assigns = zipWith (assignId pN hwty' 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,pN)

        Product _ _ hwtys -> do
          results <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          let ps'            = extendPorts $ map (prefixParent p) ps
          (ports,decls,ids) <- unzip3 <$> uncurry (zipWithM mkOutput') (ps', results)
          let netdecl = NetDecl Nothing pN hwty'
          case ids of
            [i] -> let assign  = Assignment i (Identifier pN Nothing)
                   in  return (concat ports,netdecl:assign:concat decls,pN)

            _   -> let assigns = zipWith (assignId pN hwty' 0) ids [0..]
                   in  if null attrs then
                         return (concat ports,netdecl:assigns ++ concat decls,pN)
                       else
                         throwAnnotatedSplitError $(curLoc) "Product"

        SP _ ((concat . map snd) -> [elTy]) -> do
          let hwtys = [BitVector (conSize hwty'),elTy]
          results <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          let ps'            = extendPorts $ map (prefixParent p) ps
          (ports,decls,ids) <- unzip3 <$> uncurry (zipWithM mkOutput') (ps', results)
          case ids of
            [conId,elId] ->
              let netdecl = NetDecl Nothing pN hwty'
                  conIx   = Sliced (BitVector (typeSize hwty')
                                    ,typeSize hwty' - 1
                                    ,typeSize elTy
                                    )
                  elIx    = Sliced (BitVector (typeSize hwty')
                                    ,typeSize elTy - 1
                                    ,0
                                    )
                  assigns =
                    [ Assignment conId (Identifier pN (Just conIx))
                    , Assignment elId  (FromBv Nothing elTy (Identifier pN (Just elIx))) ]
              in  return (concat ports,netdecl:assigns ++ concat decls,pN)
            _ -> error "Unexpected error for PortProduct"

        _ ->
          portProductError $(curLoc) hwty' (PortProduct p ps)

    assignId p hwty con i n =
      Assignment i (Identifier p (Just (Indexed (hwty,con,n))))

mkTopCompDecl
  :: Maybe Comment
  -- ^ Comment to add to the generated code
  -> Identifier
  -- ^ The component's (or entity's) name
  -> Identifier
  -- ^ Instance label
  -> [(Expr, HWType, Expr)]
  -- ^ List of parameters for this component (param name, param type, param value)
  -> [InstancePort]
  -- ^ Input port assignments
  -> [InstancePort]
  -- ^ Output port assignments
  -> Declaration
mkTopCompDecl comment name instName params inputs outputs =
  if all (isJust . ip_port) (inputs <> outputs) then
    let toPort dir ip = ( Identifier (fromJust (ip_port ip)) Nothing
                        , dir, ip_type ip, toExpr (ip_id ip) ) in
    commonDecl (NamedPortMap (map (toPort In) inputs ++ map (toPort Out) outputs))
  else
    let toPort dir ip = (dir, ip_type ip, toExpr (ip_id ip)) in
    commonDecl (IndexedPortMap (map (toPort In) inputs ++ map (toPort Out) outputs))
 where
  commonDecl = InstDecl Entity comment name instName params
  toExpr id_ = Identifier id_ Nothing

-- | Instantiate a TopEntity, and add the proper type-conversions where needed
mkTopUnWrapper
  :: Id
  -- ^ Name of the TopEntity component
  -> Maybe TopEntity
  -- ^ (maybe) a corresponding @TopEntity@ annotation
  -> (Identifier,HWType)
  -- ^ The name and type of the signal to which to assign the result
  -> [(Expr,HWType)]
  -- ^ The arguments
  -> [Declaration]
  -- ^ Tick declarations
  -> NetlistMonad [Declaration]
mkTopUnWrapper topEntity annM dstId args tickDecls = do

  -- component name
  newInlineStrat <- opt_newInlineStrat <$> Lens.use clashOpts
  mkIdFn <- Lens.use mkIdentifierFn
  prefixM <- Lens.use componentPrefix
  let topName = genTopComponentName newInlineStrat mkIdFn prefixM annM topEntity

  -- inputs
  let iPortSupply = maybe (repeat Nothing) (extendPorts . t_inputs) annM
  arguments <- zipWithM appendIdentifier (map (\a -> ("input",snd a)) args) [0..]
  (iports, wrappers, idsI) <- unzip3 <$> zipWithM mkTopInput iPortSupply arguments
  let inpAssigns = zipWith Assignment idsI (map fst args)

  -- output
  let oPortSupply = t_output <$> annM

  let iResult = inpAssigns ++ concat wrappers
      result = ("result",snd dstId)

  instLabel0 <- extendIdentifier Basic topName ("_" `Text.append` fst dstId)
  instLabel1 <- fromMaybe instLabel0 <$> Lens.view setName
  instLabel2 <- affixName instLabel1
  instLabel3 <- mkUniqueIdentifier Basic instLabel2
  topOutputM <- mkTopOutput oPortSupply result

  let topDecl = mkTopCompDecl (Just topName) topName instLabel3 [] (concat iports)

  case topOutputM of
    Nothing ->
      pure (topDecl [] : iResult)
    Just (oports, unwrappers, id0) -> do
      let outpAssign = Assignment (fst dstId) (Identifier id0 Nothing)
      pure (iResult ++ tickDecls ++ (topDecl oports:unwrappers) ++ [outpAssign])

data InstancePort = InstancePort
  { ip_port :: Maybe Identifier
  -- ^ Port to assign 'ip_id' to. If not given, positional instances will be used
  , ip_id :: Identifier
  -- ^ Identifier to assign to 'ip_port'.
  , ip_type :: HWType
  -- ^ Type assigned to port (superfluous?)
  }

-- | Generate input port mappings for the TopEntity
mkTopInput
  :: Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this input
  -> (Identifier, HWType)
  -> NetlistMonad ([InstancePort], [Declaration], Identifier)
mkTopInput pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    -- No @PortName@
    go
      :: (Identifier, HWType)
      -> NetlistMonad ( [InstancePort], [Declaration], Identifier)
    go (i, hwty0) = do
      i' <- mkUniqueIdentifier Basic i
      let iDecl = NetDecl Nothing i' hwty0
          (attrs, hwty') = stripAttributes hwty0
          indexI constr n = Identifier i' (Just (Indexed (hwty0, constr, n)))

      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..sz-1]
          (ports, decls, ids) <- unzip3 <$> mapM go arguments
          let assigns = zipWith Assignment ids (map (indexI 10) [0..])
          if null attrs then
            return (concat ports, iDecl:assigns++concat decls, i')
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..2^d-1]
          (ports, decls, ids) <- unzip3 <$> mapM go arguments
          let assigns = zipWith Assignment ids (map (indexI 10) [0..])
          if null attrs then
            return (concat ports, iDecl:assigns++concat decls, i')
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (i,) hwtys) [0..]
          (ports, decls, ids) <- unzip3 <$> mapM go arguments
          let assigns = zipWith Assignment ids (map (indexI 0) [0..])
          if null attrs then
            return (concat ports, iDecl:assigns++concat decls, i')
          else
            throwAnnotatedSplitError $(curLoc) "Product"

        _ -> do
          (decls, i'', _bvExpr, hwty1) <- toPrimitiveType i' hwty0
          return ([InstancePort Nothing i'' hwty1], iDecl:decls, i')

    -- With a @PortName@
    go'
      :: PortName
      -> (Identifier, HWType)
      -> NetlistMonad ([InstancePort], [Declaration], Identifier)
    go' (PortName (Text.pack -> pN)) (_, hwty0) = do
      pN' <- mkUniqueIdentifier Basic pN
      (decls, pN'', _bvExpr, hwty1) <- toPrimitiveType pN' hwty0
      return ( [InstancePort (Just pN) pN'' hwty1]
             , NetDecl Nothing pN' hwty0 : decls
             , pN' )

    go' (PortProduct p ps0) (i, hwty) = do
      let pN = portName p i
      pN' <- mkUniqueIdentifier Extended pN
      let pDecl = NetDecl Nothing pN' hwty
          (attrs, hwty') = stripAttributes hwty
          indexPN constr n = Identifier pN' (Just (Indexed (hwty, constr, n)))
          ps1 = extendPorts (map (prefixParent p) ps0)
      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (pN',hwty'')) [0..sz-1]
          (ports, decls, ids) <- unzip3 <$> zipWithM mkTopInput ps1 arguments
          let assigns = zipWith Assignment ids (map (indexPN 10) [0..])
          if null attrs then
            return (concat ports, pDecl:assigns ++ concat decls, pN')
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (pN',hwty'')) [0..2^d-1]
          (ports, decls, ids) <- unzip3 <$> zipWithM mkTopInput ps1 arguments
          let assigns = zipWith Assignment ids (map (indexPN 10) [0..])
          if null attrs then
            return (concat ports, pDecl:assigns ++ concat decls, pN')
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (ports, decls, ids) <- unzip3 <$> zipWithM mkTopInput ps1 arguments
          let assigns = zipWith Assignment ids (map (indexPN 0) [0..])
          if null attrs then
            return (concat ports, pDecl:assigns ++ concat decls, pN')
          else
            throwAnnotatedSplitError $(curLoc) "Product"

        SP _ ((concat . map snd) -> [elTy]) -> do
          let hwtys = [BitVector (conSize hwty'),elTy]
          arguments <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (ports, decls, ids) <- unzip3 <$> zipWithM mkTopInput ps1 arguments
          case ids of
            [conId,elId] -> do
              let conIx   = Sliced (BitVector (typeSize hwty')
                                    ,typeSize hwty' - 1
                                    ,typeSize elTy
                                    )
                  elIx    = Sliced (BitVector (typeSize hwty')
                                    ,typeSize elTy - 1
                                    ,0
                                    )
                  assigns =
                    [ Assignment conId (Identifier pN (Just conIx))
                    , Assignment elId  (FromBv Nothing elTy (Identifier pN (Just elIx))) ]

              return (concat ports, pDecl:assigns ++ concat decls, pN')
            _ -> error "Internal error: Unexpected error for PortProduct"
        _ ->
          portProductError $(curLoc) hwty' (PortProduct p ps0)


-- | Consider the following type signature:
--
-- @
--   f :: Signal dom (Vec 6 A) \`Annotate\` Attr "keep"
--     -> Signal dom (Vec 6 B)
-- @
--
-- What does the annotation mean, considering that Clash will split these
-- vectors into multiple in- and output ports? Should we apply the annotation
-- to all individual ports? How would we handle pin mappings? For now, we simply
-- throw an error. This is a helper function to do so.
throwAnnotatedSplitError
  :: String
  -> String
  -> NetlistMonad a
throwAnnotatedSplitError loc typ = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException sp (loc ++ printf msg typ typ) Nothing
 where
  msg = unwords $ [ "Attempted to split %s into a number of HDL ports. This"
                  , "is not allowed in combination with attribute annotations."
                  , "You can annotate %s's components by splitting it up"
                  , "manually." ]

-- | Generate output port mappings for the TopEntity. Yields /Nothing/ if
-- the output is Void
mkTopOutput
  :: Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this output
  -> (Identifier, HWType)
  -> NetlistMonad (Maybe ([InstancePort], [Declaration], Identifier))
mkTopOutput _pM (_id, BiDirectional Out _) = return Nothing
mkTopOutput _pM (_id, Void _) = return Nothing
mkTopOutput pM (o, hwty) = Just <$> mkTopOutput' pM (o, hwty)

-- | Generate output port mappings for the TopEntity
mkTopOutput'
  :: Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this output
  -> (Identifier, HWType)
  -> NetlistMonad ([InstancePort], [Declaration], Identifier)
mkTopOutput' pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    -- No @PortName@
    go
      :: (Identifier, HWType)
      -> NetlistMonad ([InstancePort], [Declaration], Identifier)
    go (o, hwty) = do
      o' <- mkUniqueIdentifier Extended o
      let oDecl = NetDecl Nothing o' hwty
          (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          results <- mapM (appendIdentifier (o',hwty'')) [0..sz-1]
          (ports, decls, ids0) <- unzip3 <$> mapM go results
          let ids1 = map (flip Identifier Nothing) ids0
              netassgn = Assignment o' (mkVectorChain sz hwty'' ids1)
          if null attrs then
            return (concat ports, oDecl:netassgn:concat decls, o')
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          results <- mapM (appendIdentifier (o',hwty'')) [0..2^d-1]
          (ports, decls, ids0) <- unzip3 <$> mapM go results
          let ids1 = map (flip Identifier Nothing) ids0
              netassgn = Assignment o' (mkRTreeChain d hwty'' ids1)
          if null attrs then
            return (concat ports, oDecl:netassgn:concat decls, o')
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ _ hwtys -> do
          results <- zipWithM appendIdentifier (map (o',) hwtys) [0..]
          (ports, decls, ids0) <- unzip3 <$> mapM go results
          let ids1 = map (flip Identifier Nothing) ids0
              netassgn = Assignment o' (DataCon hwty (DC (hwty,0)) ids1)
          if null attrs then
            return (concat ports, oDecl:netassgn:concat decls, o')
          else
            throwAnnotatedSplitError $(curLoc) "Product"

        _ -> do
          (decls, o'', _expr, hwty1) <- fromPrimitiveType o' hwty
          return ([InstancePort Nothing o' hwty1], oDecl:decls, o'')

    -- With a @PortName@
    go'
      :: PortName
      -> (Identifier, HWType)
      -> NetlistMonad ([InstancePort], [Declaration], Identifier)
    go' (PortName (Text.pack -> p)) (oN, hwty0) = do
      oN' <- mkUniqueIdentifier Extended oN
      (decls, o1, _expr, hwty1) <- fromPrimitiveType oN' hwty0
      return ( [InstancePort (Just p) oN' hwty1]
             , NetDecl Nothing oN' hwty1 : decls
             , o1 )

    go' (PortProduct p ps0) (o, hwty) = do
      let pN = portName p o
      pN' <- mkUniqueIdentifier Extended pN
      let pDecl = NetDecl Nothing pN' hwty
          (attrs, hwty') = stripAttributes hwty
          ps1 = extendPorts (map (prefixParent p) ps0)
      case hwty' of
        Vector sz hwty'' -> do
          results <- mapM (appendIdentifier (pN',hwty'')) [0..sz-1]
          (ports, decls, ids0) <- unzip3 <$> zipWithM mkTopOutput' ps1 results
          let ids1 = map (flip Identifier Nothing) ids0
              netassgn = Assignment pN' (mkVectorChain sz hwty'' ids1)
          if null attrs then
            return (concat ports, pDecl:netassgn:concat decls, pN')
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          results <- mapM (appendIdentifier (pN',hwty'')) [0..2^d-1]
          (ports, decls, ids0) <- unzip3 <$> zipWithM mkTopOutput' ps1 results
          let ids1 = map (flip Identifier Nothing) ids0
              netassgn = Assignment pN' (mkRTreeChain d hwty'' ids1)
          if null attrs then
            return (concat ports, pDecl:netassgn:concat decls, pN')
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ _ hwtys -> do
          results <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (ports, decls, ids0) <- unzip3 <$> zipWithM mkTopOutput' ps1 results
          let ids1 = map (flip Identifier Nothing) ids0
              netassgn = Assignment pN' (DataCon hwty (DC (hwty,0)) ids1)
          if null attrs then
            return (concat ports, pDecl:netassgn:concat decls, pN')
          else
            throwAnnotatedSplitError $(curLoc) "Product"


        SP _ ((concat . map snd) -> [elTy]) -> do
          let hwtys = [BitVector (conSize elTy),elTy]
          results <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (ports, decls, ids0) <- unzip3 <$> zipWithM mkTopOutput' ps1 results
          let ids1 = map (flip Identifier Nothing) ids0
              ids2 = case ids1 of
                      [conId, elId] -> [conId, ToBv Nothing elTy elId]
                      _ -> error "Unexpected error for PortProduct"
              netassgn = Assignment pN' (DataCon hwty (DC (BitVector (typeSize hwty),0)) ids2)
          return (concat ports, pDecl:netassgn:concat decls, pN')

        _ ->
          portProductError $(curLoc) hwty' (PortProduct p ps0)

-- | Try to merge nested modifiers into a single modifier, needed by the VHDL
-- and SystemVerilog backend.
nestM :: Modifier -> Modifier -> Maybe Modifier
nestM (Nested a b) m2
  | Just m1  <- nestM a b  = maybe (Just (Nested m1 m2)) Just (nestM m1 m2)
  | Just m2' <- nestM b m2 = maybe (Just (Nested a m2')) Just (nestM a m2')

nestM (Indexed (Vector n t1,1,1)) (Indexed (Vector _ t2,1,0))
  | t1 == t2 = Just (Indexed (Vector n t1,10,1))

nestM (Indexed (Vector n t1,1,1)) (Indexed (Vector _ t2,10,k))
  | t1 == t2 = Just (Indexed (Vector n t1,10,k+1))

nestM (Indexed (RTree d1 t1,1,n)) (Indexed (RTree d2 t2,0,0))
  | t1 == t2
  , d1 >= 0
  , d2 >= 0
  = Just (Indexed (RTree d1 t1,10,n))

nestM (Indexed (RTree d1 t1,1,n)) (Indexed (RTree d2 t2,1,m))
  | t1 == t2
  , d1 >= 0
  , d2 >= 0
  = if | n == 1 && m == 1 -> let r = 2 ^ d1
                                 l = r - (2 ^ (d1-1) `div` 2)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 1 && m == 0 -> let l = 2 ^ (d1-1)
                                 r = l + (l `div` 2)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 0 && m == 1 -> let l = (2 ^ (d1-1)) `div` 2
                                 r = 2 ^ (d1-1)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 0 && m == 0 -> let l = 0
                                 r = (2 ^ (d1-1)) `div` 2
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n > 1 || n < 0   -> error $ "nestM: n should be 0 or 1, not:" ++ show n
       | m > 1 || m < 0   -> error $ "nestM: m should be 0 or 1, not:" ++ show m
       | otherwise        -> error $ "nestM: unexpected (n, m): " ++ show (n, m)
nestM (Indexed (RTree (-1) t1,l,_)) (Indexed (RTree d t2,10,k))
  | t1 == t2
  , d  >= 0
  = Just (Indexed (RTree d t1,10,l+k))

nestM _ _ = Nothing


-- | Determines if any type variables (exts) are bound in any of the given
-- type or term variables (tms). It's currently only used to detect bound
-- existentials, hence the name.
bindsExistentials
  :: [TyVar]
  -> [Var a]
  -> Bool
bindsExistentials exts tms = any (`elem` freeVars) exts
 where
  freeVars = concatMap (Lens.toListOf typeFreeVars) (map varType tms)

iteAlts :: HWType -> [Alt] -> Maybe (Term,Term)
iteAlts sHTy [(pat0,alt0),(pat1,alt1)] | validIteSTy sHTy = case pat0 of
  DataPat dc _ _ -> case dcTag dc of
    2 -> Just (alt0,alt1)
    _ -> Just (alt1,alt0)
  LitPat (C.IntegerLiteral l) -> case l of
    1 -> Just (alt0,alt1)
    _ -> Just (alt1,alt0)
  DefaultPat -> case pat1 of
    DataPat dc _ _ -> case dcTag dc of
      2 -> Just (alt1,alt0)
      _ -> Just (alt0,alt1)
    LitPat (C.IntegerLiteral l) -> case l of
      1 -> Just (alt1,alt0)
      _ -> Just (alt0,alt1)
    _ -> Nothing
  _ -> Nothing
 where
  validIteSTy Bool          = True
  validIteSTy Bit           = True
  validIteSTy (Sum _ [_,_]) = True
  validIteSTy (SP _ [_,_])  = True
  validIteSTy (Unsigned 1)  = True
  validIteSTy (Index 2)     = True
  validIteSTy _             = False

iteAlts _ _ = Nothing

-- | Run a NetlistMonad computation in the context of the given source ticks and
-- name modifier ticks
withTicks
  :: [TickInfo]
  -> ([Declaration] -> NetlistMonad a)
  -- ^ The source ticks are turned into 'TickDecl's and are passed as an argument
  -- to the NetlistMonad computation. Name modifier ticks will change the local
  -- environment for the NetlistMonad computation.
  -> NetlistMonad a
withTicks ticks0 k = do
  let ticks1 = List.nub ticks0
  go [] (reverse ticks1)
 where
  go decls [] = k (reverse decls)

  go decls (DeDup:ticks) = go decls ticks

  go decls (NoDeDup:ticks) = go decls ticks

  go decls (SrcSpan sp:ticks) =
    go (TickDecl (Text.pack (showSDocUnsafe (ppr sp))):decls) ticks

  go decls (NameMod m nm0:ticks) = do
    tcm <- Lens.use tcCache
    case runExcept (tyLitShow tcm nm0) of
      Right nm1 -> local (modName m nm1) (go decls ticks)
      _ -> go decls ticks

  modName PrefixName (Text.pack -> s2) env@(NetlistEnv {_prefixName = s1})
    | Text.null s1 = env {_prefixName = s2}
    | otherwise    = env {_prefixName = s1 <> "_" <> s2}
  modName SuffixName (Text.pack -> s2) env@(NetlistEnv {_suffixName = s1})
    | Text.null s1 = env {_suffixName = s2}
    | otherwise    = env {_suffixName = s2 <> "_" <> s1}
  modName SuffixNameP (Text.pack -> s2) env@(NetlistEnv {_suffixName = s1})
    | Text.null s1 = env {_suffixName = s2}
    | otherwise    = env {_suffixName = s1 <> "_" <> s2}
  modName SetName (Text.pack -> s) env = env {_setName = Just s}

-- | Add the pre- and suffix names in the current environment to the given
-- identifier
affixName
  :: Identifier
  -> NetlistMonad Identifier
affixName nm0 = do
  NetlistEnv pre suf _ <- ask
  let nm1 = if Text.null pre then nm0 else pre <> "_" <> nm0
      nm2 = if Text.null suf then nm1 else nm1 <> "_" <> suf
  return nm2
