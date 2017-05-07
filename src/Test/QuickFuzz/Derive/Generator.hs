{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.QuickFuzz.Derive.Generator (
    devGenerator
    ) where

import           Megadeth.Prim

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax as TH
import           Test.QuickCheck

import           Control.Monad
import           GHC.Types

import           Data.List
import qualified Data.Map.Strict            as M

#if MIN_VERSION_template_haskell(2,11,0)
#    define TH211MBKIND _maybe_kind
#else
#    define TH211MBKIND
#endif

type Functorial = ([Type],[Name])

headOf' :: Type -> Maybe Name
headOf' (AppT ListT ty) = Nothing
headOf' (AppT (TupleT _) ty) = Nothing
headOf' (AppT ArrowT e) = headOf' e
headOf' (AppT ty1 _) = headOf' ty1
headOf' (SigT ty _) = headOf' ty
headOf' (ConT n) = Just n
headOf' (VarT n) = Just n
headOf' e = error ("Missing :" ++ show e)

-- | Find all simple Types that are arguments of another Type.
findArgTypes :: Type -> [Type]
findArgTypes a@(AppT ListT _) = [a]
findArgTypes a@(AppT (TupleT n) _) = [a]
findArgTypes (AppT p@(ConT _) ty) = findArgTypes ty
findArgTypes (AppT ty1 ty2) = findArgTypes ty1 ++ findArgTypes ty2
findArgTypes a@(VarT _) = [a]
findArgTypes (ForallT _ _ ty) = findArgTypes ty
findArgTypes ArrowT = []
findArgTypes ListT = []
findArgTypes StarT = []
findArgTypes ty = [ty]

-- | Substitution
subst :: Type -> Name -> Type -> Maybe Type
subst (VarT v) n t | v == n = Just t
subst (AppT l r) n t =
    case (subst l n t, subst r n t) of
        (Just l', Just r') -> Just $ AppT l' r'
        (Just l', Nothing) -> Just $ AppT l' r
        (Nothing, Just r') -> Just $ AppT l r'
        _ -> Nothing
subst (ForallT a b ty) n t = subst ty n t >>= (Just . ForallT a b)
subst _ _ _ = Nothing

helpa :: Name -> Functorial
      -> Name -> M.Map Type Name -> Type -> ExpQ
helpa rs fr nm m (ConT t) = if nm == t then
                           varE rs
                         else error " Error in helper function "
helpa rs fr nm m (AppT l r) = appE (helpa rs fr nm m l) (varE $ lookUp m r)

lookUp :: M.Map Type Name -> Type -> Name
lookUp m t = M.findWithDefault (error $ "Not found " ++  show t) t m

pseudogetLeaf :: Type -> [Type]
pseudogetLeaf (AppT l r) = r : pseudogetLeaf l
pseudogetLeaf (ConT _) = []
pseudogetLeaf x = error $ "Missing " ++ show x

rep :: M.Map Name Type -> Type -> Type
rep m ListT = ListT
rep m (VarT x) = if M.member x m then m M.! x else VarT x
rep _ (ConT c) = ConT c
rep m (AppT l r) = AppT (rep m l) $ rep m r
rep _ t = error $ "Missing: " ++ show t

unif :: Functorial -> [Type] -> [Type]
unif f@(gargs, bnds) args =
  if length bnds /= length args
  then error $ "diff sizes?" ++ show f -- safety check, it should not happen
  else
    let mp = M.fromList $ zip bnds args
    in map (rep mp) gargs

checkDiff :: (Eq a) => Maybe a -> a -> Bool
checkDiff Nothing _ = True
checkDiff (Just x) y = x /= y
genGen' :: Name
        -> Functorial
        -> Name
        -> M.Map Type Name
        -> Type -> ExpQ -- Recursive case missing
genGen' rs fr nm m t =
                     if checkDiff (headOf' t) nm
                       then varE $ lookUp m t
                       else
                       let ins = reverse $ pseudogetLeaf t
                       in genRecGen $ unif fr ins
                     where
                       genRecGen :: [Type] -> ExpQ
                       genRecGen xs =
                         foldl appE (varE rs)
                         $ map (varE . lookUp m) xs

genGen :: Name -- Recursive name, given by the user
       -> Functorial -- Recursive functorial.
       -> Name -- Top type name.
       -> M.Map Type Name -- Environment
       -> [Type] -- Constructor argument types as in C_i [Type]
       -> ExpQ -- Result expression, genT_1 <*> genT_2 <*> genT_3 ...
genGen rs fr nm m ts = foldr1
             (\x xs -> uInfixE x (varE '(<*>)) xs)
             $ map (genGen' rs fr nm m) ts


recuded' :: Name -> Type -> (M.Map Type Name, [[Type]]) ->  Q ((M.Map Type Name) , [[Type]])
recuded' nm t (rs,tss) = if checkDiff (headOf' t) nm then
                          do
                            runIO $ print $ "Added: " ++ show t
                            n <- newName "gen"
                            return (M.insert t n rs,tss)
                   else
                        -- do  runIO $ print "Args?"
                        --     runIO $ print $ findArgTypes t
                            return (rs, (findArgTypes t) : tss)
                     -- foldM (\rs t -> recuded' nm t rs )
                     --       rs
                     --       (filter ((nm /=) . headOf) (findLeafTypes t))

-- Search for the types that are known and needed.
-- Each one of these types will be an argument, hence
-- this function also generate the names of those arguments.
recuded :: Name -> [Type] -> Q ((M.Map Type Name) , [[Type]])
recuded nm ts = foldM (\rs t -> recuded' nm t rs) (M.empty,[]) ts

devGenerator :: String -> Name -> Q [Dec]
devGenerator f nm = do
  inf <- reify nm
  case inf of
    TyConI (DataD _ _ params TH211MBKIND cons _ ) -> do
      let paramNm = paramNames params
          ns = map varT paramNm
          scons = map (simpleConView nm) cons
          needed = concat $ map tt scons
      let fnm = mkName f
        -- search for types
      (mapTN', tss) <- recuded nm needed
      let instan = map (zip paramNm) tss
      mapTN <- foldM (\m is ->
                    foldM (\m (v,t) -> do
                     let rs = (M.foldrWithKey (\k l res ->
                                    let r = subst k v t in
                                        case r of
                                            Nothing -> res
                                            Just r' -> r' : res)
                                    [] m)
                     foldM (\m t -> do
                            -- runIO $ print $ "Added: " ++ show t
                            n <- newName "gen"
                            return $ M.insert t n m) m rs ) m is
                ) mapTN' instan
        -- Debugging
      -- runIO $ print "Ya están"
      -- runIO $ print mapTN
      -- runIO $ print "Faltantes"
      -- runIO $ print tss
      -- runIO $ print "Instancias "
      -- runIO $ print mapTN'
        -- Debugging
      let gsPatt = map varP $ M.elems mapTN
      let funct = M.keys mapTN -- these are the types of the arguments.
      signature <- sigD fnm (forallT params (cxt [])
                             (foldr
                              (\x xs -> appT (appT arrowT
                                              (appT (conT ''Gen)
                                               $ return x)) xs)
                              (appT
                                (conT ''Gen)
                                $ applyTo (conT nm) ns)
                              funct
                             )
                            )
      let fnmFunc = (funct, paramNm)
      fb <- funD fnm [clause gsPatt (normalB $
                       [| oneof
                          $(listE $ map (\(SimpleCon cnm _ t) ->
                               if null t then
                                 appE (varE 'pure) (conE cnm)
                               else
                                 uInfixE (conE cnm) (varE '(<$>))
                                 $ genGen fnm fnmFunc nm mapTN t
                           ) scons)
                        |]
                                    ) []
                     ]
      return [signature, fb]
    _ -> error "Complex data constructors."
