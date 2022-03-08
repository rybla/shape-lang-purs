module Language.Shape.Stlc.WrapChanges where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude hiding (mod)
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Undefined (undefined)
import Unsafe as Unsafe

{-
Question: Can a special way of doing wraps handle type changes?
-}
data Type
  = ArrType Type Type
  | BaseType
  | HoleType

data Term
  = LamTerm Name Term
  | LetTerm Let
  | NeuTerm Neu

type Let
  = Name /\ Type /\ Term /\ Term

data Neu
  = AppTerm App
  | HoleTerm

type App
  = Name /\ List Term

type Name
  = String

freshName :: Unit -> String
freshName _ = "x" -- TODO

{-
Goal: At each step of recursion, has access to necessary `Wrap` to incur type change.
-}
type Wrap a
  = a -> Term

data Change
  = DigTypeChange Term
  | FillTypeChange (Type -> Term)
  | InsertTypeChange (Type -> Term)
  | DeleteTypeChange Term
  | FillTermChange (Term -> Term) -- TODO: make sure type is compatible
  | DigTermChange Term

data LetChange
  = DigTypeLetChange ((Type /\ Term) /\ (App -> Neu))
  | FillTypeLetChange (Type -> (Type /\ Term))
  | InsertTypeLetChange (Type -> ((Type /\ Term) /\ (App -> Neu)))
  | DeleteTypeLetChange ((Type /\ Term) /\ (App -> Neu))

{-
Once a `LetChange` has been built-up via `collectLetChanges`, it can be lifted to a `Change` by applying the definition component of a `LetChange` to the `LetTerm`'s definition, and then applying the modification `App -> Neu` component to each application of the definition's variable.
-}
applyLetChange :: Let -> Wrap Let -> LetChange -> Change
applyLetChange (x /\ alpha /\ a /\ b) wrap = case _ of
  DigTypeLetChange ((alpha' /\ a') /\ modApp) ->
    DigTypeChange
      (wrap (x /\ alpha' /\ a' /\ traverseApps x modApp b))
  FillTypeLetChange kCh ->
    FillTypeChange
      ( \alpha' ->
          let
            alpha'' /\ a'' = kCh alpha'
          in
            wrap (x /\ alpha'' /\ a'' /\ b)
      )
  InsertTypeLetChange kCh ->
    InsertTypeChange
      ( \alpha' ->
          let
            (alpha'' /\ a'') /\ modApp = kCh alpha'
          in
            wrap (x /\ alpha'' /\ a'' /\ traverseApps x modApp b)
      )
  DeleteTypeLetChange ((alpha' /\ a') /\ modApp) ->
    DeleteTypeChange
      (wrap (x /\ alpha' /\ a' /\ traverseApps x modApp b))

traverseApps :: Name -> (App -> Neu) -> Term -> Term
traverseApps x modApp = case _ of
  LamTerm y b -> LamTerm y (traverseApps x modApp b)
  LetTerm (y /\ alpha /\ a /\ b) -> LetTerm (y /\ alpha /\ traverseApps x modApp a /\ traverseApps x modApp b)
  NeuTerm (AppTerm (y /\ as)) ->
    if x == y then case modApp (y /\ as) of
      AppTerm (z /\ as') -> NeuTerm (AppTerm (z /\ (traverseApps x modApp <$> as')))
      HoleTerm -> NeuTerm HoleTerm
    else
      NeuTerm (AppTerm (y /\ (traverseApps x modApp <$> as)))
  NeuTerm HoleTerm -> NeuTerm HoleTerm

-- TODO: handle LetTerm in cases
collectLetChanges :: Let -> Wrap Let -> List Change
collectLetChanges let_@(x /\ typeTop /\ termTop /\ bodyTop) wrapTop =
  applyLetChange let_ wrapTop
    <$> goNeu (typeTop /\ termTop) identity identity 0
  where
  {-
  `def` is the type and term bound in a `LetTerm`.
  `wrapDef` is a wrap for a partial def into the top def.
  `wrapNeu` is a wrap for a neutral form that is an application of `x`.
  `iArg` is the current argument index iteration.
  The function `goNeu` recurses over the structures of the type and term of a definition, enumerating each possible change, where each change modifies the definition via `wrapDef` and modifies each application of `x` via `wrapNeu`.
  -}
  goNeu :: (Type /\ Term) -> ((Type /\ Term) -> (Type /\ Term)) -> (Neu -> Neu) -> Int -> List LetChange
  goNeu def@(type_ /\ term) wrapDef wrapNeu iArg = case type_ /\ term of
    HoleType /\ a ->
      List.fromFoldable
        [ FillTypeLetChange
            ( \alpha ->
                (wrapDef $ alpha /\ a)
            )
        , InsertTypeLetChange
            ( \alpha ->
                (wrapDef $ ArrType alpha HoleType /\ LamTerm (freshName unit) a)
                  /\ (wrapNeu <<< AppTerm <<< insertArg_App iArg)
            )
        ]
    BaseType /\ a ->
      List.fromFoldable
        [ InsertTypeLetChange
            ( \alpha ->
                (wrapDef $ ArrType alpha HoleType /\ LamTerm (freshName unit) a)
                  /\ (wrapNeu <<< AppTerm <<< insertArg_App iArg)
            )
        , DigTypeLetChange
            ( (wrapDef $ HoleType /\ NeuTerm HoleTerm)
                /\ ( \(f /\ args) ->
                      if iArg == List.length args - 1 then
                        wrapNeu HoleTerm
                      else
                        wrapNeu <<< AppTerm $ digArg_App iArg (f /\ args)
                  )
            )
        ]
    ArrType alpha beta /\ LamTerm x b ->
      List.fromFoldable
        [ InsertTypeLetChange
            ( \delta ->
                (wrapDef $ ArrType delta (ArrType alpha beta) /\ LamTerm (freshName unit) (LamTerm x b))
                  /\ (wrapNeu <<< AppTerm <<< insertArg_App iArg)
            )
        , DeleteTypeLetChange
            ( (wrapDef $ beta /\ digVar_Term x b)
                /\ (wrapNeu <<< AppTerm <<< deleteArg_App iArg)
            )
        , DigTypeLetChange
            ( (wrapDef $ HoleType /\ NeuTerm HoleTerm)
                /\ (wrapNeu <<< AppTerm <<< digArg_App iArg)
            )
        ] -- recursive steps
        <> goTerm alpha
            (\alpha' -> wrapDef $ alpha' /\ LamTerm x b)
            (\wrapTerm (f /\ as) -> wrapNeu $ AppTerm (f /\ (Unsafe.fromJust $ List.modifyAt iArg wrapTerm as)))
        <> goNeu
            (beta /\ b)
            (\(beta' /\ b') -> wrapDef $ ArrType alpha beta' /\ LamTerm x b')
            wrapNeu
            (iArg + 1)
    _ -> undefined

  {-
  `wrapType` is a wrap for a type into the definition's type.
  `wrapArg` is a wrap from a term-wrapper (modifying an argument) to an application-wrapper (wrapping the argument into an application).
  A higher-order change of a definition's type corresponds to a change inside an argument of each application of `x`. Note, however, that such a higher-order change does not modify the definition's term, so that is why `Term` is missing from `wrapType`.
  -}
  goTerm :: Type -> (Type -> (Type /\ Term)) -> ((Term -> Term) -> (App -> Neu)) -> List LetChange
  goTerm type_ wrapType wrapArg = case type_ of
    HoleType ->
      List.fromFoldable
        [ FillTypeLetChange wrapType
        , InsertTypeLetChange
            ( \alpha ->
                ( (wrapType $ ArrType alpha HoleType)
                    /\ (wrapArg $ \b -> LamTerm (freshName unit) b)
                )
            )
        ]
    BaseType ->
      List.fromFoldable
        [ DigTypeLetChange
            ( (wrapType HoleType)
                /\ (wrapArg $ \_ -> NeuTerm HoleTerm)
            )
        , InsertTypeLetChange
            ( \alpha ->
                ( (wrapType $ ArrType alpha type_)
                    /\ (wrapArg $ \b -> LamTerm (freshName unit) b)
                )
            )
        ]
    ArrType alpha beta ->
      List.fromFoldable
        [ DigTypeLetChange
            ( (wrapType HoleType)
                /\ (wrapArg $ \_ -> NeuTerm HoleTerm)
            )
        , InsertTypeLetChange
            ( \alpha ->
                ( (wrapType $ ArrType alpha type_)
                    /\ (wrapArg $ \b -> LamTerm (freshName unit) b)
                )
            )
        , DeleteTypeLetChange
            ( (wrapType beta)
                /\ ( wrapArg
                      $ case _ of
                          LamTerm x b -> digVar_Term x b
                          _ -> NeuTerm HoleTerm
                  )
            )
        ]

collectChangesTerm :: Term -> Wrap Term -> List Change
collectChangesTerm term wrap = case term of
  LamTerm x b -> (DigTermChange (wrap $ NeuTerm HoleTerm)) List.: collectChangesTerm b (\b' -> wrap $ LamTerm x b')
  LetTerm let_ -> (DigTermChange (wrap $ NeuTerm HoleTerm)) List.: collectLetChanges let_ (\let' -> LetTerm let')
  NeuTerm (AppTerm (f /\ as)) ->
    (DigTermChange (wrap $ NeuTerm HoleTerm))
      List.: (flatten $ List.mapWithIndex (\i a -> collectChangesTerm a (wrap <<< \a' -> NeuTerm (AppTerm (f /\ Unsafe.fromJust (List.updateAt i a' as))))) as)
    where
    flatten = List.foldMap identity
  NeuTerm HoleTerm -> List.singleton (FillTermChange wrap)

insertArg_App :: Int -> App -> App
insertArg_App i (f /\ args) = f /\ (Unsafe.fromJust $ List.insertAt i (NeuTerm HoleTerm) args)

deleteArg_App :: Int -> App -> App
deleteArg_App i (f /\ args) = f /\ (Unsafe.fromJust $ List.deleteAt i args)

digArg_App :: Int -> App -> App
digArg_App i (f /\ args) = f /\ (Unsafe.fromJust $ List.updateAt i (NeuTerm HoleTerm) args)

digVar_Term :: Name -> Term -> Term
digVar_Term x = case _ of
  LamTerm y b -> LamTerm y (digVar_Term x b)
  LetTerm (y /\ alpha /\ a /\ b) -> LetTerm (y /\ alpha /\ digVar_Term x a /\ digVar_Term x b)
  NeuTerm (AppTerm (y /\ as)) ->
    if x == y then
      NeuTerm HoleTerm
    else
      NeuTerm $ AppTerm (y /\ (digVar_Term x <$> as))
  NeuTerm HoleTerm -> NeuTerm HoleTerm
