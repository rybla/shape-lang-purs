module Language.Shape.Stlc.Recursor.Action where

import Data.Tuple.Nested
import Language.Shape.Stlc.Action
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Syntax.Metadata
import Language.Shape.Stlc.Transition
import Language.Shape.Stlc.Types
import Prelude
import Control.Monad.State (get)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Language.Shape.Stlc.Recursor.Index (Visit)
import Language.Shape.Stlc.Recursor.Metacontext as Rec
import Language.Shape.Stlc.Rendering.Utilities (maybeArray')
import Prim (Array, Record, Row, String)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R

bindMaybeEffectUnit :: forall a. Maybe a -> (a -> Effect Unit) -> Effect Unit
bindMaybeEffectUnit = case _ of
  Just a -> (a # _)
  Nothing -> const (pure unit)

infixr 5 bindMaybeEffectUnit as >>|=

-- | recType
type ArgsType r
  = Rec.ArgsType ( | r )

type ArgsArrowType r rType
  = Rec.ArgsArrowType ( actions :: Array Action | r ) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType ( actions :: Array Action | r ) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType ( actions :: Array Action | r ) rHoleId

recType ::
  forall r a.
  Lacks "type_" r =>
  { arrowType :: Record (ArgsArrowType r (ArgsType r)) -> a
  , dataType :: Record (ArgsDataType r (ArgsTypeId r)) -> a
  , holeType :: Record (ArgsHoleType r (ArgsHoleId r)) -> a
  } ->
  Record (ArgsType r) -> a
recType rec =
  Rec.recType
    { arrowType:
        \args ->
          rec.arrowType
            $ R.union
                { actions:
                    Array.concat
                      [ common (ArrowType args.arrowType) args
                      , [ unarrow { args } ]
                      , maybeArray' do
                          arrow <- case args.arrowType.cod of
                            ArrowType arrow -> Just arrow
                            _ -> Nothing
                          pure $ swaparrow { args, arrow }
                      ]
                }
                args
    , dataType:
        \args ->
          rec.dataType
            $ R.union { actions: common (DataType args.dataType) args <> [] }
                args
    , holeType:
        \args ->
          rec.holeType
            $ R.union { actions: common (HoleType args.holeType) args <> [] }
                args
    }
  where
  common :: forall r. Type -> { visit :: Visit | r } -> Array Action
  common type_ args =
    [ digtype
    ]

-- | recTerm
type ArgsTerm r
  = Rec.ArgsTerm ( | r )

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam ( actions :: Array Action | r ) rTermBind rTerm

type ArgsNeu r rTermId rArgItem
  = Rec.ArgsNeu ( actions :: Array Action | r ) rTermId rArgItem

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( actions :: Array Action | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( actions :: Array Action | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItem
  = Rec.ArgsData ( actions :: Array Action | r ) rTypeBind rTerm rSumItem

type ArgsMatch r rTypeId rTerm rCaseItem
  = Rec.ArgsMatch ( actions :: Array Action | r ) rTypeId rTerm rCaseItem

type ArgsHole r
  = Rec.ArgsHole ( actions :: Array Action | r )

recTerm ::
  forall r a.
  Lacks "term" r =>
  Lacks "alpha" r =>
  { lam :: Record (ArgsLam r (ArgsTermBind r) (ArgsTerm r)) -> a
  , neu :: Record (ArgsNeu r (ArgsTermId r) (ArgsArgItem r)) -> a
  , let_ :: Record (ArgsLet r (ArgsTermBind r) (ArgsType r) (ArgsTerm r)) -> a
  , buf :: Record (ArgsBuf r (ArgsType r) (ArgsTerm r)) -> a
  , data_ :: Record (ArgsData r (ArgsTypeBind r) (ArgsSumItem r) (ArgsTerm r)) -> a
  , match :: Record (ArgsMatch r (ArgsTypeId r) (ArgsTerm r) (ArgsCaseItem r)) -> a
  , hole :: Record (ArgsHole r) -> a
  } ->
  Record (ArgsTerm r) -> a
recTerm rec =
  Rec.recTerm
    { lam:
        \args ->
          rec.lam
            $ R.union
                { actions:
                    Array.concat
                      [ common (Lam args.lam) args
                      , [ unlambda { args } ]
                      , maybeArray' do
                          lam' <- case args.lam.body of
                            Lam lam' -> Just lam' -- the body is also a lambda
                            _ -> Nothing
                          pure $ swaplambdas { args, lam' }
                      ]
                }
                args
    , neu:
        \args ->
          rec.neu
            $ R.union
                { actions:
                    Array.concat
                      [ common (Neu args.neu) args
                      , [ app { args } ]
                      , [ unapp { args } ]
                      ]
                }
                args
    , let_:
        \args ->
          rec.let_
            $ R.union
                { actions:
                    Array.concat
                      [ common (Let args.let_) args
                      , [ unlet { args } ]
                      ]
                }
                args
    , buf:
        \args ->
          rec.buf
            $ R.union
                { actions:
                    Array.concat
                      [ common (Buf args.buf) args
                      , [ unbuffer { args } ]
                      ]
                }
                args
    , data_:
        \args ->
          rec.data_
            $ R.union
                { actions:
                    Array.concat
                      [ common (Data args.data_) args
                      , [ undata { args } ]
                      ]
                }
                args
    , match:
        \args ->
          rec.match
            $ R.union
                { actions:
                    Array.concat
                      [ common (Match args.match) args
                      ]
                }
                args
    , hole:
        \args ->
          rec.hole
            $ R.union
                { actions:
                    Array.concat
                      [ common (Hole args.hole) args
                      , [ inlambda { args } ]
                      ]
                }
                args
    }
  where
  common :: forall r. Term -> { visit :: Visit | r } -> Array Action
  common term args =
    [ enlambda { args, term }
    , digterm { args }
    , enlet { args, term }
    , enbuffer { args, term }
    , endata { args, term }
    , pop { args, term }
    ]

-- copy { clipboard: { ix: fromJust args.visit.ix, gamma: ?a, alpha: ?a, term: ?a } }
-- | recArgItem
type ArgsArgItem r
  = Rec.ArgsArgItem ( | r )

type ArgsArgItem_ArgItem r rTerm
  = Rec.ArgsArgItem_ArgItem ( actions :: Array Action | r ) rTerm

recArgItem ::
  forall r a.
  Lacks "argItem" r =>
  Lacks "gamma" r =>
  Lacks "doms" r =>
  Lacks "cod" r =>
  { argItem :: Record (ArgsArgItem_ArgItem r (ArgsTerm r)) -> a } ->
  Record (ArgsArgItem r) -> a
recArgItem rec =
  Rec.recArgItem
    { argItem:
        \args ->
          rec.argItem
            $ R.union
                { actions:
                    [ actionIndent
                    ]
                }
                args
    }

-- | recSumItem
type ArgsSumItem r
  = Rec.ArgsSumItem ( | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = Rec.ArgsSumItem_SumItem ( actions :: Array Action | r ) rTermBind rParamItems

recSumItem ::
  forall r a.
  Lacks "sumItem" r =>
  { sumItem :: Record (ArgsSumItem_SumItem r (ArgsTermBind r) (ArgsParamItem r)) -> a } ->
  Record (ArgsSumItem r) -> a
recSumItem rec =
  Rec.recSumItem
    { sumItem:
        \args ->
          rec.sumItem
            $ R.union
                { actions: []
                }
                args
    }

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( | r )

type ArgsCaseItem_CaseItem r rTermBindItem rTerm
  = Rec.ArgsCaseItem_CaseItem ( actions :: Array Action | r ) rTermBindItem rTerm

recCaseItem ::
  forall r a.
  Lacks "caseItem" r =>
  Lacks "alpha" r =>
  Lacks "typeId" r =>
  Lacks "termId" r =>
  { caseItem :: Record (ArgsCaseItem_CaseItem r (ArgsTermBindItem r) (ArgsTerm r)) -> a } ->
  Record (ArgsCaseItem r) -> a
recCaseItem rec =
  Rec.recCaseItem
    { caseItem:
        \args ->
          rec.caseItem
            $ R.union
                { actions: []
                }
                args
    }

-- | recParamItems
type ArgsParamItem r
  = Rec.ArgsParamItem ( | r )

type ArgsParamItem_ParamItem r rType
  = Rec.ArgsParamItem_ParamItem ( actions :: Array Action | r ) rType

recParamItem ::
  forall r a.
  Lacks "paramItem" r =>
  { paramItem :: Record (ArgsParamItem_ParamItem r (ArgsType r)) -> a } ->
  Record (ArgsParamItem r) -> a
recParamItem rec =
  Rec.recParamItem
    { paramItem:
        \args ->
          rec.paramItem
            $ R.union
                { actions: []
                }
                args
    }

-- | recTermBindItems
type ArgsTermBindItem r
  = Rec.ArgsTermBindItem ( | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = Rec.ArgsTermBindItem_TermBindItem ( actions :: Array Action | r ) rTermBind

recTermBindItem ::
  forall r a.
  Lacks "termBindItem" r =>
  { termBindItem :: Record (ArgsTermBindItem_TermBindItem r (ArgsTermBind r)) -> a } ->
  Record (ArgsTermBindItem r) -> a
recTermBindItem rec =
  Rec.recTermBindItem
    { termBindItem:
        \args ->
          rec.termBindItem
            $ R.union
                { actions: []
                }
                args
    }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( actions :: Array Action | r ) rTypeId

recTypeBind ::
  forall r a.
  Lacks "typeBind" r =>
  { typeBind :: Record (ArgsTypeBind_TypeBind r (ArgsTypeId r)) -> a } ->
  Record (ArgsTypeBind r) -> a
recTypeBind rec =
  Rec.recTypeBind
    { typeBind:
        \args ->
          rec.typeBind
            $ R.union
                { actions:
                    [ editTypeBind { args, name: (unwrap args.typeBind.meta).name } ]
                }
                args
    }

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( actions :: Array Action | r ) rTermId

recTermBind ::
  forall r a.
  Lacks "termBind" r =>
  { termBind :: Record (ArgsTermBind_TermBind r (ArgsTermId r)) -> a } ->
  Record (ArgsTermBind r) -> a
recTermBind rec =
  Rec.recTermBind
    { termBind:
        \args ->
          rec.termBind
            $ R.union
                { actions:
                    [ editTermBind { args, name: (unwrap args.termBind.meta).name } ]
                }
                args
    }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( | r )

type ArgsTypeId_TypeId r
  = Rec.ArgsTypeId ( actions :: Array Action | r )

recTypeId ::
  forall r a.
  { typeId :: Record (ArgsTypeId_TypeId r) -> a } ->
  Record (ArgsTypeId r) -> a
recTypeId rec args = rec.typeId $ R.union { actions: [] } args

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( | r )

type ArgsTermId_TermId r
  = Rec.ArgsTermId ( actions :: Array Action | r )

recTermId ::
  forall r a.
  { termId :: Record (ArgsTermId_TermId r) -> a } ->
  Record (ArgsTermId r) -> a
recTermId rec args = rec.termId $ R.union { actions: [] } args

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( | r )

type ArgsHoleId_HoleId r
  = Rec.ArgsHoleId ( actions :: Array Action | r )

recHoleId ::
  forall r a.
  { holeId :: Record (ArgsHoleId_HoleId r) -> a } ->
  Record (ArgsHoleId r) -> a
recHoleId rec args = rec.holeId $ R.union { actions: [] } args

-- misc actions
actionIndent :: Action
actionIndent =
  Action
    { tooltip: Nothing
    , triggers: [ ActionTrigger_Keypress keys.indent ]
    , transition:
        { label: "indent"
        , effect:
            do
              state <- get
              selMode <- requireSelectMode
              let
                mb_step /\ ixIndentableParent = stepUpToNearestIndentableParentIxUp (toIxUp selMode.ix)
              term <-
                maybeTransitionM "indexSyntaxAt failed"
                  $ toTerm
                  =<< indentSyntaxAt mb_step (toIxDown ixIndentableParent) (SyntaxTerm state.program.term)
              setProgram
                (state.program { term = term })
        }
    }
