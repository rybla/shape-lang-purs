module App.Action where

import App.State
import Language.Shape.Stlc.Syntax
import Prelude

-- needs to send "query" into LogConsole that triggers LogConsoleAction inside of it
data AppAction
  = LogAppAction String

data EditorAction
  = LiftEditorAction AppAction
  | UpdateEditorAction (EditorState -> EditorState)
  | SequenceEditorActions (Array EditorAction)

data ConsoleAction
  = LiftConsoleAction AppAction
  | LogConsoleAction String

setModule :: Module -> EditorAction
setModule module' = UpdateEditorAction $ _ { module_ = module' }
