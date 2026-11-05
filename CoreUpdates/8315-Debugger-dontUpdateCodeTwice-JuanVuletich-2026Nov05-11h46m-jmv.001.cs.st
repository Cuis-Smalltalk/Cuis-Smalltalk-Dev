'From Cuis7.9 [latest update: #8314] on 5 November 2026 at 11:49:39 am'!

!Debugger methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 2:43:57 pm (UTC)'!
resetContext: aContext
	"Used when a new context becomes top-of-stack, for instance when the
	method of the selected context is re-compiled, or the simulator steps or
	returns to a new method. There is room for much optimization here, first
	to save recomputing the whole stack list (and text), and secondly to avoid
	recomposing all that text (by editing the TextComposition instead of recreating it)."

	| oldContext |
	oldContext := self selectedContext.
	contextStackTop := aContext.
	self newStack: contextStackTop contextStack.
	self changed: #contextStackList.
	self contextStackIndex: 1 oldContextWas: oldContext.! !

