'From Cuis7.3 [latest update: #7079] on 19 March 2025 at 10:55:27 am'!

!Debugger methodsFor: 'initialization' stamp: 'FJG 2/22/2025 18:35:24'!
isOnBreakpointMethod
	"Tells whether the selected method is a halt method, like #halt or #haltOnce"

	^ self selectedContext method selector beginsWith: 'halt'! !

!Debugger methodsFor: 'initialization' stamp: 'FJG 2/22/2025 18:37:03'!
stepOutBreakpointMethods

	^ [self isOnBreakpointMethod ]
		whileTrue: [ self send. self down ]! !


!Debugger methodsFor: 'initialization' stamp: 'FJG 2/22/2025 18:37:03'!
openFullMorphicLabel: aLabelString
	"Open a full morphic debugger with the given label"

	| oldContextStackIndex |
	oldContextStackIndex := contextStackIndex.
	self expandStack. "Sets contextStackIndex to zero."

	DebuggerWindow open: self label: aLabelString.
	self toggleContextStackIndex: oldContextStackIndex.

	self stepOutBreakpointMethods.! !

