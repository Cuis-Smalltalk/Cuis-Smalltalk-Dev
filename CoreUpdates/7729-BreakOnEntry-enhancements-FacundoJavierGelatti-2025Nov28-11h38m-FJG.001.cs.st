'From Cuis7.5 [latest update: #7728] on 28 November 2025 at 11:40:59 am'!

!Debugger methodsFor: 'testing' stamp: 'FJG 27/Nov/2025 16:50:00'!
isOnBreakingMethodWrapper

	^ self selectedContext method == (BreakingMethodWrapper>>#run:with:in:)! !

!Debugger methodsFor: 'testing' stamp: 'FJG 27/Nov/2025 16:50:00'!
isOnHaltMethod
	"Tells whether the selected method is a halt method, like #halt or #haltOnce"

	^ self selectedContext method selector beginsWith: 'halt'! !


!Debugger methodsFor: 'initialization' stamp: 'FJG 27/Nov/2025 16:50:00'!
stepOutBreakpointMethods

	[ self isOnBreakingMethodWrapper ]
		whileTrue: [ self send ].

	[self isOnHaltMethod ]
		whileTrue: [ self send. self down ].
! !

!methodRemoval: Debugger #isOnBreakpointMethod stamp: 'FJG 28/Nov/2025 11:38:27'!
Debugger removeSelector: #isOnBreakpointMethod!
