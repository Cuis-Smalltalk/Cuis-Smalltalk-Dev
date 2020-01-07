'From Cuis 5.0 [latest update: #3985] on 23 December 2019 at 7:26:58 am'!

!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 07:24:08'!
hasLocallyArgumentOrTemporaryNamed: aVariableName

	"- hasArgumentOrTemporaryNamed: returns true if there is a temp or variable in this scope or subscopes 
	   named aVariableName
	- hasLocalName: returns true if there is a variable in the scopeTable name aVariableName. That includes 
	  temps, arguments, instance variables and pseudo-variables
	- hasLocallyArgumentOrTemporaryNamed: returns true if ony this scope defines a temp or argument 
	  named aVariableName. - Hernan" 
	
	^(self hasNodeIn: self temporaries named: aVariableName)
		or: [self hasNodeIn: self arguments named: aVariableName ]! !


!MethodNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 07:24:55'!
hasArgumentOrTemporaryNamed: aVariableName

	"See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan" 
	
	^self tempNames includes: aVariableName! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 07:25:03'!
hasLocalNamed: aName

	"See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan" 

	^ encoder hasLocalNamed: aName ! !

!methodRemoval: MethodNode #hasLocallyArgumentOrTemporaryNamed: stamp: 'HAW 12/23/2019 07:22:01'!
MethodNode removeSelector: #hasLocallyArgumentOrTemporaryNamed:!
!methodRemoval: MethodNode #isLocalArgumentOrTemporary: stamp: 'HAW 12/23/2019 07:22:37'!
MethodNode removeSelector: #isLocalArgumentOrTemporary:!
!methodRemoval: BlockNode #hasLocallyArgumentOrTemporaryNamed: stamp: 'HAW 12/23/2019 07:21:56'!
BlockNode removeSelector: #hasLocallyArgumentOrTemporaryNamed:!
!methodRemoval: BlockNode #isLocalArgumentOrTemporary: stamp: 'HAW 12/23/2019 07:22:33'!
BlockNode removeSelector: #isLocalArgumentOrTemporary:!
!methodRemoval: ParseNode #hasNodeIn:named: stamp: 'HAW 12/23/2019 07:22:15'!
ParseNode removeSelector: #hasNodeIn:named:!
