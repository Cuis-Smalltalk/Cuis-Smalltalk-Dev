'From Cuis 4.2 of 25 July 2013 [latest update: #1766] on 28 July 2013 at 8:20:34.357 pm'!

!Class methodsFor: 'compiling' stamp: 'jmv 7/28/2013 20:15'!
localBindingOf: varNameSymbol
	"Answer the binding of some variable resolved in the scope of the receiver."

	"First look in classVar dictionary."
	(self classPool bindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		(pool bindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].
	].

	"Finally look higher up the superclass chain and fail at the end."
	^superclass ifNotNil: [ superclass localBindingOf: varNameSymbol ]! !


!Class methodsFor: 'compiling' stamp: 'jmv 7/28/2013 20:18'!
bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol |
	aSymbol _ varName asSymbol.

	"First look in receiver, and up the hierarchy.  Inherited variables take precedence over globals."
	(self localBindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in declared environment. (Smalltalk is the only environment in Cuis)"
	(Smalltalk bindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Fail at the end."
	^nil! !

