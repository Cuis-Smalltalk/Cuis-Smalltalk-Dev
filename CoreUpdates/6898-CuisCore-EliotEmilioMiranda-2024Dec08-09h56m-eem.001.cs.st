'From Cuis7.3 [latest update: #6897] on 8 December 2024 at 10:35:01 am'!

!Parser methodsFor: 'primitives' stamp: 'eem 12/8/2024 10:30:17'!
externalType: descriptorClass
	"Parse an return an external type"
	| xType |
	(here == #'.' and: [hereType == #period and: [tokenType == #period]]) ifTrue:
		[[here == #'.'] whileTrue: [self advance].
		 ^descriptorClass punctuatorNamed: #'...'].
	xType := descriptorClass atomicTypeNamed: here.
	xType ifNil: "Look up from class scope"
		[Symbol
			hasInterned: here
			ifTrue:[:sym| xType := descriptorClass structTypeNamed: sym]].
	xType ifNil:
		["Raise an error if user is there"
		self interactive ifTrue:[^nil].
		"otherwise go over it silently"
		xType := descriptorClass forceTypeNamed: here].
	self advance.
	^(self matchToken: #*)
		ifTrue:[xType asPointerType]
		ifFalse:[xType]! !

