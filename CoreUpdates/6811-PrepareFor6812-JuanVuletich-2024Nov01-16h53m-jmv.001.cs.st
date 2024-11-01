'From Cuis7.1 [latest update: #6810] on 1 November 2024 at 4:53:50 pm'!

!Behavior methodsFor: 'testing' stamp: 'jmv 11/1/2024 16:53:26'!
shouldNotBeRedefined
	"Return true if the receiver should not be redefined.
	The assumption is that classes in Smalltalk specialObjects and 
	Behaviors should not be redefined"
true ifTrue: [ ^false ].
	^(Smalltalk specialObjectsArray identityIncludes: self)
			or: [self isKindOf: self]! !

