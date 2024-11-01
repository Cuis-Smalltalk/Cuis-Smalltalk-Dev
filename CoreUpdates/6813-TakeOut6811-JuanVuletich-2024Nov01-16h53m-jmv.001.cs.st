'From Cuis7.1 [latest update: #6810] on 1 November 2024 at 4:54:00 pm'!

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:24:23'!
shouldNotBeRedefined
	"Return true if the receiver should not be redefined.
	The assumption is that classes in Smalltalk specialObjects and 
	Behaviors should not be redefined"

	^(Smalltalk specialObjectsArray identityIncludes: self)
			or: [self isKindOf: self]! !

