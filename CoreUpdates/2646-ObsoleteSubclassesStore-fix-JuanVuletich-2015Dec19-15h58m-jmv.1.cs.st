'From Cuis 4.2 of 25 July 2013 [latest update: #2645] on 19 December 2015 at 4:02:04.617065 pm'!

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 12/19/2015 16:00'!
addObsoleteSubclass: aClass
	"Weakly remember that aClass was a subclass of the receiver and is now obsolete"
	| obs |
	ObsoleteSubclasses ifNil: [
		ObsoleteSubclasses _ WeakIdentityKeyDictionary new ].
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[WeakArray new].
	(obs includes: aClass) ifTrue:[^self].
	obs _ obs copyWithout: nil.
	obs _ obs copyWith: aClass.
	ObsoleteSubclasses at: self put: obs.
! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 12/19/2015 16:01'!
obsoleteSubclasses
	"Return all the weakly remembered obsolete subclasses of the receiver"
	| obs |
	ObsoleteSubclasses ifNil: [
		^#() ].
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[^#()].
	obs _ obs copyWithout: nil.
	obs isEmpty
		ifTrue: [ ObsoleteSubclasses removeKey: self ifAbsent: nil ]
		ifFalse: [ ObsoleteSubclasses at: self put: obs].
	^obs! !

!methodRemoval: Behavior class #globalObsoleteSubclassesStore!
Behavior class removeSelector: #globalObsoleteSubclassesStore!
!methodRemoval: Behavior class #initialize!
Behavior class removeSelector: #initialize!
!methodRemoval: Behavior class #initializeObsoleteSubclasses!
Behavior class removeSelector: #initializeObsoleteSubclasses!
Behavior initialize!
!methodRemoval: Behavior #globalObsoleteSubclassesStore!
Behavior removeSelector: #globalObsoleteSubclassesStore!
