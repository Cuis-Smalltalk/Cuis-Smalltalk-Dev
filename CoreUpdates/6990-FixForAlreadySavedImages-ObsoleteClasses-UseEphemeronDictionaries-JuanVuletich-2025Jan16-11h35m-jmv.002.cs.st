'From Cuis7.3 [latest update: #6982] on 16 January 2025 at 11:52:32 am'!

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 1/16/2025 11:37:45'!
recreateObsoleteSubclassesCollection
	"
	Behavior recreateObsoleteSubclassesCollection
	"
	| newOne |
	newOne := EphemeronIdentityDictionary new.
	ObsoleteSubclasses ifNotNil:
		[:oldOne|
		oldOne keysAndValuesDo:
			[ :k :v | newOne at: k put: v ] ].
	ObsoleteSubclasses := newOne.! !


!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 1/16/2025 11:42:49'!
addObsoleteSubclass: aClass
	"Weakly remember that aClass was a subclass of the receiver and is now obsolete"
	| obs |
	ObsoleteSubclasses ifNil: [
		ObsoleteSubclasses := EphemeronIdentityDictionary new ].
	obs := ObsoleteSubclasses at: self ifAbsent:[WeakArray new].
	(obs includes: aClass) ifTrue:[^self].
	obs := obs copyWithout: nil.
	obs := obs copyWith: aClass.
	ObsoleteSubclasses at: self put: obs.
! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 1/16/2025 11:46:50'!
obsoleteSubclasses
	"Return all the weakly remembered obsolete subclasses of the receiver.
	Clean out the already collected ones."
	| obs |
	ObsoleteSubclasses ifNil: [
		^#() ].
	obs := ObsoleteSubclasses at: self ifAbsent:[^#()].
	obs := obs copyWithout: nil.
	obs isEmpty
		ifTrue: [ ObsoleteSubclasses removeKey: self ifAbsent: nil ]
		ifFalse: [ ObsoleteSubclasses at: self put: obs].
	^obs! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 1/16/2025 11:42:11'!
removeAllObsoleteSubclasses
	"Remove all the obsolete subclasses of the receiver"
	ObsoleteSubclasses ifNotNil: [
		ObsoleteSubclasses removeKey: self ifAbsent: nil ]! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 1/16/2025 11:47:11'!
removeObsoleteSubclass: aClass
	"Remove aClass from the weakly remembered obsolete subclasses.
	Clean out the already collected ones."
	| obs |
	ObsoleteSubclasses ifNil: [
		^ self ].
	obs := ObsoleteSubclasses at: self ifAbsent:[^ self].
	(obs includes: aClass) ifFalse:[^self].
	obs := obs copyWithout: aClass.
	obs := obs copyWithout: nil.
	obs isEmpty
		ifTrue: [ObsoleteSubclasses removeKey: self ifAbsent: nil]
		ifFalse: [ObsoleteSubclasses at: self put: obs]! !


!ClassBuilder methodsFor: 'initialization' stamp: 'jmv 1/16/2025 11:40:05'!
doneCompiling: aClass
	"The receiver has finished modifying the class hierarchy.
	Do any necessary cleanup."
	aClass doneCompiling.! !

!methodRemoval: Behavior class #flushObsoleteSubclasses stamp: 'jmv 1/16/2025 11:40:16'!
Behavior class removeSelector: #flushObsoleteSubclasses!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Behavior recreateObsoleteSubclassesCollection!

