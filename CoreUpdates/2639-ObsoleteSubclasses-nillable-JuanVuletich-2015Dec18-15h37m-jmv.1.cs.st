'From Cuis 4.2 of 25 July 2013 [latest update: #2638] on 18 December 2015 at 3:41:44.984592 pm'!

!Behavior class methodsFor: 'class initialization' stamp: 'jmv 12/14/2015 23:24'!
globalObsoleteSubclassesStore
	ObsoleteSubclasses ifNil: [
		self initializeObsoleteSubclasses ].
	^ObsoleteSubclasses! !


!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 12/14/2015 23:26'!
addObsoleteSubclass: aClass
	"Weakly remember that aClass was a subclass of the receiver and is now obsolete"
	| obs |
	self class globalObsoleteSubclassesStore.
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[WeakArray new].
	(obs includes: aClass) ifTrue:[^self].
	obs _ obs copyWithout: nil.
	obs _ obs copyWith: aClass.
	ObsoleteSubclasses at: self put: obs.
! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 12/14/2015 23:26'!
obsoleteSubclasses
	"Return all the weakly remembered obsolete subclasses of the receiver"
	| obs |
	self class globalObsoleteSubclassesStore.
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[^#()].
	obs _ obs copyWithout: nil.
	obs isEmpty
		ifTrue: [ ObsoleteSubclasses removeKey: self ifAbsent: nil ]
		ifFalse: [ ObsoleteSubclasses at: self put: obs].
	^obs! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 12/18/2015 15:39'!
removeAllObsoleteSubclasses
	"Remove all the obsolete subclasses of the receiver"
	ObsoleteSubclasses ifNotNil: [
		ObsoleteSubclasses finalizeValues. "clean up if need be"
		ObsoleteSubclasses removeKey: self ifAbsent: nil ]! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 12/18/2015 15:39'!
removeObsoleteSubclass: aClass
	"Remove aClass from the weakly remembered obsolete subclasses"
	| obs |
	ObsoleteSubclasses ifNil: [
		^ self ].
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[^ self].
	(obs includes: aClass) ifFalse:[^self].
	obs _ obs copyWithout: aClass.
	obs _ obs copyWithout: nil.
	obs isEmpty
		ifTrue: [ObsoleteSubclasses removeKey: self ifAbsent: nil]
		ifFalse: [ObsoleteSubclasses at: self put: obs]! !


!Behavior class methodsFor: 'class initialization' stamp: 'jmv 12/18/2015 15:40'!
flushObsoleteSubclasses
	"Behavior flushObsoleteSubclasses"

	ObsoleteSubclasses ifNotNil: [
		ObsoleteSubclasses keys "need a copy" 
			do: [ :obs| obs ifNotNil:[ obs obsoleteSubclasses ]]. "remove themselves"
		ObsoleteSubclasses finalizeValues ]! !

