'From Cuis 6.0 [latest update: #6027] on 13 September 2023 at 6:34:54 pm'!

!Class methodsFor: 'pool variables' stamp: 'jmv 9/13/2023 18:07:13'!
addSharedPool: aSharedPool 
	"Add the argument, aSharedPool, as one of the receiver's shared pools. 
	Create an error if the shared pool is already one of the pools.
	This method will work with shared pools that are plain Dictionaries or thenewer SharedPool subclasses"

	(self sharedPools includes: aSharedPool)
		ifTrue: [^self error: 'This is already in my shared pool list'].
	sharedPools := sharedPools
		ifNil: [Array with: aSharedPool]
		ifNotNil: [sharedPools copyWith: aSharedPool]! !

!Class methodsFor: 'pool variables' stamp: 'jmv 9/13/2023 18:24:47'!
allSharedPools
	"Answer a collection of the pools the receiver shares, including those defined  
	in the superclasses of the receiver."

	^ superclass
		ifNil: [ self sharedPools copy ]
		ifNotNil: [ superclass allSharedPools, self sharedPools ].! !

!Class methodsFor: 'pool variables' stamp: 'jmv 9/13/2023 18:20:54'!
removeSharedPool: aDictionary
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool
	dictionaries. Create an error notification if the dictionary is not one of
	the pools.
	: Note that it removes the wrong one if there are two empty Dictionaries in the list."

	| satisfiedSet workingSet aSubclass |

	(sharedPools isNil or: [(sharedPools includes: aDictionary) not ])
		ifTrue: [^self error: 'the dictionary is not in my pool'].

	"first see if it is declared in a superclass in which case we can remove it."
	(self selectSuperclasses: [ :class | class sharedPools includes: aDictionary]) isEmpty
		ifFalse: [
			sharedPools := sharedPools size = 1
				ifTrue: [nil]
				ifFalse: [sharedPools reject: [ :any | any = aDictionary]].
			^self ].

	"second get all the subclasses that reference aDictionary through me rather than a
	superclass that is one of my subclasses."

	workingSet := self subclasses asOrderedCollection.
	satisfiedSet := Set new.
	[workingSet isEmpty] whileFalse: [
		aSubclass := workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary)
			ifFalse: [
				satisfiedSet add: aSubclass.
				workingSet addAll: aSubclass subclasses]].

	"for each of these, see if they refer to any of the variables in aDictionary because
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do: [ :sub |
		aDictionary associationsDo: [ :aGlobal |
			(sub whichSelectorsReferTo: aGlobal) isEmpty
				ifFalse: [
					^self error: aGlobal key
						, ' is still used in code of class '
						, sub name ]]].
	sharedPools := sharedPools size = 1
		ifTrue: [nil]
		ifFalse: [sharedPools reject: [ :any | any = aDictionary]].! !

!Class methodsFor: 'pool variables' stamp: 'jmv 9/13/2023 18:06:41'!
sharedPools
	"Answer a Set of the pool dictionaries declared in the receiver."

	^sharedPools ifNil: [#()]! !

!Class methodsFor: 'initialization' stamp: 'jmv 9/13/2023 18:13:20'!
declareSharedPools: sharedPoolNames
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools s |
	oldPools := self sharedPools.
	s := sharedPoolNames size.
	s > 0 ifTrue: [
		sharedPools := Array new: s.
		sharedPoolNames withIndexDo: [ :poolName :i | | poolDict |
			poolDict := Smalltalk at: poolName asSymbol ifAbsent: [
				(PoolDefinitionNotification signalNamed: poolName)
					ifTrue: [ Smalltalk at: poolName asSymbol put: Dictionary new ]
					ifFalse: [^self error: poolName,' does not exist']].
			sharedPools at: i put: poolDict ]]
	ifFalse: [
		sharedPools := nil ].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]].! !

