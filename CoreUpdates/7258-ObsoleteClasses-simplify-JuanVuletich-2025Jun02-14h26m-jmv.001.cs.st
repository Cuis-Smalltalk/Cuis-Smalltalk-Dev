'From Cuis7.5 [latest update: #7257] on 2 June 2025 at 2:40:50 pm'!

!WeakSet commentStamp: '<historical>' prior: 0!
An implementation of WeakSet that gives good performance for large instances.

It is implemented as a two level hash table. The outer level is a closed hash table, and is accessed using the higher bits of each element's hash value. At each position a Bucket is stored.

The lower bits of the hash are used to acess each Bucket. The buckets for an open hash table. Each bucket has a fixed size for direct access using the hash, and after it, a variable size area to store elements with duplicated (partial) hashes.

Currently, the Buckets use 8 positions for O(1) direct access using the lower 3 bits of the hash. The iterable part after it can grow as needed.

Buckets are WeakArrays.

The idea for small WeakArrays as buckets is from a previous SymbolSet by Andrés Valloud.
The idea of buckets having a direct access part, and an variable sized overflow part for (partial) hash collisions is from Juan Vuletich, specific for this class.

Note: An implementations using these ideas, could be good for Set et al.
Note: Perhaps a three level table, with a fixed size outer level, and where the mid level arrays grow as needed, could further improve performance for very large collections.!


!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 6/2/2025 14:07:59'!
recreateObsoleteClassesCollection
	"
	Behavior recreateObsoleteClassesCollection
	"
	ObsoleteSubclasses := nil.
	Smalltalk garbageCollect.
	Smalltalk allObjectsDo: [ :cl |
		(cl isBehavior and: [cl isObsolete]) ifTrue: [
			ObsoleteSubclasses ifNil: [
				ObsoleteSubclasses := WeakIdentitySet new ].
			ObsoleteSubclasses add: cl]].! !


!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 6/2/2025 13:25:19'!
addObsoleteSubclass: aClass
	"Weakly remember that aClass was a subclass of the receiver and is now obsolete"

	ObsoleteSubclasses ifNil: [
		ObsoleteSubclasses := WeakIdentitySet new ].
	ObsoleteSubclasses add: aClass.! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 6/2/2025 13:25:44'!
obsoleteSubclasses
	"Return all the weakly remembered obsolete subclasses of the receiver.
	Clean out the already collected ones."

	ObsoleteSubclasses ifNil: [
		^#() ].
	^ObsoleteSubclasses select: [ :any |
		any superclass == self ].! !

!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 6/2/2025 13:30:18'!
removeObsoleteSubclass: aClass
	"Remove aClass from the weakly remembered obsolete subclasses."

	ObsoleteSubclasses ifNil: [
		^ self ].
	ObsoleteSubclasses remove: aClass ifAbsent: [].! !


!WeakSet class methodsFor: '[mkSymbol] instance creation' stamp: 'jmv 6/2/2025 14:38:40'!
newWithRoomForMoreThan: nElements
	"Create a Set large enough to hold nElements without growing
	WeakSet newWithRoomForMoreThan: 31
	WeakSet newWithRoomForMoreThan: 32
	"

	| answer |
	answer := self basicNew.
	answer init: (2 raisedToInteger: (nElements // answer bucketDesiredMeanOccupation max: 1) highBit -1).
	^answer! !

!methodRemoval: Metaclass #replaceObsoleteInstanceWith: stamp: 'jmv 6/2/2025 14:26:27'!
Metaclass removeSelector: #replaceObsoleteInstanceWith:!
!methodRemoval: Behavior #removeAllObsoleteSubclasses stamp: 'jmv 6/2/2025 14:26:27'!
Behavior removeSelector: #removeAllObsoleteSubclasses!
!methodRemoval: Behavior #recreateObsoleteSubclassesCollection stamp: 'jmv 6/2/2025 14:26:27'!
Behavior removeSelector: #recreateObsoleteSubclassesCollection!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Behavior recreateObsoleteClassesCollection!

