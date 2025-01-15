'From Cuis7.3 [latest update: #6977] on 15 January 2025 at 9:40:31 am'!
!classDefinition: #WeakSet category: #'Collections-Unordered'!
Collection subclass: #WeakSet
	instanceVariableNames: 'buckets estimatedSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!WeakSet commentStamp: 'jmv 1/15/2025 09:20:40' prior: 0!
An implementation of WeakSet that gives food performance for large instances.

It is implemented as a two level hash table. The outer level is a closed hash table, and is accessed using the higher bits of each element's hash value. At each position a Bucket is stored.

The lower bits of the hash are used to acess each Bucket. The buckets for an open hash table. Each bucket has a fixed size for direct access using the hash, and after it, a variable size area to store elements with duplicated (partial) hashes.

Currently, the Buckets use 8 positions for O(1) direct access using the lower 3 bits of the hash. The iterable part after it can grow as needed.

Buckets are WeakArrays.

The idea for small WeakArrays as buckets is from a previous SymbolSet by Andrés Valloud.
The idea of buckets having a direct access part, and an variable sized overflow part for (partial) hash collisions is from Juan Vuletich, specific for this class.

Note: An implementations using these ideas, could be good for Set et al.
Note: Perhaps a three level table, with a fixed size outer level, and where the mid level arrays grow as needed, could further improve performance for very large collections.!

!classDefinition: #WeakIdentitySet category: #'Collections-Unordered'!
WeakSet subclass: #WeakIdentitySet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!WeakIdentitySet commentStamp: 'jmv 1/13/2025 09:10:25' prior: 0!
See class comment at WeakSet.

This subclass only differs from it in using #== and #identityHash instead of #= and #hash.!


!WeakSet methodsFor: 'accessing' stamp: 'jmv 1/13/2025 13:08:52'!
estimatedSize
	"Actual size may be less: as our references to elements are weak, elements could vanish anytime."

	^estimatedSize! !

!WeakSet methodsFor: 'accessing' stamp: 'jmv 1/12/2025 18:05:02'!
size

	^buckets inject: 0 into: [ :prev :bucket | bucket ifNil:[prev] ifNotNil: [(bucket count: [:s | s notNil]) + prev ]]! !

!WeakSet methodsFor: 'adding and finding' stamp: 'jmv 1/13/2025 13:11:00'!
add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present.
	Answer newObject (even if another, but equal, object is already included)."

	| hash |
	hash := self hashFor: newObject.
	(self basicLookup: newObject withHash: hash) ifNil: [
		self basicAdd: newObject withHash: hash ].
	^newObject! !

!WeakSet methodsFor: 'adding and finding' stamp: 'jmv 1/10/2025 10:10:53'!
add: newObject withOccurrences: anInteger
	^ self add: newObject! !

!WeakSet methodsFor: 'adding and finding' stamp: 'jmv 1/10/2025 10:55:11'!
includes: anObject 
	^ (self lookup: anObject) ~~ nil! !

!WeakSet methodsFor: 'adding and finding' stamp: 'jmv 1/11/2025 10:14:21'!
lookup: anObject
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise.
	Note: The hash will be used several times, and it may be expensive. Ask for it just once."

	^self basicLookup: anObject withHash: (self hashFor: anObject)! !

!WeakSet methodsFor: 'adding and finding' stamp: 'jmv 1/11/2025 10:22:07'!
lookup: anObject ifAbsentAddFrom: aBlock
	"Look for an element equal to anObject.
	If found, answer it.
	If not found, evaluate aBlock, add the result and answer it.
	Note: Do NOT add anObject to the Set!!"
	
	| newObject |
	^(self lookup: anObject) ifNil: [
		newObject := aBlock value.
		self basicAdd: newObject withHash: (self hashFor: newObject).
		newObject ].! !

!WeakSet methodsFor: 'adding and finding' stamp: 'jmv 1/10/2025 10:21:45'!
occurrencesOf: anObject
	^ (self includes: anObject) ifTrue: [1] ifFalse: [0]! !

!WeakSet methodsFor: 'enumerating' stamp: 'jmv 1/13/2025 13:09:42'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet := self species newWithRoomForMoreThan: self size.
	self do:
		[ :each |
			newSet add: (aBlock value: each)].
	^ newSet! !

!WeakSet methodsFor: 'enumerating' stamp: 'jmv 1/13/2025 16:59:24'!
do: aBlock

	buckets do:
		[:eachBucket |
			eachBucket ifNotNil:
				[
					eachBucket do:
						[:element |
							element notNil ifTrue:
								[aBlock value: element]]]]! !

!WeakSet methodsFor: 'enumerating' stamp: 'jmv 1/13/2025 13:09:14'!
union: aCollection
	"Answer the set theoretic union of the receiver and aCollection, using the receiver's notion of equality and not side effecting the receiver at all."

	^(self species newWithRoomForMoreThan: estimatedSize + aCollection estimatedSize)
		addAll: self;
		addAll: aCollection;
		yourself.! !

!WeakSet methodsFor: 'comparing' stamp: 'jmv 1/10/2025 10:17:35'!
emptyCollectionHash
	^ Set hash! !

!WeakSet methodsFor: 'copying' stamp: 'jmv 1/13/2025 13:03:50'!
postCopy

	self rehash! !

!WeakSet methodsFor: 'objects from disk' stamp: 'jmv 1/13/2025 13:02:58'!
comeFullyUpOnReload: smartRefStream
	"Symbols have new hashes in this image."

	self rehash.! !

!WeakSet methodsFor: 'initialization' stamp: 'jmv 1/13/2025 12:44:39'!
init: n
	"Initialize array to an array size of n"
	buckets := Array new: n.
	estimatedSize := 0.! !

!WeakSet methodsFor: 'private' stamp: 'jmv 1/14/2025 17:51:20'!
basicAdd: anObject withHash: hashValue
	"Add an object.
	Note: It is assumed that the argument is NOT already included. Check senders."

	| bucketIndex bucket indexInBucket bucketSize newBucket |

	"bucket creation is lazy"
	bucketIndex := self bucketIndexFor: hashValue.
	bucket := buckets at: bucketIndex.
	bucket ifNil: [ bucket := buckets at: bucketIndex put: (WeakArray new: self bucketDirectAccessSize)].

	"1. If our direct access spot in the hasheable area is free, just use it and we are done."
	indexInBucket := self indexInBucketFor: hashValue.
	(bucket at: indexInBucket) == nil ifTrue: [
		bucket at: indexInBucket put: anObject.
		estimatedSize := estimatedSize + 1.
		^self ].

	"2. If there is available room in the iterable part of the bucket, add our new element there."
	bucketSize := bucket size.
	self bucketIterablePartStart to: bucketSize do: [ :i |
		(bucket at: i) == nil ifTrue: [
			bucket at: i put: anObject.
			estimatedSize := estimatedSize + 1.
			^self ]].

	"There's no room in this bucket. It may be worth growing the collection.
	I (jmv) could not find a good heuristic on whether to grow the collection based on a single bucket.
	The adopted one was to limit average mean occupation over all buckets."
	estimatedSize > (buckets size * self bucketDesiredMeanOccupation) ifTrue: [
		self grow.
		"Now try again.
		Maybe the new element is added in the first part of the bucket,
		or in already available room in the second part. (1 and 2 above).
		Or maybe growing the bucket as below (3) is needed.
		What is sure is that this won't recurse more than once: buckets size was just doubled by #grow."
		^self basicAdd: anObject withHash: hashValue ].

	"3. We still need to grow the bucket. Grow it and add our new element."
	newBucket := WeakArray new: bucketSize + self bucketIterablePartGrow.
	newBucket replaceFrom: 1 to: bucketSize with: bucket startingAt: 1.
	newBucket at: bucketSize + 1 put: anObject.
	buckets at: bucketIndex put: newBucket.
	estimatedSize := estimatedSize + 1.! !

!WeakSet methodsFor: 'private' stamp: 'jmv 1/13/2025 14:56:03'!
basicLookup: anObject withHash: hashValue
	"Look for anObject. Assume hashValue is anObject hash, passed as argument to avoid possibly expensive repeated hash computation.
	Answer the element found, or nil if not found."

	| bucket indexInBucketForDirectAccess directAccessElelemt |
	bucket := buckets at: (self bucketIndexFor: hashValue).
	bucket ifNil: [ ^nil ].

	indexInBucketForDirectAccess := self indexInBucketFor: hashValue.
	directAccessElelemt := bucket at: indexInBucketForDirectAccess.
	(self areEqual: directAccessElelemt and: anObject)
		ifTrue: [ ^directAccessElelemt ].

	(self basicLook: anObject inIterablePartOf: bucket)
		ifNotNil: [ :index | | found |
			"We found it at the iterable part of the bucket.
			Move to the the direct access slot if possible."
			found := bucket at: index.
			directAccessElelemt isNil ifTrue: 
				[
					bucket at: indexInBucketForDirectAccess put: found.
					bucket at: index put: nil].
			^found].

	^nil! !

!WeakSet methodsFor: 'private' stamp: 'jmv 1/13/2025 14:38:32'!
bucketIndexFor: hashValue

	"The lowest few bits are used to index into each bucket.
	Don't use them for finding the bucket.
	See #indexInBucketFor:"
	^ ((hashValue bitShift: self bucketsHashShift)
			"Take only as much bits as we can use."
			bitAnd: buckets size-1)
				"And remember Smalltalk does 1-based indexing."
				+ 1! !

!WeakSet methodsFor: 'private' stamp: 'jmv 1/14/2025 17:20:05'!
grow
	"Duplicate size of buckets. Keep old buckets.
	For each old bucket:
	- Duplicate it as a new bucket
	- Move elements that belong in the new bucket to it
	- If possible, move elements in the iterable area to the hasheable area. This is important for search performance.
	
	Note: An alternative implementation is simply
		self rehash: buckets size * 2
	But the current implementation is much faster, at the expense of additional memory consumption."

	| newBuckets oldBucketsSize maskForNewUsedHashBit anOldBucket aNewBucket elementHash diirectAccessIndexInBucket keepdOldBucket |
	estimatedSize := 0.
	oldBucketsSize := buckets size.
	maskForNewUsedHashBit := oldBucketsSize * self bucketDirectAccessSize. "We're directly accessing the full hash, including the lower bits."
	newBuckets := buckets grownTo: oldBucketsSize * 2.
	1 to: oldBucketsSize do:
		[ :indexToOldBucket |
			anOldBucket := buckets at: indexToOldBucket.
			anOldBucket ifNotNil:
				[
					keepdOldBucket := false.
					aNewBucket := nil.
					"Move elements in hasheable area to new bucket if appropriate."
					1 to: self bucketDirectAccessSize do:
						[ :indexInBucket |
							(anOldBucket at: indexInBucket) ifNotNil:
								[ :element |
									estimatedSize := estimatedSize + 1.
									((self hashFor: element) bitAnd: maskForNewUsedHashBit) = 0
									ifTrue:
										[ keepdOldBucket := true ]
									ifFalse:
										[
											aNewBucket ifNil: [
												aNewBucket := WeakArray new: anOldBucket size.
												newBuckets at: oldBucketsSize + indexToOldBucket put: aNewBucket ].
											aNewBucket at: indexInBucket put: (anOldBucket at: indexInBucket).
											anOldBucket at: indexInBucket put: nil ]]].
					"For elements is iterable area, try to move them to the hasheable area."
					self bucketIterablePartStart to: anOldBucket size do:
						[ :indexInBucket |
							(anOldBucket at: indexInBucket) ifNotNil:
								[ :element |
									estimatedSize := estimatedSize + 1.
									elementHash := self hashFor: element.
									diirectAccessIndexInBucket := self indexInBucketFor: elementHash.
									(elementHash bitAnd: maskForNewUsedHashBit) = 0
									ifTrue:
										[ "Stays in old bucket. Try to move to hasheable part."
											keepdOldBucket := true.
											(anOldBucket at: diirectAccessIndexInBucket) ifNil:
												[
													anOldBucket at: diirectAccessIndexInBucket put: element.
													anOldBucket at: indexInBucket put: nil ]]
									ifFalse:
										[ "Moves to new bucket. Try to move to hasheable part, or move to iterable part."
											aNewBucket ifNil: [
												aNewBucket := WeakArray new: anOldBucket size.
												newBuckets at: oldBucketsSize + indexToOldBucket put: aNewBucket ].
											(aNewBucket at: diirectAccessIndexInBucket) ifNil:
												[ aNewBucket at: diirectAccessIndexInBucket put: element ]
											ifNotNil:
												[ aNewBucket at: indexInBucket put: element ].
											anOldBucket at: indexInBucket put: nil ]]].
					keepdOldBucket ifFalse:
						[newBuckets at: indexToOldBucket put: nil ]]].
	buckets := newBuckets.! !

!WeakSet methodsFor: 'private' stamp: 'jmv 1/13/2025 14:38:55'!
indexInBucketFor: hashValue
	"see also #bucketIndexFor:"

	^ (hashValue bitAnd: self bucketDirectAccessMask) + 1! !

!WeakSet methodsFor: 'private' stamp: 'jmv 1/14/2025 11:55:48'!
rehash
	self rehash: buckets size! !

!WeakSet methodsFor: 'private' stamp: 'jmv 1/14/2025 11:55:25'!
rehash: newBucketsSize
	| oldBuckets |
	oldBuckets := buckets.
	self init: newBucketsSize.
	oldBuckets do:
		[:eachBucket |
			eachBucket ifNotNil:
				[
					eachBucket do:
						[:element |
							element notNil
								ifTrue: [self add: element]]]]! !

!WeakSet methodsFor: 'private-equality or identity' stamp: 'jmv 1/13/2025 09:43:19'!
areEqual: a and: b
	"To make it easy to implement WeakIdentitySet as a subclass."

	^a = b! !

!WeakSet methodsFor: 'private-equality or identity' stamp: 'jmv 1/13/2025 09:32:18'!
basicLook: anObject inIterablePartOf: aBucket
	"To make it easy to implement WeakIdentitySet as a subclass."
	"Answer the index or nil if not found"

	self bucketIterablePartStart to: aBucket size do: [ :i |
		(aBucket at: i) = anObject ifTrue: [ ^i ]].
	^nil! !

!WeakSet methodsFor: 'private-equality or identity' stamp: 'jmv 1/13/2025 09:08:56'!
hashFor: anObject
	"To make it easy to implement WeakIdentitySet as a subclass."

	^anObject hash! !

!WeakSet methodsFor: 'removing' stamp: 'jmv 1/13/2025 14:53:46'!
copyWithout: oldElement 
	"Answer a copy of the receiver that does not contain any elements equal
	to oldElement."

	^ self copy
		remove: oldElement ifAbsent: nil;
		yourself! !

!WeakSet methodsFor: 'removing' stamp: 'jmv 1/13/2025 14:53:40'!
remove: oldObject ifAbsent: aBlock

	| hashValue indexInBucketForDirectAccess |
	hashValue := self hashFor: oldObject.
	(buckets at: (self bucketIndexFor: hashValue)) ifNotNil:
		[ :bucket |
			indexInBucketForDirectAccess := self indexInBucketFor: hashValue.
			(self areEqual: (bucket at: indexInBucketForDirectAccess) and: oldObject) ifTrue:
				[
					bucket at: indexInBucketForDirectAccess put: nil.
					estimatedSize := estimatedSize - 1.
					^oldObject ].
			(self basicLook: oldObject inIterablePartOf: bucket)
				ifNotNil: [ :index |
					bucket at: index put: nil.
					estimatedSize := estimatedSize - 1.
					^oldObject ]].
	^ aBlock value! !

!WeakSet methodsFor: 'removing' stamp: 'jmv 1/13/2025 12:38:32'!
removeAll

	buckets atAllPut: nil.
	estimatedSize := 0.! !

!WeakSet methodsFor: 'testing' stamp: 'jmv 1/13/2025 12:29:57'!
= aSet

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	(aSet is: #Set) ifFalse: [^ false].
	(aSet is: #Dictionary) ifTrue: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true! !

!WeakSet methodsFor: 'testing' stamp: 'jmv 1/10/2025 10:10:13'!
is: aSymbol
	^aSymbol == #Set or: [ super is: aSymbol ]! !

!WeakSet methodsFor: 'tuning constants' stamp: 'jmv 1/14/2025 17:56:51'!
bucketDesiredMeanOccupation
	"How much of a bucket to use on average.
	Larger values, avoiding waste of memory on empty slots in buckets mean slower access due to hash conflicts forcing iteration on the iterable part of the unlucky buckets.
	Smaller values, avoiding iteration and faster access mean more wasted memory.
	Some experimentation sugges that using in average half of the buckets gives great performance, with some disregard for memory."

	^5! !

!WeakSet methodsFor: 'tuning constants' stamp: 'jmv 1/13/2025 12:29:06'!
bucketDirectAccessMask
	"The mask to apply to a hash value to index it (+1 due to 1-based indexing).
	Must equal #bucketDirectAccessSize-1.
	See also #bucketDirectAccessSize, #bucketIterablePartStart and #bucketsHashShift"

	^7! !

!WeakSet methodsFor: 'tuning constants' stamp: 'jmv 1/13/2025 12:28:59'!
bucketDirectAccessSize
	"The size of the initial part of each bucket, that is only used for elements with matching hash bits.
	See also #bucketDirectAccessMask, #bucketIterablePartStart and #bucketsHashShift"

	^8! !

!WeakSet methodsFor: 'tuning constants' stamp: 'jmv 1/14/2025 17:53:35'!
bucketIterablePartGrow
	"Number of slots to add at once to the iterable part of Buckets."

	^1! !

!WeakSet methodsFor: 'tuning constants' stamp: 'jmv 1/13/2025 12:29:13'!
bucketIterablePartStart
	"The first index of the iterable area of each Bucket.
	Must equal #bucketDirectAccessSize+1
	See also #bucketDirectAccessSize, #bucketDirectAccessMask and #bucketsHashShift"

	^9! !

!WeakSet methodsFor: 'tuning constants' stamp: 'jmv 1/13/2025 12:29:22'!
bucketsHashShift
	"The mask to apply to a hash value to index it (+1 due to 1-based indexing).
	Must equal (#bucketDirectAccessMask highBit negated)
	See also #bucketDirectAccessSize, #bucketDirectAccessMask and #bucketIterablePartStart"

	^ -3! !


!WeakSet class methodsFor: 'instance creation' stamp: 'jmv 1/13/2025 13:01:47'!
new
	^ self newWithRoomForMoreThan: 32! !

!WeakSet class methodsFor: 'instance creation' stamp: 'jmv 1/13/2025 12:53:56'!
newFrom: aCollection
	"Create a new collection containing all the elements from aCollection"

	^(self newWithRoomForMoreThan: aCollection size)
		addAll: aCollection;
		yourself! !

!WeakSet class methodsFor: 'instance creation' stamp: 'jmv 1/13/2025 12:53:33'!
newWithRoomForMoreThan: nElements
	"Create a Set large enough to hold nElements without growing
	WeakSet newWithRoomForMoreThan: 31
	WeakSet newWithRoomForMoreThan: 32
	"

	| answer |
	answer := self basicNew.
	answer init: (2 raisedToInteger: (nElements // answer bucketDesiredMeanOccupation) highBit -1).
	^answer! !

!WeakSet class methodsFor: 'experiments - tuning' stamp: 'jmv 1/15/2025 09:22:57'!
experiment01
	"For measurements and tuning.
	Slow and memory hungry. Adjust a and m paramenters, play with this.
	A nice visualization of hash values is commented out. Play with it.
	self experiment01
	"
| a bits f h m n o pixelsMask s w ws |
n := 1000.
m := 10000.
a := Array new: n.
Transcript clear.
ws := WeakSet new.
Symbol allSymbols do: [ :sym | ws add: sym ].
ws inspect. a inspect.
Smalltalk garbageCollect.
Symbol rehash.
Smalltalk garbageCollect.
"---"
w := 2048.
h := 2048.
bits := ByteArray new: w*h // 8.
f := Form extent: w@h depth: 1 bits: bits.
bits atAllPut: 0.
pixelsMask := w*h-1.
"---"
[
	1 to: n do: [ :i |
		i \\ 10 = 0 ifTrue: [
			{'i: '. i. Time now} print ].
		o := Array new: m.
		a at: i put: o.
		1 to: m do: [ :i2 |
			s := Random next printString.
			o at: i2 put: s.
			ws add: s.
			"
			bits bitAt: (s hash bitAnd: pixelsMask)+1 put: 1.
			Random next > 10.9999 ifTrue: [
				f display. Display forceToScreen ]
			"
	 ]].
] timeToRun print.
ws size print.
ws instVarNamed: 'buckets' :: size print.
(ws instVarNamed: 'buckets') sum: [ :b | b ifNil: [0] ifNotNil: [b size ]] :: print.
(ws instVarNamed: 'buckets') count: [ :b | b notNil] :: print.
(ws instVarNamed: 'buckets') count: [ :b | b notNil and: [b size > 12 ]] :: print.
(ws instVarNamed: 'buckets') count: [ :b | b notNil and: [b size > 15 ]] :: print.
(ws instVarNamed: 'buckets') count: [ :b | b notNil and: [b allSatisfy: [ :e | e isNil ]] ] :: print.
'-----' print.
8 to: 15 do: [ :xx |
	(ws instVarNamed: 'buckets') count: [ :b | b notNil and: [b size = xx ]] :: print ].
(ws instVarNamed: 'buckets') count: [ :b | b notNil and: [b size > 15 ]] :: print.! !


!WeakIdentitySet methodsFor: 'private-equality or identity' stamp: 'jmv 1/13/2025 09:43:13'!
areEqual: a and: b
	"To make it easy to implement WeakIdentitySet as a subclass."

	^a == b! !

!WeakIdentitySet methodsFor: 'private-equality or identity' stamp: 'jmv 1/13/2025 09:32:27'!
basicLook: anObject inIterablePartOf: aBucket
	"To make it easy to implement WeakIdentitySet as a subclass."
	"Answer the index or nil if not found"

	self bucketIterablePartStart to: aBucket size do: [ :i |
		(aBucket at: i) == anObject ifTrue: [ ^i ]].
	^nil! !

!WeakIdentitySet methodsFor: 'private-equality or identity' stamp: 'jmv 1/13/2025 09:09:15'!
hashFor: anObject
	"To make it easy to implement WeakIdentitySet as a subclass."

	^anObject identityHash! !

