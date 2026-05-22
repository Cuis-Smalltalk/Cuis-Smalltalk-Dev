"
I represent a set of objects without duplicates.  I can hold anything that responds to
#hash and #=, except for nil.  My instances will automatically grow, if necessary,
Note that I rely on #=, not #==.  If you want a set using #==, use IdentitySet.

Instance structure:

  array	An array whose non-nil elements are the elements of the set,
		and whose nil elements are empty slots.  There is always at least one nil.
		In fact I try to keep my "load" at 75% or less so that hashing will work well.

  tally	The number of elements in the set.  The array size is always greater than this.

The core operation is #findElementOrNil:, which either finds the position where an
object is stored in array, if it is present, or finds a suitable position holding nil, if
its argument is not present in array,
"
Class {
	#name : 'Set',
	#superclass : 'Collection',
	#instVars : [
		'tally',
		'array'
	],
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'instance creation' }
Set class >> new [
	^ self newWithRoomForMoreThan: 4
]

{ #category : 'instance creation' }
Set class >> newFrom: aCollection [
	"Create a new collection containing all the elements from aCollection"

	^(self newWithRoomForMoreThan: aCollection size)
		addAll: aCollection;
		yourself
]

{ #category : 'instance creation' }
Set class >> newWithRoomForMoreThan: nElements [
	"Create a Set large enough to hold nElements without growing"
	^ self basicNew init: (self sizeFor: nElements)
]

{ #category : 'initialization' }
Set class >> quickRehashAllSets  "Set rehashAllSets" [
	self withAllSubclassesDo:
		[:c |
			| insts |
			insts := c allInstances.
			(insts isEmpty or: [c = MethodDictionary]) ifFalse:
			['Rehashing instances of ' , c name
				displayProgressAt: Sensor mousePoint
				from: 1 to: insts size
				during: [ :barBlock | 1 to: insts size do: [:x | barBlock value: x. (insts at: x) rehash]]]]
]

{ #category : 'initialization' }
Set class >> rehashAllSets  "Set rehashAllSets" [
	self withAllSubclassesDo:
		[:c |
			| insts |
			insts := c allInstances.
			insts isEmpty ifFalse:
			['Rehashing instances of ' , c name
				displayProgressAt: Sensor mousePoint
				from: 1 to: insts size
				during: [ :barBlock |
				1 to: insts size do:
					[ :x | barBlock value: x.
					(insts at: x) rehash]]]]
]

{ #category : 'instance creation' }
Set class >> sizeFor: nElements [
	"Large enough size to hold nElements with some slop (see fullCheck)"
	nElements <= 0 ifTrue: [^ 1].
	^ nElements+1*4//3
]

{ #category : 'comparing' }
Set >> = aSet [

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	(aSet is: #Set) ifFalse: [^ false].
	(aSet is: #Dictionary) ifTrue: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true
]

{ #category : 'adding' }
Set >> add: newObject [
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: newObject.
	(array at: index) ifNil: [self atNewIndex: index put: newObject].
	^ newObject
]

{ #category : 'adding' }
Set >> add: newObject withOccurrences: anInteger [
	^ self add: newObject
]

{ #category : 'private' }
Set >> array [
	^ array
]

{ #category : 'private' }
Set >> atNewIndex: index put: anObject [
	array at: index put: anObject.
	tally := tally + 1.
	self fullCheck.
	^ anObject
]

{ #category : 'accessing' }
Set >> atRandom: aGenerator  [
	"Answer a random element of the receiver.  Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator.  Causes an error if self has no  
	elements.
	WARNING: This implementation cannot guarantee reproducibility.
	Calling twice with generators setup with the same seed can produce
	different results (for example if the collection is rehashed between calls).
	If you need reproducibility, you should convert the collection to a sequenceable
	collection first."
	self emptyCheck.
	[(array atRandom: aGenerator) ifNotNil: [:element| ^ element]] repeat
]

{ #category : 'enumerating' }
Set >> collect: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet := self species newWithRoomForMoreThan: self size.
	array do: [ :each | each ifNotNil: [ newSet add: (aBlock value: each)]].
	^ newSet
]

{ #category : 'object serialization' }
Set >> comeFullyUpOnReload: smartRefStream [
	"Symbols have new hashes in this image."

	self rehash.
	"^ self"

]

{ #category : 'removing' }
Set >> copyWithout: oldElement  [
	"Answer a copy of the receiver that does not contain any elements equal
	to oldElement."
	^ self copy
		remove: oldElement ifAbsent: nil;
		yourself
]

{ #category : 'enumerating' }
Set >> do: aBlock  [
	| each |
	tally = 0 ifTrue: [^ self].
	1 to: array size do: [ :index |
		(each := array at: index) ifNotNil: [aBlock value: each]]
]

{ #category : 'comparing' }
Set >> emptyCollectionHash [
	^ Set hash
]

{ #category : 'private' }
Set >> findElementOrNil: anObject [
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."
	| index |
	index := self scanFor: anObject.
	index > 0 ifTrue: [ ^ index ].

	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	self error: 'There is no free space in this set!'.
]

{ #category : 'private' }
Set >> fixCollisionsFrom: index [
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| length oldIndex newIndex element |
	oldIndex := index.
	length := array size.
	[oldIndex = length
			ifTrue: [oldIndex := 1]
			ifFalse: [oldIndex := oldIndex + 1].
	(element := self keyAt: oldIndex) == nil]
		whileFalse: [
			newIndex := self findElementOrNil: element.
			oldIndex = newIndex ifFalse: [self privateSwapArrayElement: oldIndex with: newIndex]].
]

{ #category : 'private' }
Set >> fullCheck [
	"Keep array at least 1/4 free for decent hash behavior"
	array size - tally < (array size // 4 max: 1)
		ifTrue: [self grow]
]

{ #category : 'private' }
Set >> grow [
	"Grow the elements array and reinsert the old elements"
	| oldElements |
	oldElements := array.
	array := Array new: array size + self growSize.
	tally := 0.
	oldElements do: [ :each |
		each ifNotNil: [self noCheckAdd: each]]
]

{ #category : 'private' }
Set >> growSize [
	^ array size max: 2
]

{ #category : 'testing' }
Set >> includes: anObject  [
	^ (self lookup: anObject) ~~ nil
]

{ #category : 'private' }
Set >> init: n [
	"Initialize array to an array size of n"
	array := Array new: n.
	tally := 0
]

{ #category : 'inspecting' }
Set >> inspectorClass  [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ SetInspector
]

{ #category : 'testing' }
Set >> is: aSymbol [
	^aSymbol == #Set or: [ super is: aSymbol ]
]

{ #category : 'private' }
Set >> keyAt: index [
	"May be overridden by subclasses so that fixCollisionsFrom: will work"
	^ array at: index
]

{ #category : 'accessing' }
Set >> like: anObject [
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| index |

	^(index := self scanFor: anObject) = 0
		ifFalse: [array at: index]
]

{ #category : 'lookup' }
Set >> lookup: anObject [
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise."

	^ array at: (self findElementOrNil: anObject).
]

{ #category : 'private' }
Set >> noCheckAdd: anObject [
	array at: (self findElementOrNil: anObject) put: anObject.
	tally := tally + 1
]

{ #category : 'testing' }
Set >> occurrencesOf: anObject  [
	^ (self includes: anObject) ifTrue: [1] ifFalse: [0]
]

{ #category : 'copying' }
Set >> postCopy [

	array := array copy
]

{ #category : 'private' }
Set >> privateSwapArrayElement: oneIndex with: otherIndex [
	"May be overridden by subclasses so that fixCollisionsFrom: will work"

	array swap: oneIndex with: otherIndex.
]

{ #category : 'private' }
Set >> rehash [
	| newSelf |
	newSelf := self species newWithRoomForMoreThan: self size.
	self do: [:each | newSelf noCheckAdd: each].
	array := newSelf array
]

{ #category : 'removing' }
Set >> remove: oldObject ifAbsent: aBlock [

	| index |
	index := self findElementOrNil: oldObject.
	(array at: index) ifNil: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject
]

{ #category : 'removing' }
Set >> removeAll [
	self init: array size
]

{ #category : 'private' }
Set >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : 'accessing' }
Set >> size [
	^tally
]

{ #category : 'enumerating' }
Set >> union: aCollection [
	"Answer the set theoretic union of the receiver and aCollection, using the receiver's notion of equality and not side effecting the receiver at all."

	^ self copy addAll: aCollection; yourself


]
