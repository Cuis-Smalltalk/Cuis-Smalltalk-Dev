"
I represent a collection of objects ordered by the collector.
"
Class {
	#name : 'OrderedCollection',
	#superclass : 'SequenceableCollection',
	#instVars : [
		'array',
		'firstIndex',
		'lastIndex'
	],
	#category : 'Collections-Sequenceable',
	#package : 'Collections',
	#tag : 'Sequenceable'
}

{ #category : 'instance creation' }
OrderedCollection class >> new [
	^self basicNew setCollection: (Array new: 10)
]

{ #category : 'instance creation' }
OrderedCollection class >> newFrom: aCollection [
	"Create a new collection containing all the elements from aCollection"

	^self new
		resetTo: 1;
		addAll: aCollection;
		yourself
]

{ #category : 'stream creation' }
OrderedCollection class >> streamContents: blockWithArg [

	^self streamContents: blockWithArg on: self new
]

{ #category : 'stream creation' }
OrderedCollection class >> streamContents: blockWithArg estimatedSize: estimatedSize [
	"Compatibility. Simply ignore size hint."

	^self streamContents: blockWithArg
]

{ #category : 'comparing' }
OrderedCollection >> = otherCollection  [
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."


	"species is not enough. a SortedCollection is never equal to an OrderedCollection"
	self == otherCollection ifTrue: [^ true].
	self class == otherCollection class ifFalse: [^ false].
	^ self hasEqualElements: otherCollection
]

{ #category : 'adding' }
OrderedCollection >> add: newObject  [
	^self addLast: newObject
]

{ #category : 'adding' }
OrderedCollection >> add: newObject after: oldObject  [
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just succeeding oldObject.  Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index + 1.
	^newObject
]

{ #category : 'adding' }
OrderedCollection >> add: newObject afterIndex: index  [
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just after index. Answer newObject."
	(index between: 0 and: self size) ifFalse:[^self errorSubscriptBounds: index].
	self insert: newObject before: firstIndex + index.
	^ newObject
]

{ #category : 'adding' }
OrderedCollection >> add: newObject before: oldObject  [
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just preceding oldObject.  Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index.
	^newObject
]

{ #category : 'adding' }
OrderedCollection >> add: newObject beforeIndex: index  [
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just before index. Answer newObject."
	(index between: 1 and: self size+1) ifFalse:[^self errorSubscriptBounds: index].
	self insert: newObject before: firstIndex + index - 1.
	^ newObject
]

{ #category : 'adding' }
OrderedCollection >> addAllFirst: anOrderedCollection  [
	"Add each element of anOrderedCollection at the beginning of the receiver.	
	Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection
]

{ #category : 'adding' }
OrderedCollection >> addAllLast: aCollection  [
	"Add each element of aCollection at the end of me. Answer aCollection."

	^aCollection do: [ :each | self addLast: each ]
]

{ #category : 'adding' }
OrderedCollection >> addFirst: newObject  [
	"Add newObject to the beginning of the receiver. Answer newObject."

	firstIndex = 1 ifTrue: [self makeRoomAtFirst].
	firstIndex := firstIndex - 1.
	array at: firstIndex put: newObject.
	^ newObject
]

{ #category : 'adding' }
OrderedCollection >> addLast: newObject  [
	"Add newObject to the end of the receiver. Answer newObject."

	lastIndex = array size ifTrue: [self makeRoomAtLast].
	lastIndex := lastIndex + 1.
	array at: lastIndex put: newObject.
	^ newObject
]

{ #category : 'converting' }
OrderedCollection >> asNewArray [
	^ array copyFrom: firstIndex to: lastIndex
]

{ #category : 'accessing' }
OrderedCollection >> at: anInteger  [
	"Answer my element at index anInteger. at: is used by a knowledgeable
	client to access an existing element"

	(anInteger < 1 or: [anInteger + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^ array at: anInteger + firstIndex - 1]
]

{ #category : 'adding' }
OrderedCollection >> at: index ifAbsentPut: block [
	"Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)"

	| v |
	index <= self size ifTrue: [
		^ (v := self at: index)
			ifNotNil: [v]
			ifNil: [self at: index put: block value]
	].
	[self size < index] whileTrue: [self add: nil].
	^ self at: index put: block value
]

{ #category : 'accessing' }
OrderedCollection >> at: anInteger put: anObject  [
	"Put anObject at element index anInteger. at:put: cannot be used to
	append, front or back, to an ordered collection; it is used by a
	knowledgeable client to replace an element."

	| index |
	index := anInteger asInteger.
	(index < 1 or: [index + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^array at: index + firstIndex - 1 put: anObject]
]

{ #category : 'enumerating' }
OrderedCollection >> collect: collectBlock thenSelect: selectBlock [
	"Evaluate collectBlock with each my elements as the argument. Collect the 
	resulting values into a collection that is like me, but only those elements for which
	selectBlock evaluates to true. Answer the new collection.
	Overriden for performance."

	| newCollection newElement |
	newCollection := self species new.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		newElement := collectBlock value: (array at: index).
		(selectBlock value: newElement)
			ifTrue: [ newCollection addLast: newElement ]].
	^ newCollection
]

{ #category : 'copying' }
OrderedCollection >> copy [

	^self copyFrom: 1 to: self size
]

{ #category : 'copying' }
OrderedCollection >> copyBlank: requestedSize [
	"Answer an instance like me, of size n, but without content.
	All entries are nil."

	^self species basicNew initializeOfSize: requestedSize
]

{ #category : 'copying' }
OrderedCollection >> copyFrom: startIndex to: endIndex [
	"Answer a copy of the receiver that contains elements from position startIndex to endIndex."
	| delta newArray |
	delta := firstIndex - 1.
	newArray := array copyFrom: startIndex + delta to: endIndex + delta.
	^ OrderedCollection basicNew setContents: newArray.
]

{ #category : 'copying' }
OrderedCollection >> copyReplaceFrom: start to: stop with: replacementCollection  [
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| newOrderedCollection startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front. 
	if start greater than self size, ignore stop and assume this is appending. 
	otherwise, it is replacing part of me and start and stop have to be within my 
	bounds. "
	startIndex := start.
	stopIndex := stop.
	start < 1
		ifTrue: [startIndex := stopIndex := 0]
		ifFalse: [startIndex > self size
				ifTrue: [startIndex := stopIndex := self size + 1]
				ifFalse: 
					[(stopIndex < (startIndex - 1) or: [stopIndex > self size])
						ifTrue: [ self errorOutOfBounds ]]].
	newOrderedCollection := self species new.
	1 to: startIndex - 1 do: [:index | newOrderedCollection add: (self at: index)].
	1 to: replacementCollection size do: 
		[:index | newOrderedCollection add: (replacementCollection at: index)].
	stopIndex + 1 to: self size do: [:index | newOrderedCollection add: (self at: index)].
	^newOrderedCollection
]

{ #category : 'enumerating' }
OrderedCollection >> do: aBlock  [

	firstIndex to: lastIndex do: [ :index |
		aBlock value: (array at: index) ]
]

{ #category : 'private' }
OrderedCollection >> errorNoSuchElement [
	self error: 'attempt to index non-existent element in an ordered collection'
]

{ #category : 'inspecting' }
OrderedCollection >> explorerContents [
	"See #indexHead and #indexTail"

	| answer indexHead indexTail total shown elementIndex |
	indexHead := 500.
	indexTail := 30.
	shown := indexHead + indexTail.
	total := self size.

	total <= shown ifTrue: [
		^super explorerContents ].

	answer := Array new: shown.
	1 to: indexHead do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: index)
				name: index printString
				model: self) ].

	elementIndex := total-indexTail+1.
	indexHead+1 to: shown do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: elementIndex)
				name: elementIndex printString
				model: self).
		elementIndex := elementIndex + 1 ].

	^answer
]

{ #category : 'private' }
OrderedCollection >> find: oldObject [
	"Answer an index in the range [firstIndex, lastIndex]"

	firstIndex to: lastIndex do: [ :index |
		(array at: index) = oldObject ifTrue: [ ^index ]].
	self errorNotFound: oldObject
]

{ #category : 'adding' }
OrderedCollection >> growSize [
	^ array size max: 2
]

{ #category : 'initialization' }
OrderedCollection >> initializeOfSize: aSize [

	array := Array new: aSize.
	firstIndex := 1.
	lastIndex := aSize.
]

{ #category : 'private' }
OrderedCollection >> insert: anObject before: spot [
	"Spot is an index in the range [firstIndex, lastIndex]"

	| delta spotIndex|
	spotIndex := spot.
	delta := spotIndex - firstIndex.
	firstIndex = 1 ifTrue: 
		[self makeRoomAtFirst.
		spotIndex := firstIndex + delta].
	firstIndex := firstIndex - 1.
	array
		replaceFrom: firstIndex
		to: spotIndex - 2
		with: array
		startingAt: firstIndex + 1.
	array at: spotIndex - 1 put: anObject.
	^anObject
]

{ #category : 'inspecting' }
OrderedCollection >> inspectorClass  [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector
]

{ #category : 'accessing' }
OrderedCollection >> isEmpty [
	^self size = 0
]

{ #category : 'private' }
OrderedCollection >> makeRoomAtFirst [

	| size newArray newFirst newLast lastGap writeGap |
	size := self size.
	lastGap := size - lastIndex.
	lastGap - 4 * 5 > size ifTrue:
		[
			newLast := size - (lastGap bitShift: -5).
			writeGap := newLast - lastIndex.
			writeGap > 0 ifTrue:
				[
					newFirst := firstIndex + writeGap.
					newLast to: newFirst by: -1 do:
						[:each | array at: each put: (array at: each - writeGap)].
					array from: firstIndex to: newFirst - 1 put: nil.
					firstIndex := newFirst.
					lastIndex := newLast.
					^self
				]
		].
	newLast := size + self growSize.
	newArray := Array new: newLast.
	newFirst := firstIndex + newLast - lastIndex.
	newArray replaceFrom: newFirst to: newLast with: array startingAt: firstIndex.
	array := newArray.
	firstIndex := newFirst.
	lastIndex := newLast
]

{ #category : 'private' }
OrderedCollection >> makeRoomAtLast [

	| size newArray newFirst newLast |
	size := self size.
	firstIndex - 4 * 5 > size ifTrue:
		[
			newFirst := (firstIndex bitShift: -5) + 1.
			newFirst < firstIndex ifTrue:
				[
					newLast := lastIndex - firstIndex + newFirst.
					array
						replaceFrom: newFirst
						to: newLast
						with: array
						startingAt: firstIndex.
					array from: newLast + 1 to: lastIndex put: nil.
					firstIndex := newFirst.
					lastIndex := newLast.
					^self
				]
		].
	newArray := Array new: size + self growSize.
	newLast := lastIndex - firstIndex + 1.
	newArray replaceFrom: 1 to: newLast with: array startingAt: firstIndex.
	array := newArray.
	firstIndex := 1.
	lastIndex := newLast
]

{ #category : 'removing' }
OrderedCollection >> remove: oldObject ifAbsent: absentBlock [

	firstIndex to: lastIndex do: [ :index |
		oldObject = (array at: index)
			ifTrue: [
				self removeIndex: index.
				^ oldObject ]].
	^ absentBlock value
]

{ #category : 'removing' }
OrderedCollection >> removeAll [

	array from: firstIndex to: lastIndex put: nil.
	lastIndex := firstIndex - 1
]

{ #category : 'removing' }
OrderedCollection >> removeAllSuchThat: aBlock  [
	"Remove each element of the receiver for which aBlock evaluates to true.
	The method in Collection is O(N^2), this is O(N)."

	| n |
	n := firstIndex.
	firstIndex to: lastIndex do: [:index |
	    (aBlock value: (array at: index)) ifFalse: [
			array at: n put: (array at: index).
			n := n + 1]].
	array from: n to: lastIndex put: nil.
	lastIndex := n - 1
]

{ #category : 'removing' }
OrderedCollection >> removeAt: index [
	| removed |
	removed := self at: index.
	self removeIndex: index + firstIndex - 1.
	^removed
]

{ #category : 'removing' }
OrderedCollection >> removeFirst [
	"Remove the first element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| firstObject |
	self emptyCheck.
	firstObject := array at: firstIndex.
	array at: firstIndex put: nil.
	firstIndex := firstIndex + 1.
	^ firstObject
]

{ #category : 'removing' }
OrderedCollection >> removeFirst: n [
	"Remove first n object into an array"

	| list |
	list := Array new: n.
	1 to: n do: [:i |
		list at: i put: self removeFirst].
	^ list
]

{ #category : 'private' }
OrderedCollection >> removeIndex: removedIndex [
 	"The removedIndex value is in the range [firstIndex, lastIndex]"

	array 
		replaceFrom: removedIndex 
		to: lastIndex - 1 
		with: array 
		startingAt: removedIndex+1.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1
]

{ #category : 'removing' }
OrderedCollection >> removeLast [
	"Remove the last element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| lastObject |
	self emptyCheck.
	lastObject := array at: lastIndex.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.
	^ lastObject
]

{ #category : 'removing' }
OrderedCollection >> removeLast: n [
	"Remove last n object into an array with last in last position"

	| list |
	list := Array new: n.
	n to: 1 by: -1 do: [:i |
		list at: i put: self removeLast].
	^ list
]

{ #category : 'private' }
OrderedCollection >> reset [
	firstIndex := array size // 3 max: 1.
	lastIndex := firstIndex - 1
]

{ #category : 'private' }
OrderedCollection >> resetTo: index [
	firstIndex := index.
	lastIndex := firstIndex - 1
]

{ #category : 'enumerating' }
OrderedCollection >> reverseDo: aBlock  [

	lastIndex to: firstIndex by: -1 do: [ :index |
		aBlock value: (array at: index) ]
]

{ #category : 'enumerating' }
OrderedCollection >> select: aBlock  [
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	newCollection := self species new.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element := array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection
]

{ #category : 'enumerating' }
OrderedCollection >> select: selectBlock thenCollect: collectBlock [
	"Evaluate selectBlock with each my elements as the argument. For those who evaluate to true,
	collect the result of evaluating collectBlock on them into a collection that is like me.
	Answer the new collection.
	Overriden for performance."

	| newCollection each |
	newCollection := self species new.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		each := array at: index.
		(selectBlock value: each) ifTrue: [
			newCollection addLast: (collectBlock value: each) ]].
	^ newCollection
]

{ #category : 'private' }
OrderedCollection >> setCollection: anArray [
	array := anArray.
	self reset
]

{ #category : 'private' }
OrderedCollection >> setContents: anArray [
	array := anArray.
	firstIndex := 1.
	lastIndex := array size.
]

{ #category : 'accessing' }
OrderedCollection >> size [
	^lastIndex - firstIndex + 1
]

{ #category : 'sorting' }
OrderedCollection >> sort [
	"Sort this array into ascending order using the '<=' operator."

	self sort: nil
]

{ #category : 'sorting' }
OrderedCollection >> sort: aSortBlock  [
	"Sort this collection using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one.
	If aSortBlock is nil then <= is used for comparison."

	self ifNotEmpty: [
		array
			mergeSortFrom: firstIndex
			to: lastIndex
			by: aSortBlock ]
]
