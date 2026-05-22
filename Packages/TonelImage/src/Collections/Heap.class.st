"
Class Heap implements a special data structure commonly referred to as 'heap' [ http://en.wikipedia.org/wiki/Heap_%28data_structure%29 ]
A Heap is a kind of binary tree stored in a linear array - see details after the instance variables description.

Heaps are good at handling priority queues because:
1) greatest priority element according to the sort block will be stored in first position and thus accessed in O(1) operations
2) worse time for inserting or removing an element is in O(log n) operations, where n is the size of the Heap
	Insertion/Removal times are more efficient than above upper bound, provided that:
	a) Elements are only removed at the beginning
	b) Elements are added with arbitrary sort order.
3) there is no need to fully sort the Heap, which makes it more efficient than a SortedCollection

The heap can be fully sorted by sending the message #fullySort.
Worse time for fully sorting the Heap is in O(n log n) operations, but this is rarely used a feature.
Remind that the Heap does not fully sort the collection if you don't ask.
Thus don't expect #do: and other iterators to enumerate elements according to the sortBlock order.

Instance variables:
	array		<Array>		The data repository
	tally		<Integer>	The number of elements in the heap
	sortBlock	<Block|nil>	A two-argument block defining the sort order,
							or nil in which case the default sort order is
								[:element1 :element2| element1 <= element2]
	indexUpdateBlock 	<Block|nil> 
							A two-argument block of the form [:data :index | ... ]
							which allows an application object to keep track of its
							index within the heap.  Useful for quick heap update
							when object's sort value changes (for example, when an
							object in a priority queue has its priority increased
							by an external event, you don't want to have to search
							through the whole heap to find the index before fixing
							the heap).  No update occurs if nil.
									
The Heap can be viewed as a binary tree (every node in the tree has at most two children).
The root is stored in first slot of internal array.
The children are stored in next two slots.
The children of children in next four slots.
etc...
For a node A of index i (1 based), the two children B1 and B2 are thus stored in indices (2*i) and (2*i+1).
Of course, the children indices must be less than the tally otherwise they are considered inexistent.

The Heap does arrange to preserve the following invariant:
For any children B of a node A, A is sorted before B, in other words, (self sort: A before: B) = true
This implies that the root is always the first element according to sort order.


"
Class {
	#name : 'Heap',
	#superclass : 'SequenceableCollection',
	#instVars : [
		'array',
		'tally',
		'sortBlock',
		'indexUpdateBlock'
	],
	#category : 'Collections-Sequenceable',
	#package : 'Collections',
	#tag : 'Sequenceable'
}

{ #category : 'examples' }
Heap class >> heapExample	"Heap heapExample" [
	"Create a sorted collection of numbers, remove the elements
	sequentially and add new objects randomly.
	Note: This is the kind of benchmark a heap is designed for."
	| n rnd array time |
	n := 5000. "# of elements to sort"
	rnd := Random new.
	array := (1 to: n) collect:[:i| rnd next].
	"First, the heap version"
	time := Time millisecondsToRun:[| sorted |
		sorted := Heap withAll: array.
		1 to: n do: [ :i | 
			sorted removeFirst.
			sorted add: rnd next].
	].
	Transcript newLine; show:'Time for Heap: ', time printString,' msecs'.
	"The quicksort version"
	time := Time millisecondsToRun:[| sorted |
		sorted := SortedCollection withAll: array.
		1 to: n do:[:i| 
			sorted removeFirst.
			sorted add: rnd next].
	].
	Transcript newLine; show:'Time for SortedCollection: ', time printString,' msecs'.

]

{ #category : 'examples' }
Heap class >> heapSortExample	"Heap heapSortExample" [
	"Sort a random collection of Floats and compare the results with
	SortedCollection (using the quick-sort algorithm) and 
	ArrayedCollection>>mergeSortFrom:to:by: (using the merge-sort algorithm)."
	| n rnd array time |
	n := 10000. "# of elements to sort"
	rnd := Random new.
	array := (1 to: n) collect:[:i| rnd next].
	"First, the heap version"
	time := Time millisecondsToRun: [
		| sorted |
		sorted := Heap withAll: array.
		1 to: n do: [ :i | sorted removeFirst ]].
	Transcript newLine; show:'Time for heap-sort: ', time printString,' msecs'.
	"The quicksort version"
	time := Time millisecondsToRun:[| sorted |
		sorted := SortedCollection withAll: array.
	].
	Transcript newLine; show:'Time for quick-sort: ', time printString,' msecs'.
	"The merge-sort version"
	time := Time millisecondsToRun:[
		array mergeSortFrom: 1 to: array size by: [:v1 :v2| v1 <= v2].
	].
	Transcript newLine; show:'Time for merge-sort: ', time printString,' msecs'.

]

{ #category : 'instance creation' }
Heap class >> new [
	^self basicNew setCollection: (Array new: 10)
]

{ #category : 'instance creation' }
Heap class >> newFrom: aCollection [
	"Create a new heap with all the elements from aCollection"
	^(self basicNew)
		setCollection: aCollection asArray copy tally: aCollection size;
		reSort;
		yourself
]

{ #category : 'instance creation' }
Heap class >> sortBlock: aBlock [
	"Create a new heap sorted by the given block"
	^self new sortBlock: aBlock
]

{ #category : 'instance creation' }
Heap class >> withAll: aCollection sortBlock: sortBlock [
	"Create a new heap with all the elements from aCollection"
	^(self basicNew)
		setCollection: aCollection asArray copy tally: aCollection size;
		sortBlock: sortBlock;
		yourself
]

{ #category : 'comparing' }
Heap >> = otherCollection [

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].

	sortBlock = otherCollection sortBlock
		ifFalse: [ ^false ].

	^self hasEqualElements: otherCollection 
]

{ #category : 'adding' }
Heap >> add: anObject [
	"Include newObject as one of the receiver's elements. Answer newObject."
	tally = array size ifTrue:[self grow].
	array at: (tally := tally + 1) put: anObject.
	self updateObjectIndex: tally.
	self upHeap: tally.
	^anObject
]

{ #category : 'private' }
Heap >> array [
	^array
]

{ #category : 'accessing' }
Heap >> at: index [
	"Return the element at the given position within the receiver"
	(index < 1 or:[index > tally]) ifTrue:[^self errorSubscriptBounds: index].
	^array at: index
]

{ #category : 'accessing' }
Heap >> at: index put: newObject [
	"Heaps are accessed with #add: not #at:put:"
	^self shouldNotImplement
]

{ #category : 'enumerating' }
Heap >> do: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument."
	1 to: tally do:[:i| aBlock value: (array at: i)]
]

{ #category : 'private-heap' }
Heap >> downHeap: anIndex [
	"Check the heap downwards for correctness starting at anIndex.
	 Everything above (i.e. left of) anIndex is ok."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[ j := j + 1].
		"check if position k is ok"
		(self sorts: value before: (array at: j)) 
			ifTrue:[	"yes -> break loop"
					n := k - 1]
			ifFalse:[	"no -> make room at j by moving j-th element to k-th position"
					array at: k put: (array at: j).
					self updateObjectIndex: k.
					"and try again with j"
					k := j]].
	array at: k put: value.
	self updateObjectIndex: k.
]

{ #category : 'private-heap' }
Heap >> downHeapSingle: anIndex [
	"This version is optimized for the case when only one element in the receiver can be at a wrong position. It avoids one comparison at each node when travelling down the heap and checks the heap upwards after the element is at a bottom position. Since the probability for being at the bottom of the heap is much larger than for being somewhere in the middle this version should be faster."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[	j := j + 1].
		array at: k put: (array at: j).
		self updateObjectIndex: k.
		"and try again with j"
		k := j].
	array at: k put: value.
	self updateObjectIndex: k.
	self upHeap: k
]

{ #category : 'accessing' }
Heap >> first [
	"Return the first element in the receiver"
	^array at: 1
]

{ #category : 'accessing' }
Heap >> fullySort [
	"Fully sort the heap.
	This method preserves the heap invariants and can thus be sent safely"
	self privateReverseSort.
	1 to: tally // 2 do: [:i | array swap: i with: 1 + tally - i]
]

{ #category : 'growing' }
Heap >> grow [
	"Become larger."
	self growTo: self size + self growSize.
]

{ #category : 'growing' }
Heap >> growSize [
	"Return the size by which the receiver should grow if there are no empty slots left."
	^array size max: 5
]

{ #category : 'growing' }
Heap >> growTo: newSize [
	"Grow to the requested size."
	| newArray |
	newArray := Array new: (newSize max: tally).
	newArray replaceFrom: 1 to: array size with: array startingAt: 1.
	array := newArray
]

{ #category : 'accessing' }
Heap >> indexUpdateBlock: aBlockOrNil [

	indexUpdateBlock := aBlockOrNil.


]

{ #category : 'testing' }
Heap >> isEmpty [
	"Answer whether the receiver contains any elements."
	^tally = 0
]

{ #category : 'copying' }
Heap >> postCopy [

	array := array copy.
	tally := tally copy
]

{ #category : 'private' }
Heap >> privateRemoveAt: index [
	"Remove the element at the given index and make sure the sorting order is okay"
	| removed |
	removed := array at: index.
	array at: index put: (array at: tally).
	array at: tally put: nil.
	tally := tally - 1.
	index > tally ifFalse:[
		"Use #downHeapSingle: since only one element has been removed"
		self downHeapSingle: index].
	^removed
]

{ #category : 'private' }
Heap >> privateReverseSort [
	"Arrange to have the array sorted in reverse order.
	WARNING: this method breaks the heap invariants. It's up to the sender to restore them afterwards."
	| oldTally |
	oldTally := tally.
	[tally > 1] whileTrue:
		[array swap: 1 with: tally.
		tally := tally - 1.
		self downHeapSingle: 1].
	tally := oldTally
]

{ #category : 'accessing' }
Heap >> reSort [
	"Resort the entire heap"
	self isEmpty ifTrue:[^self].
	tally // 2 to: 1 by: -1 do:[:i| self downHeap: i].
]

{ #category : 'removing' }
Heap >> remove: oldObject ifAbsent: aBlock [
	"Remove oldObject as one of the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject."
	1 to: tally do:[:i| 
		(array at: i) = oldObject ifTrue:[^self privateRemoveAt: i]].
	^aBlock value
]

{ #category : 'removing' }
Heap >> removeAll [

	array atAllPut: nil.
	tally := 0
]

{ #category : 'removing' }
Heap >> removeAt: index [
	"Remove the element at given position"
	(index < 1 or:[index > tally]) ifTrue:[^self errorSubscriptBounds: index].
	^self privateRemoveAt: index
]

{ #category : 'removing' }
Heap >> removeFirst [
	"Remove the first element from the receiver"
	^self removeAt: 1
]

{ #category : 'enumerating' }
Heap >> select: aBlock  [
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| answer |
	answer := self species sortBlock: sortBlock.
	self do: [ :each |
		(aBlock value: each)
			ifTrue: [ answer add: each ]].
	^ answer
]

{ #category : 'private' }
Heap >> setCollection: aCollection [
	array := aCollection.
	tally := 0.
]

{ #category : 'private' }
Heap >> setCollection: aCollection tally: newTally [
	array := aCollection.
	tally := newTally.
]

{ #category : 'accessing' }
Heap >> size [
	"Return the number of elements in the receiver"
	^tally
]

{ #category : 'accessing' }
Heap >> sortBlock [
	^sortBlock
]

{ #category : 'accessing' }
Heap >> sortBlock: aBlock [
	sortBlock := aBlock.
	self reSort
]

{ #category : 'testing' }
Heap >> sorts: element1 before: element2 [
	"Return true if element1 should be sorted before element2.
	This method defines the sort order in the receiver"
	^ sortBlock
		ifNil: [element1 <= element2]
		ifNotNil: [sortBlock value: element1 value: element2]
]

{ #category : 'growing' }
Heap >> trim [
	"Remove any empty slots in the receiver."
	self growTo: self size.
]

{ #category : 'private-heap' }
Heap >> upHeap: anIndex [
	"Check the heap upwards for correctness starting at anIndex.
	 Everything below anIndex is ok."
	| value k kDiv2 tmp |
	anIndex = 0 ifTrue:[^self].
	k := anIndex.
	value := array at: anIndex.
	[ (k > 1) and:[self sorts: value before: (tmp := array at: (kDiv2 := k bitShift: -1))] ] 
		whileTrue:[
			array at: k put: tmp.
			self updateObjectIndex: k.
			k := kDiv2].
	array at: k put: value.
	self updateObjectIndex: k.
]

{ #category : 'private' }
Heap >> updateObjectIndex: index [
	"If indexUpdateBlock is not nil, notify the object at index of its new position in the heap array."
	indexUpdateBlock ifNotNil: [
		indexUpdateBlock value: (array at: index) value: index]
]
