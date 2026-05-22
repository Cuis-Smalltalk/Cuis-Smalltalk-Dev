"
I represent a collection of objects ordered by some property of the objects themselves. The ordering is specified in a BlockClosure.
"
Class {
	#name : 'SortedCollection',
	#superclass : 'OrderedCollection',
	#instVars : [
		'sortBlock'
	],
	#category : 'Collections-Sequenceable',
	#package : 'Collections',
	#tag : 'Sequenceable'
}

{ #category : 'instance creation' }
SortedCollection class >> sortBlock: aBlock  [
	"Answer an instance of me such that its elements are sorted according to 
	the criterion specified in aBlock."

	^ self new sortBlock: aBlock
]

{ #category : 'comparing' }
SortedCollection >> = aSortedCollection [
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	"Any object is equal to itself"
	self == aSortedCollection ifTrue: [ ^ true ].

	"species is not enough. a SortedCollection is never equal to an OrderedCollection"
	self class == aSortedCollection class ifFalse: [ ^ false ].

	sortBlock = aSortedCollection sortBlock
		ifFalse: [ ^false ].

	^self hasEqualElements: aSortedCollection 
]

{ #category : 'adding' }
SortedCollection >> add: newObject [
	^ super insert: newObject before: (self indexForInserting: newObject)
]

{ #category : 'adding' }
SortedCollection >> addAll: aCollection [
	"optimization"
	aCollection size > (self size // 3)
		ifTrue:
			[aCollection do: [:each | self addLast: each].
			self reSort]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection
]

{ #category : 'adding' }
SortedCollection >> addFirst: newObject [
	self shouldNotImplement
]

{ #category : 'accessing' }
SortedCollection >> at: anInteger put: anObject [
	self shouldNotImplement
]

{ #category : 'copying' }
SortedCollection >> copy [

	^self shallowCopy postCopy
]

{ #category : 'private' }
SortedCollection >> defaultSort: i to: j  [
	"Sort elements i through j of self to be nondescending according to
	sortBlock."	"Assume the default sort block ([:x :y | x <= y])."

	| di dij dj tt ij k l n |
	"The prefix d means the data at that index."
	(n := j + 1  - i) <= 1 ifTrue: [^self].	"Nothing to sort." 
	 "Sort di,dj."
	di := array at: i.
	dj := array at: j.
	(di <= dj) "i.e., should di precede dj?"
		ifFalse: 
			[array swap: i with: j.
			 tt := di.
			 di := dj.
			 dj := tt].
	n > 2
		ifTrue:  "More than two elements."
			[ij := (i + j) // 2.  "ij is the midpoint of i and j."
			 dij := array at: ij.  "Sort di,dij,dj.  Make dij be their median."
			 (di <= dij) "i.e. should di precede dij?"
			   ifTrue: 
				[(dij <= dj) "i.e., should dij precede dj?"
				  ifFalse: 
					[array swap: j with: ij.
					 dij := dj]]
			   ifFalse:  "i.e. di should come after dij"
				[array swap: i with: ij.
				 dij := di].
			n > 3
			  ifTrue:  "More than three elements."
				["Find k>i and l<j such that dk,dij,dl are in reverse order.
				Swap k and l.  Repeat this procedure until k and l pass each other."
				 k := i.
				 l := j.
				 [[l := l - 1.  k <= l and: [dij <= (array at: l)]]
				   whileTrue.  "i.e. while dl succeeds dij"
				  [k := k + 1.  k <= l and: [(array at: k) <= dij]]
				   whileTrue.  "i.e. while dij succeeds dk"
				  k <= l]
				   whileTrue:
					[array swap: k with: l]. 
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
				self defaultSort: i to: l.
				self defaultSort: k to: j]]
]

{ #category : 'private' }
SortedCollection >> indexForInserting: newObject [

	| index low high |
	low := firstIndex.
	high := lastIndex.
	sortBlock
		ifNil: [
			[index := high + low // 2.  low > high]
				whileFalse: [
					((array at: index) <= newObject)
						ifTrue: [low := index + 1]
						ifFalse: [high := index - 1]]]
		ifNotNil: [
			[index := high + low // 2.  low > high]
				whileFalse: [
					(sortBlock value: (array at: index) value: newObject)
						ifTrue: [low := index + 1]
						ifFalse: [high := index - 1]]].
	^low
]

{ #category : 'accessing' }
SortedCollection >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock [
	| sortBlockToUse firstCandidate |
	self isEmpty ifTrue: [^ exceptionBlock value].
	sortBlockToUse := sortBlock ifNil: [[:a :b | a <= b ]].
	"if first element to check is it, answer so."
	firstCandidate := self at: start.
	firstCandidate = anElement ifTrue: [ ^ start ].
	"if first element to check is already too late, answer 'NotHere' "
	(sortBlockToUse value: anElement value: firstCandidate) ifTrue: [
		^ exceptionBlock value ].
	"Ok. Look for it."
	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ (sortBlockToUse value: anElement value: some) ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | exceptionBlock value ]
]

{ #category : 'private' }
SortedCollection >> insert: anObject before: spot [
	self shouldNotImplement
]

{ #category : 'copying' }
SortedCollection >> postCopy [

	array := array copy.
	firstIndex := firstIndex copy.
	lastIndex := lastIndex copy
]

{ #category : 'accessing' }
SortedCollection >> quickIndexOf: anElement  [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	| sortBlockToUse |
	sortBlockToUse := sortBlock ifNil: [[:a :b | a <= b ]].
	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ (sortBlockToUse value: anElement value: some) ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | 0 ]
]

{ #category : 'private' }
SortedCollection >> reSort [

	firstIndex < lastIndex ifTrue: [ 
		array quickSortFrom: firstIndex to: lastIndex by: sortBlock ]
]

{ #category : 'enumerating' }
SortedCollection >> select: aBlock  [
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	"Use self class and not self species. SortedCollection is special,
	the answer for #collect: (an OrderedCollection) is of different kind than
	the answer for #select: or #copy"
	newCollection := self class sortBlock: sortBlock.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element := array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection
]

{ #category : 'accessing' }
SortedCollection >> sortBlock [
	"Answer the BlockClosure which is the criterion for sorting elements of 
	the receiver."

	^sortBlock
]

{ #category : 'accessing' }
SortedCollection >> sortBlock: aBlock  [
	"Make the argument, aBlock, be the criterion for ordering elements of the 
	receiver."

	sortBlock := aBlock.
	self size > 0 ifTrue: [ self reSort ]
]

{ #category : 'private' }
SortedCollection >> species [
	"aSortedCollection collect: should answer an OrderedCollection"

	^OrderedCollection
]
