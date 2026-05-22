"
I represent an unordered collection of possibly duplicate elements.
	
I store these elements in a dictionary, tallying up occurrences of equal objects. Because I store an occurrence only once, my clients should beware that objects they store will not necessarily be retrieved such that == is true. If the client cares, a subclass of me should be created.
"
Class {
	#name : 'Bag',
	#superclass : 'Collection',
	#instVars : [
		'contents'
	],
	#category : 'Collections-Unordered',
	#package : 'Collections',
	#tag : 'Unordered'
}

{ #category : 'instance creation' }
Bag class >> contentsClass [
	^Dictionary
]

{ #category : 'instance creation' }
Bag class >> new [
	^ self basicNew setContents: self contentsClass new
]

{ #category : 'instance creation' }
Bag class >> newFrom: aCollection [
	"Create a new collection containing all the elements from aCollection"

	^ self new
		addAll: aCollection;
		yourself
]

{ #category : 'comparing' }
Bag >> = aBag [
	"Two bags are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same size.
	 (c) each element occurs the same number of times in both of them"

	self == aBag ifTrue: [ ^ true ].		"Any object is equal to itself"
	(aBag is: #Bag) ifFalse: [ ^ false ].
	self size = aBag size ifFalse: [ ^ false ].
	contents associationsDo: [ :assoc |
		(aBag occurrencesOf: assoc key) = assoc value
			ifFalse: [ ^ false ]].
	^true
]

{ #category : 'adding' }
Bag >> add: newObject  [
	^self add: newObject withOccurrences: 1
]

{ #category : 'adding' }
Bag >> add: newObject withOccurrences: anInteger  [
	"Add the element newObject to the receiver. Do so as though the element  
	were added anInteger number of times. Answer newObject."
	contents at: newObject put: (contents at: newObject ifAbsent: [0])
			+ anInteger.
	^ newObject
]

{ #category : 'adding' }
Bag >> addAll: aCollection [
	"Refer to the comment in Collection|addAll:."

	(aCollection is: #Bag) ifFalse: [^ super addAll: aCollection].
	aCollection contents associationsDo: [ :each |
		self add: each key withOccurrences: each value].
	^ aCollection
]

{ #category : 'enumerating' }
Bag >> allSatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for any element return false.
	Otherwise return true."

	contents keysDo: [:each | (aBlock value: each) ifFalse: [^ false]].
	^ true
]

{ #category : 'enumerating' }
Bag >> anySatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns true for any element return true.
	Otherwise return false"

	contents keysDo: [:each| (aBlock value: each) ifTrue: [^ true]].
	^ false
]

{ #category : 'enumerating' }
Bag >> asSet [
	"Answer a set with the elements of the receiver"

	 ^contents keys asSet
]

{ #category : 'accessing' }
Bag >> at: index  [
	self errorNotKeyed
]

{ #category : 'accessing' }
Bag >> at: index put: anObject [
	self errorNotKeyed
]

{ #category : 'accessing' }
Bag >> atRandom: aGenerator  [
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
	| n |
	self emptyCheck.
	n := self size atRandom: aGenerator.
	self withOccurrencesDo: [:each :times| (n := n - times) <= 0 ifTrue: [^ each]].
	self error: 'collection invariants broken'
]

{ #category : 'enumerating' }
Bag >> collect: aBlock  [
	"Evaluate aBlock with each of the receiver's elements as the argument. Collect the resulting values into a collection that is like the receiver and nswer the new collection."
	| answer |
	answer := self species new.
	self withOccurrencesDo: [:each :times| answer add: (aBlock value: each) withOccurrences: times].
	^ answer
]

{ #category : 'private' }
Bag >> contents [
	^contents

]

{ #category : 'enumerating' }
Bag >> count: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."
	| count |
	count := 0.
	self withOccurrencesDo: [:each :times| (aBlock value: each) ifTrue: [count := count + times]].
	^ count
]

{ #category : 'accessing' }
Bag >> cumulativeCounts [
	"Answer with a collection of cumulative percents covered by elements so far."
	| s n |
	s := self size / 100.0.
	n := 0.
	^ self sortedCounts asArray collect: [ :a |
		n := n + a key.
		(n / s roundTo: 0.1) -> a value]
]

{ #category : 'enumerating' }
Bag >> detect: aBlock ifNone: exceptionBlock  [
	"Refer to the comment in Collection|detect:ifNone:."

	contents keysDo: [:each | (aBlock value: each) ifTrue: [^ each]].
	^ exceptionBlock value
]

{ #category : 'enumerating' }
Bag >> do: aBlock  [
	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]
]

{ #category : 'testing' }
Bag >> includes: anObject  [
	^contents includesKey: anObject
]

{ #category : 'testing' }
Bag >> is: aSymbol [
	^ aSymbol == #Bag or: [ super is: aSymbol ]
]

{ #category : 'statistics' }
Bag >> max: aBlock [
	"Answer the maximum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	contents keysDo: [:each| answer := answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer
]

{ #category : 'statistics' }
Bag >> min: aBlock [
	"Answer the minimum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	contents keysDo: [:each| answer := answer isNil ifTrue: [aBlock value: each] ifFalse: [answer min: (aBlock value: each)]].
	^ answer
]

{ #category : 'testing' }
Bag >> occurrencesOf: anObject  [
	(self includes: anObject)
		ifTrue: [^contents at: anObject]
		ifFalse: [^0]
]

{ #category : 'copying' }
Bag >> postCopy [

	contents := contents copy
]

{ #category : 'statistics' }
Bag >> product: aBlock [
	| answer |
	self emptyCheck.
	self withOccurrencesDo: [ :each :times | | v |
		v := aBlock value: each.
		answer := answer isNil
			ifTrue: [v raisedTo: times]
			ifFalse: [answer * (v raisedTo: times)]].
	^ answer
]

{ #category : 'removing' }
Bag >> remove: oldObject ifAbsent: exceptionBlock  [
	"Refer to the comment in Collection|remove:ifAbsent:."

	| count |
	count := contents at: oldObject ifAbsent: [^ exceptionBlock value].
	count = 1
		ifTrue: [contents removeKey: oldObject]
		ifFalse: [contents at: oldObject put: count - 1].
	^ oldObject
]

{ #category : 'enumerating' }
Bag >> select: aBlock  [
	"Refer to the comment in Collection|select:."

	| newCollection |
	newCollection := self species new.
	contents associationsDo: [:each |
		(aBlock value: each key)
			ifTrue: [newCollection add: each key withOccurrences: each value]].
	^ newCollection
]

{ #category : 'private' }
Bag >> setContents: aDictionary [
	contents := aDictionary
]

{ #category : 'accessing' }
Bag >> size [

	^contents inject: 0 into: [ :prevValue :each | prevValue + each ]
]

{ #category : 'accessing' }
Bag >> sortedCounts [
	"Answer with a collection of counts with elements, sorted by decreasing count."
	| counts |
	counts := SortedCollection sortBlock: [:x :y | x >= y].
	contents associationsDo:
		[:assn |
		counts add: (Association key: assn value value: assn key)].
	^ counts
]

{ #category : 'accessing' }
Bag >> sortedElements [
	"Answer with a collection of elements with counts, sorted by element."
	| elements |
	elements := SortedCollection new.
	contents associationsDo: [:assn | elements add: assn].
	^ elements
]

{ #category : 'statistics' }
Bag >> sum: aBlock ifEmpty: emptyBlockOrValue [
	| answer |
	self isEmpty ifTrue: [^ emptyBlockOrValue value].
	self withOccurrencesDo: [ :each :times | | v |
		v := aBlock value: each.
		answer := answer isNil
			ifTrue: [v * times]
			ifFalse: [v * times + answer]].
	^ answer
]

{ #category : 'enumerating' }
Bag >> withOccurrencesDo: aBlock [
	contents associationsDo: [:each| aBlock value: each key value: each value]
]
