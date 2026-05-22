Class {
	#name : 'SequenceableCollectionTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'tests' }
SequenceableCollectionTest >> testCombinationsAtATimeDoShouldEvaluateBlockWithEmptyCollection [
	
	| combinationWasEmpty |
	
	combinationWasEmpty := false.
	'ab' combinations: 0 atATimeDo: [ :combination | combinationWasEmpty := combination isEmpty ].
	
	self assert: combinationWasEmpty
]

{ #category : 'tests' }
SequenceableCollectionTest >> testCombinationsAtATimeDoWorksAsExpected [
	
	| combinations |
	 
	combinations := OrderedCollection new.
	'abc' combinations: 2 atATimeDo: [ :combination | combinations add: combination copy].
	
	self assert: combinations size equals: 3.
	self assert: (combinations includes: #($a $b)).
	self assert: (combinations includes: #($a $c)).
	self assert: (combinations includes: #($b $c)).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testDoSeparatedByDoesNotValueSeparatedBlockForCollectionsOfOneElement [

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: 1.
	traversedElements := OrderedCollection new.
	self shouldntFail: [ collectionToTraverse do: [ :anElement | traversedElements add: anElement ] separatedBy: [ self fail ] ].

	self assert: traversedElements equals: collectionToTraverse.
]

{ #category : 'tests' }
SequenceableCollectionTest >> testDoSeparatedByDoesNotValueSeparatedBlockOnLastElement [

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: 1 with: 2.
	traversedElements := OrderedCollection new.

	self shouldntFail: [ collectionToTraverse do: [ :anElement | traversedElements add: anElement ] separatedBy: [ traversedElements add: $-] ].

	self assert: traversedElements equals: (OrderedCollection with: 1 with: $- with: 2).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testDoSeparatedByDoesNothingForEmptyCollection [

	self shouldntFail: [ #() do: [ :anElement | self fail ] separatedBy: [ self fail ] ]
]

{ #category : 'tests' }
SequenceableCollectionTest >> testFirstAvailableReturnsAllObjectsWhenRequestedMoreThanTheSize [

	self assert: (#(10 20 30) firstAvailable: 4) equals:  #(10 20 30).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testFirstAvailableReturnsAllObjectsWhenRequestedTheReceiversSize [

	self assert: (#(10 20 30) firstAvailable: 3) equals: #(10 20 30).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testFirstAvailableReturnsTheFirstRequestedObjectsWhenTheyAreAvailable [

	self assert: (#(10 20 30) firstAvailable: 2) equals: #(10 20).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testLastAvailableReturnsAllObjectsWhenRequestedMoreThanTheSize [

	self assert: (#(10 20 30) lastAvailable: 4) equals: #(10 20 30).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testLastAvailableReturnsAllObjectsWhenRequestedTheReceiversSize [

	self assert: (#(10 20 30) lastAvailable: 3) equals: #(10 20 30).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testLastAvailableReturnsTheLastRequestedObjectsWhenTheyAreAvailable [

	self assert: (#(10 20 30) lastAvailable: 2) equals: #(20 30).
]

{ #category : 'tests' }
SequenceableCollectionTest >> testWithAllSatisfyReturnsFalseWhenOneIsNotTrue [

	self deny: (#(1 2 3) with: #(1 2 4) allSatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])
]

{ #category : 'tests' }
SequenceableCollectionTest >> testWithAllSatisfyReturnsTrueWhenAllReturnTrue [

	self assert: (#(1 2 3) with: #(1 2 3) allSatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])
]

{ #category : 'tests' }
SequenceableCollectionTest >> testWithAnySatisfyReturnsFalseIfNoneSatisfies [

	self deny: (#(1 2 3) with: #(4 5 6) anySatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])
]

{ #category : 'tests' }
SequenceableCollectionTest >> testWithAnySatisfyReturnsTrueIfOneSatisfies [

	self assert: (#(1 2 3) with: #(4 5 3) anySatisfy: [ :leftNumber :rightNumber | leftNumber = rightNumber ])
]

{ #category : 'tests' }
SequenceableCollectionTest >> testWithIndexDoSeparatedByDoesNotValueSeparatedBlockForCollectionsOfOneElement [

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: $a.
	traversedElements := OrderedCollection new.

	self shouldntFail: [ collectionToTraverse withIndexDo: [ :anElement :index | traversedElements add: anElement  -> index ] separatedBy: [ self fail ] ].

	self assert: traversedElements size equals: 1.
	self assert: traversedElements first equals: $a->1.
]

{ #category : 'tests' }
SequenceableCollectionTest >> testWithIndexDoSeparatedByDoesNotValueSeparatedBlockOnLastElement [

	| collectionToTraverse traversedElements |

	collectionToTraverse := OrderedCollection with: $a with: $b.
	traversedElements := OrderedCollection new.

	collectionToTraverse
		withIndexDo: [ :anElement :index | traversedElements add: anElement  -> index ]
		separatedBy: [ traversedElements add: $-].

	self assert: traversedElements size equals: 3.
	self assert: traversedElements first equals: $a->1.
	self assert: traversedElements second equals: $-.
	self assert: traversedElements third equals: $b->2.
]

{ #category : 'tests' }
SequenceableCollectionTest >> testWithIndexDoSeparatedByDoesNothingForEmptyCollection [

	self shouldntFail: [ #() withIndexDo: [ :anElement :index | self fail ] separatedBy: [ self fail ] ]
]
