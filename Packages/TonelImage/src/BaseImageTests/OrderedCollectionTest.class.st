Class {
	#name : 'OrderedCollectionTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'tests' }
OrderedCollectionTest >> testIsCollection [
	self assert: (OrderedCollection new is: #Collection).
]

{ #category : 'tests' }
OrderedCollectionTest >> testStreamContentsWorksAsExpected [

	| contents |
	
	contents := OrderedCollection streamContents: [ :stream | stream nextPut: 1 ].
	
	self assert: contents equals: (OrderedCollection with: 1).
]

{ #category : 'tests' }
OrderedCollectionTest >> testWriteStreamOnOrderedCollectionGrowsAsExpected [

	| contents |
	
	contents := OrderedCollection streamContents: [ :stream | 
		1 to: 101 do: [ :aNumber | stream nextPut: aNumber ]].
	
	1 to: 101 do: [ :aNumber | self assert: (contents at: aNumber) equals: aNumber ].
]
