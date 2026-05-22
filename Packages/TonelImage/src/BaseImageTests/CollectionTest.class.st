Class {
	#name : 'CollectionTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'as comma separated tests' }
CollectionTest >> assertAsCommaSeparated: aCollection equals: expectedString  [
	
	self 
		assert: (String streamContents: [ :stream | aCollection asCommaSeparated: [ :elem | stream print: elem + 1] on: stream ])
		equals: expectedString
]

{ #category : 'as comma separated tests' }
CollectionTest >> testAsCommaSeparatedOnWorksAsCommaStringAnd [

	self assertAsCommaSeparated: #() equals: ''. 
	self assertAsCommaSeparated: #(1) equals: '2'.
	self assertAsCommaSeparated: #(1 2) equals: '2 and 3'. 
	self assertAsCommaSeparated: #(1 2 3) equals: '2, 3 and 4'.
]

{ #category : 'as comma separated tests' }
CollectionTest >> testAsCommaStringAndDoesNotAddAnythingWhenEmpty [

	self assert: #() asCommaStringAnd isEmpty 
]

{ #category : 'as comma separated tests' }
CollectionTest >> testAsCommaStringAndDoesNotAddAnythingWhenHasOneElement [

	self assert: #(1) asCommaStringAnd equals: '1' 
]

{ #category : 'as comma separated tests' }
CollectionTest >> testAsCommaStringAndSeparatesWithAndWhenTwoOneElements [

	self assert: #(1 2) asCommaStringAnd equals: '1 and 2' 
]

{ #category : 'as comma separated tests' }
CollectionTest >> testAsCommaStringAndSeparatesWithCommaButLastOneWithAndWhenMoreThanTwoElements [

	self assert: #(1 2 3) asCommaStringAnd equals: '1, 2 and 3' 
]

{ #category : 'average tests' }
CollectionTest >> testAverageFailsWhenTheCollectionIsEmpty [

	self 
		should: [ #() average: [ :each | each ] ]
		raise: Error 
		description: Collection emptyCollectionDescription
]

{ #category : 'average tests' }
CollectionTest >> testAverageIfEmptyCalculatesItAsUsualWhenTheCollectionIsNotEmpty [

	| someNumbers |
	
	someNumbers _ #(1 5).

	self 
		assert: (someNumbers average: [ :each | each ] ifEmpty: [ self fail ])
		equals: someNumbers average
]

{ #category : 'average tests' }
CollectionTest >> testAverageIfEmptyEvaluatesEmptyBlockWhenTheCollectionIsEmpty [

	| emptyBlock |
	
	emptyBlock _ [ 0 ].

	self 
		assert: (#() average: [ :each | each asInteger ] ifEmpty: emptyBlock)
		equals: emptyBlock value
]

{ #category : 'groupBy tests' }
CollectionTest >> testGroupByHavingSelectsTheResultOfGroupBy [

	| collectionToGroupBy groupedByEven |
	
	collectionToGroupBy := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
	groupedByEven := collectionToGroupBy groupBy: [ :anInteger | anInteger even ] having: [ :group | group size > 2 ].
	
	self assert: (groupedByEven size) equals: 1.	
	self assert: (collectionToGroupBy select: [ :anInteger | anInteger odd ]) equals: (groupedByEven at: false).
]

{ #category : 'groupBy tests' }
CollectionTest >> testGroupByReturnsADictionaryThatGroupsACollectionByThePluggableKey [

	| collectionToGroupBy groupedByEven |
	
	collectionToGroupBy := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
	groupedByEven := collectionToGroupBy groupBy: [ :anInteger | anInteger even ].
	
	self assert: groupedByEven size equals: 2.
	self assert: (collectionToGroupBy select: [ :anInteger | anInteger even ]) equals: (groupedByEven at: true).
	self assert:(collectionToGroupBy select: [ :anInteger | anInteger odd ]) equals: (groupedByEven at: false).
]

{ #category : 'misc tests' }
CollectionTest >> testIsCollection [
	self assert: (Bag new is: #Collection).
]

{ #category : 'flatten tests' }
CollectionTest >> testItMaintainsTheSameCollectionSpecies [

	self assert: (OrderedCollection with: 1 with: #(2)) flatten equals: (OrderedCollection with: 1 with: 2).
]

{ #category : 'misc tests' }
CollectionTest >> testSetEquality [
	self assert: Set new = IdentitySet new.
	self assert: Set new hash = IdentitySet new hash.
	self assert: Dictionary new = IdentityDictionary new.
	self assert: Dictionary new hash = IdentityDictionary new hash.
	self assert: Dictionary new = OrderedDictionary new.
	self assert: Dictionary new hash = OrderedDictionary new hash.
	self assert: IdentityDictionary new = OrderedDictionary new.
	self assert: IdentityDictionary new hash = OrderedDictionary new hash.

	self deny: Set new = Dictionary new
]

{ #category : 'flatten tests' }
CollectionTest >> testWhenCollectionContainsASingleLevelOfElementsItReturnsTheSameCollection [

	self assert: #(1 2 3) flatten equals: #(1 2 3).
]

{ #category : 'flatten tests' }
CollectionTest >> testWhenCollectionContainsMoreThanTwoLevelsltReturnsTheElementsOfAllNestedCollections [

	self assert: #(1 #(2 3) #(4 #(5))) flatten equals: #(1 2 3 4 5).
]

{ #category : 'flatten tests' }
CollectionTest >> testWhenCollectionContainsStringCollectionsItDoesNotFlattenTheStrings [

	self assert: #('string1' #('string2' 'string3')) flatten equals: #('string1' 'string2' 'string3').
]

{ #category : 'flatten tests' }
CollectionTest >> testWhenCollectionContainsStringsItReturnsTheSameCollection [

	self assert: #('string1' 'string2' 'string3') flatten equals: #('string1' 'string2' 'string3').
]

{ #category : 'flatten tests' }
CollectionTest >> testWhenCollectionContainsTwoLevelsOfElementsItReturnsTheElementsOfAllNestedCollections [
	
	self assert: #(#(1) #(2 3)) flatten equals: #(1 2 3).
]

{ #category : 'flatten tests' }
CollectionTest >> testWhenCollectionIsEmptyItReturnsTheSameCollection [

	self assert: #() flatten equals: #().
]

{ #category : 'average tests' }
CollectionTest >> testsAverageWorksWithABlock [

	self assert: ({ '1' . '2' . '3' } average: [ :each | each asNumber ]) equals: 2
]
