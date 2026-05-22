Class {
	#name : 'CollectTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'testing' }
CollectTest >> testIdentitySet [
	"
	CollectTest  new testIdentitySet
	"
	| col result |
	col _ #(1 2 3 1.0 2.0 3.0) asIdentitySet.
	result _ col collect: [ :elem |  elem yourself ].
	self assert: result class = IdentitySet.
	self assert: result = #(1 2 3 1.0 2.0 3.0) asIdentitySet.
	self assert: result = col
]

{ #category : 'testing' }
CollectTest >> testOrderedCollection [
	"
	CollectTest  new testOrderedCollection
	"
	| col result |
	col _ #(1 2 3 4 5) asOrderedCollection.
	result _ col collect: [ :elem |  elem * 2 ].
	self assert: result class = OrderedCollection.
	self assert: result = #(2 4 6 8 10) asOrderedCollection
]

{ #category : 'testing' }
CollectTest >> testSet [
	"
	CollectTest  new testSet
	"
	| col result |
	col _ #(1 2 3 1.0 2.0 3.0) asSet.
	result _ col collect: [ :elem |  elem yourself ].
	self assert: result class = Set.
	self assert: result = #(1 2 3) asSet.
	self assert: result = col
]

{ #category : 'testing' }
CollectTest >> testSortedCollection [
	"
	CollectTest  new testSortedCollection
	"
	| col result |
	col _ #(1 2 3 4 5) asSortedCollection: [ :a :b | a > b ].
	result _ col collect: [ :elem |  elem * 2 ].
	self assert: result class = OrderedCollection.
	self assert: result = #(10 8 6 4 2) asOrderedCollection
]
