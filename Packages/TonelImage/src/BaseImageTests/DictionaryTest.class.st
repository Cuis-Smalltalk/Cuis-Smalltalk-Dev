Class {
	#name : 'DictionaryTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'tests' }
DictionaryTest >> testIsCollection [
	self assert: (Dictionary new is: #Collection)
]

{ #category : 'tests' }
DictionaryTest >> testKeyNotFoundSignalTheRightMessage [

	self 
		should: [ Dictionary new errorKeyNotFound: 'theKey' ]
		raise: Error
		withMessageText: 'key: ''theKey'' not found'
]
