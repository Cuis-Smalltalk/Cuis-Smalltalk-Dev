Class {
	#name : 'ObjectTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Objects',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Objects'
}

{ #category : 'error tests' }
ObjectTest >> testErrorSignalsTheRightException [

	| reason |
	
	reason :=  'something went wrong'.
	self 
		should: [ self error: reason ]
		raise: Error
		withMessageText: reason 
]

{ #category : 'error tests' }
ObjectTest >> testShouldNotHappenBecauseSignalsTheRightError [

	| reason |
	
	reason :=  'something went wrong'.
	self 
		should: [ self shouldNotHappenBecause: reason ]
		raise: Error
		withMessageText: self shouldNotHappenBecauseErrorMessage, reason 
]

{ #category : 'error tests' }
ObjectTest >> testShouldNotHappenSignalsTheRightError [

	self 
		should: [ self shouldNotHappen ]
		raise: Error
		withMessageText: self shouldNotHappenErrorMessage 
]
