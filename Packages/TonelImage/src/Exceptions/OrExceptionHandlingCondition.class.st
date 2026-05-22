Class {
	#name : 'OrExceptionHandlingCondition',
	#superclass : 'ExceptionHandlingCondition',
	#instVars : [
		'leftCondition',
		'rightCondition'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'instance creation' }
OrExceptionHandlingCondition class >> handling: anExceptionClass or: anotherExceptionClass [
 
	^self new initializeHandling: anExceptionClass or: anotherExceptionClass

]

{ #category : 'handling condition creation' }
OrExceptionHandlingCondition >> , anExceptionHandlingCondition [

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createOrConditionWithOrCondition: self
	
]

{ #category : 'handling condition creation' }
OrExceptionHandlingCondition >> - anExceptionHandlingCondition [

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createFilterConditionWithOrCondition: self
	
]

{ #category : 'handling condition creation - private' }
OrExceptionHandlingCondition >> createFilterConditionWithExceptionType: anExceptionType [

	^FilterExceptionHandlingCondition 
		handling: anExceptionType - leftCondition 
		filtering: rightCondition 
]

{ #category : 'handling condition creation - private' }
OrExceptionHandlingCondition >> createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition  [
	
	^aFilterExceptionHandlingCondition - leftCondition - rightCondition 
]

{ #category : 'handling condition creation - private' }
OrExceptionHandlingCondition >> createFilterConditionWithOrCondition: anOrExceptionHandlingCondition  [
	
	^ anOrExceptionHandlingCondition - leftCondition - rightCondition 
]

{ #category : 'handling condition creation - private' }
OrExceptionHandlingCondition >> createOrConditionWithExceptionType: anExceptionType [

	^self class handling: anExceptionType or: self
]

{ #category : 'handling condition creation - private' }
OrExceptionHandlingCondition >> createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition  [
	
	^self, aFilterExceptionHandlingCondition 
]

{ #category : 'handling condition creation - private' }
OrExceptionHandlingCondition >> createOrConditionWithOrCondition: anOrExceptionHandlingCondition  [
	
	^self class handling: anOrExceptionHandlingCondition or: self
]

{ #category : 'error handling' }
OrExceptionHandlingCondition >> handles: anException [

	^ (leftCondition handles: anException) or: [ rightCondition handles: anException ]
]

{ #category : 'initialization' }
OrExceptionHandlingCondition >> initializeHandling: aLeftCondition or: aRightCondition [

	leftCondition := aLeftCondition.
	rightCondition := aRightCondition 
]

{ #category : 'accessing' }
OrExceptionHandlingCondition >> leftCondition [

	^leftCondition 
]

{ #category : 'printing' }
OrExceptionHandlingCondition >> printOn: aStream [
	
	aStream
		print: leftCondition;
		nextPutAll: ', ';
		print: rightCondition 
]

{ #category : 'accessing' }
OrExceptionHandlingCondition >> rightCondition [

	^rightCondition 
]
