Class {
	#name : 'FilterExceptionHandlingCondition',
	#superclass : 'ExceptionHandlingCondition',
	#instVars : [
		'handleCondition',
		'filterCondition'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'instance creation' }
FilterExceptionHandlingCondition class >> handling: aHandleCondition filtering: aFilterCondition  [
	
	^self new initializeHandling: aHandleCondition filtering: aFilterCondition 

]

{ #category : 'handling condition creation' }
FilterExceptionHandlingCondition >> , anExceptionHandlingCondition [

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createOrConditionWithFilterCondition: self
]

{ #category : 'handling condition creation' }
FilterExceptionHandlingCondition >> - anExceptionHandlingCondition [

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createFilterConditionWithFilterCondition: self
]

{ #category : 'handling condition creation - private' }
FilterExceptionHandlingCondition >> createFilterConditionWithExceptionType: anExceptionType [

	^self class 
		handling: anExceptionType, filterCondition 
		filtering: handleCondition 
]

{ #category : 'handling condition creation - private' }
FilterExceptionHandlingCondition >> createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition  [
	
	^aFilterExceptionHandlingCondition - handleCondition, filterCondition 
]

{ #category : 'handling condition creation - private' }
FilterExceptionHandlingCondition >> createFilterConditionWithOrCondition: anOrExceptionHandlingCondition  [
	
	^anOrExceptionHandlingCondition, filterCondition - handleCondition 
]

{ #category : 'handling condition creation - private' }
FilterExceptionHandlingCondition >> createOrConditionWithExceptionType: anExceptionType [

	^anExceptionType, handleCondition - filterCondition 
]

{ #category : 'handling condition creation - private' }
FilterExceptionHandlingCondition >> createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition  [
	
	^aFilterExceptionHandlingCondition handleCondition,handleCondition - aFilterExceptionHandlingCondition filterCondition - filterCondition 
]

{ #category : 'handling condition creation - private' }
FilterExceptionHandlingCondition >> createOrConditionWithOrCondition: anOrExceptionHandlingCondition  [
	
	^anOrExceptionHandlingCondition, handleCondition - filterCondition 
]

{ #category : 'accessing' }
FilterExceptionHandlingCondition >> filterCondition [
	
	^filterCondition 
]

{ #category : 'accessing' }
FilterExceptionHandlingCondition >> handleCondition [
	
	^handleCondition 
]

{ #category : 'error handling' }
FilterExceptionHandlingCondition >> handles: anException  [
	
	^ (filterCondition handles: anException) not and: [ handleCondition handles: anException ]
]

{ #category : 'initialization' }
FilterExceptionHandlingCondition >> initializeHandling: aHandleCondition filtering: aFilterCondition  [

	handleCondition := aHandleCondition.
	filterCondition := aFilterCondition 
]

{ #category : 'printing' }
FilterExceptionHandlingCondition >> printOn: aStream [

	aStream
		print: handleCondition ;
		nextPutAll: ' - ';
		print: filterCondition 
]
