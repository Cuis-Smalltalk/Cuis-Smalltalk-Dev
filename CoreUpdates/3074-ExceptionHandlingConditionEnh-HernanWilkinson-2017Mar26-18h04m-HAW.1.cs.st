'From Cuis 5.0 of 7 November 2016 [latest update: #3064] on 29 March 2017 at 5:18:04 pm'!
!classDefinition: #ExceptionHandlingCondition category: #'Exceptions Kernel'!
Object subclass: #ExceptionHandlingCondition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!ExceptionHandlingCondition commentStamp: '<historical>' prior: 0!
I represent the protocol expected to be as condition on the exception handling message on:do:
I also define the protocol to create and combine exceptions handling conditions.
See methods #, and #- for a complemented documentation!


!ExceptionHandlingCondition commentStamp: '<historical>' prior: 0!
I represent the protocol expected to be as condition on the exception handling message on:do:
I also define the protocol to create and combine exceptions handling conditions.
See methods #, and #- for a complemented documentation!

Smalltalk renameClassNamed: #ExceptionFilter as: #FilterExceptionHandlingCondition!
!classDefinition: #FilterExceptionHandlingCondition category: #'Exceptions Kernel'!
ExceptionHandlingCondition subclass: #FilterExceptionHandlingCondition
	instanceVariableNames: 'handleCondition filterCondition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!
Smalltalk renameClassNamed: #ExceptionAdd as: #OrExceptionHandlingCondition!
!classDefinition: #OrExceptionHandlingCondition category: #'Exceptions Kernel'!
ExceptionHandlingCondition subclass: #OrExceptionHandlingCondition
	instanceVariableNames: 'leftCondition rightCondition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!BlockClosure methodsFor: 'error handing' stamp: 'HAW 3/29/2017 15:16:01'!
handles: anException

	"This allows a block to be the handling condition of an exception handling.
	See Exception class>>handles:"
	
	^self value: anException ! !


!Exception class methodsFor: 'handling condition creation' stamp: 'HAW 3/28/2017 15:48:57'!
, anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>,"
	
	^anExceptionHandlingCondition createOrConditionWithExceptionType: self! !

!Exception class methodsFor: 'handling condition creation' stamp: 'HAW 3/28/2017 15:49:08'!
- anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>-"

	^anExceptionHandlingCondition createFilterConditionWithExceptionType: self! !

!Exception class methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 11:23:04'!
createFilterConditionWithExceptionType: anExceptionType

	^FilterExceptionHandlingCondition handling: anExceptionType filtering: self! !

!Exception class methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 11:26:42'!
createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^FilterExceptionHandlingCondition handling: aFilterExceptionHandlingCondition filtering: self! !

!Exception class methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 15:40:33'!
createFilterConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^FilterExceptionHandlingCondition handling: anOrExceptionHandlingCondition filtering: self! !

!Exception class methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 15:37:59'!
createOrConditionWithExceptionType: anExceptionType

	^OrExceptionHandlingCondition handling: anExceptionType or: self
! !

!Exception class methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 15:48:29'!
createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^aFilterExceptionHandlingCondition handleCondition, self - aFilterExceptionHandlingCondition filterCondition 
	
	! !

!Exception class methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 11:09:54'!
createOrConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^OrExceptionHandlingCondition handling: anOrExceptionHandlingCondition or: self! !


!ExceptionHandlingCondition methodsFor: 'error handling' stamp: 'HAW 3/28/2017 17:29:18'!
handles: anException 
	
	"Must return true if anException must be handle
	See also Exception class>>handles: anException"
	
	self subclassResponsibility ! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation' stamp: 'HAW 3/28/2017 17:17:36'!
, anExceptionHandlingCondition

	"Creates a handling condition that will return true if either part of the condition handles the exception.
	It behaves like an or
	The following example will handle the exception
	[ Error signal ]
	   on:  Error, Halt 
	  do: [ :anError | ... ]
	
	The following example will also handle the exception:
	[ Halt signal ]
	   on:  Error, Halt
	  do: [ :anError | ... ]"
	
	self subclassResponsibility 
	! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation' stamp: 'HAW 3/29/2017 13:59:37'!
- anExceptionHandlingCondition

	"Creates a handling condition that will not handle exceptions that meet the right side of the condition
	The following example will not handle the exception
	[ 1/0 ]
	   on:  Error - ZeroDivide 
	  do: [ :anError | ... ]
	
	The following example will handle the exception:
	[ Error signal ]
	   on:  Error - ZeroDivide 
	  do: [ :anError | ... ]
	
	Due to inconsisties that can arrise with combining #, with #- the implementation orders the in such a way that 'or conditions' go first 
	and 'filter conditions' go last. Doing so (Error - Notification) , (UnhandledError - ZeroDivide)  is converted to  Error, UnhandledError - Notification - ZeroDivide 
	Inconsisties can arrise because ZeroDivide is a subclass of Error and therefore if the condition is not ordered correctly a ZeroDivide could be handled.
	This inconsisty can be found in Pharo where the condition (Error - Notification) , (UnhandledError - ZeroDivide) does not filter ZeroDivide but 
	the condition Error, UnhandledError - Notification - ZeroDivide does filter it.
	That is the reason the implementation uses double dispatch
	"
	
	self subclassResponsibility 
	! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 13:31:13'!
createFilterConditionWithExceptionType: anExceptionType

	self subclassResponsibility ! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 13:33:31'!
createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition 

	self subclassResponsibility ! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 13:33:39'!
createFilterConditionWithOrCondition: anOrExceptionHandlingCondition 

	self subclassResponsibility ! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 13:33:51'!
createOrConditionWithExceptionType: anExceptionType

	self subclassResponsibility ! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 13:34:31'!
createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition 

	self subclassResponsibility ! !

!ExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 13:34:42'!
createOrConditionWithOrCondition: anOrExceptionHandlingCondition 

	self subclassResponsibility ! !


!FilterExceptionHandlingCondition methodsFor: 'accessing' stamp: 'HAW 3/28/2017 17:20:21'!
filterCondition
	
	^filterCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'accessing' stamp: 'HAW 3/28/2017 17:20:26'!
handleCondition
	
	^handleCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'error handling' stamp: 'HAW 3/28/2017 17:20:41'!
handles: anException 
	
	^ (filterCondition handles: anException) not and: [ handleCondition handles: anException ]! !

!FilterExceptionHandlingCondition methodsFor: 'initialization' stamp: 'HAW 3/29/2017 13:45:21'!
initializeHandling: aHandleCondition filtering: aFilterCondition 

	handleCondition _ aHandleCondition.
	filterCondition _ aFilterCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation' stamp: 'HAW 3/29/2017 13:59:08'!
, anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createOrConditionWithFilterCondition: self! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation' stamp: 'HAW 3/29/2017 13:59:00'!
- anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createFilterConditionWithFilterCondition: self! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:56:46'!
createFilterConditionWithExceptionType: anExceptionType

	^self class 
		handling: anExceptionType, filterCondition 
		filtering: handleCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:19:24'!
createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^aFilterExceptionHandlingCondition - handleCondition, filterCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:19:48'!
createFilterConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^anOrExceptionHandlingCondition, filterCondition - handleCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:19:56'!
createOrConditionWithExceptionType: anExceptionType

	^anExceptionType - handleCondition - filterCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:20:04'!
createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^aFilterExceptionHandlingCondition handleCondition,handleCondition - aFilterExceptionHandlingCondition filterCondition - filterCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 15:32:33'!
createOrConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^anOrExceptionHandlingCondition, handleCondition - filterCondition ! !

!FilterExceptionHandlingCondition methodsFor: 'printing' stamp: 'HAW 3/28/2017 17:20:55'!
printOn: aStream

	aStream
		print: handleCondition ;
		nextPutAll: ' - ';
		print: filterCondition ! !


!FilterExceptionHandlingCondition class methodsFor: 'instance creation' stamp: 'HAW 3/28/2017 17:18:11'!
handling: aHandleCondition filtering: aFilterCondition 
	
	^self new initializeHandling: aHandleCondition filtering: aFilterCondition 
! !


!OrExceptionHandlingCondition methodsFor: 'accessing' stamp: 'HAW 3/28/2017 17:55:27'!
leftCondition

	^leftCondition ! !

!OrExceptionHandlingCondition methodsFor: 'accessing' stamp: 'HAW 3/28/2017 17:56:11'!
rightCondition

	^rightCondition ! !

!OrExceptionHandlingCondition methodsFor: 'error handling' stamp: 'HAW 3/28/2017 17:31:39'!
handles: anException

	^ (leftCondition handles: anException) or: [ rightCondition handles: anException ]! !

!OrExceptionHandlingCondition methodsFor: 'initialization' stamp: 'HAW 3/28/2017 17:32:20'!
initializeHandling: aLeftCondition or: aRightCondition

	leftCondition _ aLeftCondition.
	rightCondition _ aRightCondition ! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation' stamp: 'HAW 3/29/2017 13:59:16'!
, anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createOrConditionWithOrCondition: self
	! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation' stamp: 'HAW 3/29/2017 13:59:22'!
- anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createFilterConditionWithOrCondition: self
	! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:32:37'!
createFilterConditionWithExceptionType: anExceptionType

	^FilterExceptionHandlingCondition 
		handling: anExceptionType - leftCondition 
		filtering: rightCondition ! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:33:37'!
createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^aFilterExceptionHandlingCondition - leftCondition - rightCondition ! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 17:34:05'!
createFilterConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^ anOrExceptionHandlingCondition - leftCondition - rightCondition ! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 11:16:37'!
createOrConditionWithExceptionType: anExceptionType

	^self class handling: anExceptionType or: self! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 14:56:09'!
createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^self, aFilterExceptionHandlingCondition ! !

!OrExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/28/2017 11:20:32'!
createOrConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^self class handling: anOrExceptionHandlingCondition or: self! !

!OrExceptionHandlingCondition methodsFor: 'printing' stamp: 'HAW 3/28/2017 17:54:46'!
printOn: aStream
	
	aStream
		print: leftCondition;
		nextPutAll: ', ';
		print: rightCondition ! !


!OrExceptionHandlingCondition class methodsFor: 'instance creation' stamp: 'HAW 3/27/2017 15:47:32'!
handling: anExceptionClass or: anotherExceptionClass
 
	^self new initializeHandling: anExceptionClass or: anotherExceptionClass
! !

!methodRemoval: OrExceptionHandlingCondition #createOrHandlingConditionWithOrHandlingCondition:!
OrExceptionHandlingCondition removeSelector: #createOrHandlingConditionWithOrHandlingCondition:!
!methodRemoval: Exception class #createFilterConditionWithExceptionClass:!
Exception class removeSelector: #createFilterConditionWithExceptionClass:!
!methodRemoval: Exception class #createHandlingConditionWithExceptionClass:!
Exception class removeSelector: #createHandlingConditionWithExceptionClass:!
!methodRemoval: Exception class #handling:!
Exception class removeSelector: #handling:!
!methodRemoval: Exception class #orHandlingExceptionClass:!
Exception class removeSelector: #orHandlingExceptionClass:!

!Exception class reorganize!
('exceptionInstantiator' signal signal:)
('error handling' handles:)
('Camp Smalltalk' sunitSignalWith:)
('handling condition creation' , -)
('handling condition creation - private' createFilterConditionWithExceptionType: createFilterConditionWithFilterCondition: createFilterConditionWithOrCondition: createOrConditionWithExceptionType: createOrConditionWithFilterCondition: createOrConditionWithOrCondition:)
!


!ExceptionHandlingCondition reorganize!
('error handling' handles:)
('handling condition creation' , -)
('handling condition creation - private' createFilterConditionWithExceptionType: createFilterConditionWithFilterCondition: createFilterConditionWithOrCondition: createOrConditionWithExceptionType: createOrConditionWithFilterCondition: createOrConditionWithOrCondition:)
!


!ExceptionHandlingCondition class reorganize!
('as yet unclassified')
!


!FilterExceptionHandlingCondition reorganize!
('accessing' filterCondition handleCondition)
('error handling' handles:)
('initialization' initializeHandling:filtering:)
('handling condition creation' , -)
('handling condition creation - private' createFilterConditionWithExceptionType: createFilterConditionWithFilterCondition: createFilterConditionWithOrCondition: createOrConditionWithExceptionType: createOrConditionWithFilterCondition: createOrConditionWithOrCondition:)
('printing' printOn:)
!


!OrExceptionHandlingCondition reorganize!
('accessing' leftCondition rightCondition)
('error handling' handles:)
('initialization' initializeHandling:or:)
('handling condition creation' , -)
('handling condition creation - private' createFilterConditionWithExceptionType: createFilterConditionWithFilterCondition: createFilterConditionWithOrCondition: createOrConditionWithExceptionType: createOrConditionWithFilterCondition: createOrConditionWithOrCondition:)
('printing' printOn:)
!

!classRemoval: #ExceptionSet!
Smalltalk removeClassNamed: #ExceptionSet!
