"
I represent the protocol expected to be as condition on the exception handling message on:do:
I also define the protocol to create and combine exceptions handling conditions.
See methods #, and #- for a complemented documentation
"
Class {
	#name : 'ExceptionHandlingCondition',
	#superclass : 'Object',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'handling condition creation' }
ExceptionHandlingCondition >> , anExceptionHandlingCondition [

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
	
]

{ #category : 'handling condition creation' }
ExceptionHandlingCondition >> - anExceptionHandlingCondition [

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
	
]

{ #category : 'handling condition creation - private' }
ExceptionHandlingCondition >> createFilterConditionWithExceptionType: anExceptionType [

	self subclassResponsibility 
]

{ #category : 'handling condition creation - private' }
ExceptionHandlingCondition >> createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition  [

	self subclassResponsibility 
]

{ #category : 'handling condition creation - private' }
ExceptionHandlingCondition >> createFilterConditionWithOrCondition: anOrExceptionHandlingCondition  [

	self subclassResponsibility 
]

{ #category : 'handling condition creation - private' }
ExceptionHandlingCondition >> createOrConditionWithExceptionType: anExceptionType [

	self subclassResponsibility 
]

{ #category : 'handling condition creation - private' }
ExceptionHandlingCondition >> createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition  [

	self subclassResponsibility 
]

{ #category : 'handling condition creation - private' }
ExceptionHandlingCondition >> createOrConditionWithOrCondition: anOrExceptionHandlingCondition  [

	self subclassResponsibility 
]

{ #category : 'error handling' }
ExceptionHandlingCondition >> handles: anException  [
	
	"Must return true if anException must be handle
	See also Exception class>>handles: anException"
	
	self subclassResponsibility 
]
