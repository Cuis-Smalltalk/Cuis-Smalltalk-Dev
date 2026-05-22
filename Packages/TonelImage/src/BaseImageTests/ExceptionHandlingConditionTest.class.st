Class {
	#name : 'ExceptionHandlingConditionTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Exceptions',
	#package : 'BaseImageTests',
	#tag : 'Exceptions'
}

{ #category : 'exception handling tests' }
ExceptionHandlingConditionTest >> testBlockClosuresCanBeUsedAsHandlingCondition [

	self 
		shouldnt: [ [ Error signal ] on: [:anException | true ] do: [ :anError | ] ]
		raise: Error.
		
	
]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithExceptionTypeAndExceptionType [

	| condition |
	
	condition := Error - ZeroDivide .
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	
]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithExceptionTypeAndFilterCondition [

	| condition |
	
	condition := Error - (ZeroDivide - Halt).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Halt new).
	
]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithExceptionTypeAndOrCondition [

	| condition |
	
	condition := Error - (ZeroDivide, Halt).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self deny: (condition handles: Halt new).
	
]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithFilterConditionAndExceptionType [

	| condition |
	
	condition := (Error - Halt) - ZeroDivide.
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).

]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithFilterConditionAndFilterCondition [

	| condition |
	
	condition := (Error - Halt) - (ZeroDivide - Notification).
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Notification new).

]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithFilterConditionAndOrCondition [

	| condition |
	
	condition := (Error - Halt) - (Notification, ZeroDivide).
		
	self assert: (condition handles: Error new).
	self deny: (condition handles: Halt new).
	self deny: (condition handles: Notification new).
	self deny: (condition handles: ZeroDivide new).

]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithOrConditionAndExceptionType [

	| condition |
	
	condition := (Error, Halt) - ZeroDivide.
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	
]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithOrConditionAndFilterCondition [

	| condition |
	
	condition := (Error, Halt) - (ZeroDivide - ArithmeticError).
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	
]

{ #category : 'filter condition tests' }
ExceptionHandlingConditionTest >> testCanCreateFilterConditionWithOrConditionAndOrCondition [

	| condition |
	
	condition := (Error, Halt) - (ArithmeticError, ZeroDivide).
		
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).
	self deny: (condition handles: ArithmeticError new).
	self deny: (condition handles: ZeroDivide new).
	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithExceptionTypeAndExceptionType [

	| handlingCondition |
	
	handlingCondition := Error, Notification.
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Notification new).
	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithExceptionTypeAndFilterCondition [
	
	| handlingCondition |
	
	handlingCondition := Error, (ArithmeticError - ZeroDivide).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: ArithmeticError new).
	self deny: (handlingCondition handles: ZeroDivide new).
	
	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithExceptionTypeAndOrCondition [

	| handlingCondition |
	
	handlingCondition := Error, (Notification, UnhandledError).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Notification new).
	self assert: (handlingCondition handles: UnhandledError new).
	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithFilterConditionAndExceptionType [

	| condition |
	
	condition := (Error - ZeroDivide), ArithmeticError .
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithFilterConditionAndFilterCondition [

	| condition |
	
	condition := (Error - ZeroDivide), (ArithmeticError - DomainError).
	
	self assert: (condition handles: Error new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: ArithmeticError new).
	self deny: (condition handles: DomainError new).

	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithFilterConditionAndOrCondition [

	| condition |
	
	condition := (ArithmeticError - ZeroDivide), (Error, Halt).
	
	self assert: (condition handles: ArithmeticError new).
	self deny: (condition handles: ZeroDivide new).
	self assert: (condition handles: Error new).
	self assert: (condition handles: Halt new).

	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithOrConditionAndExceptionType [
	
	| handlingCondition |
	
	handlingCondition := (Error, Halt), Notification.
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Halt new).
	self assert: (handlingCondition handles: Notification new).
	
	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithOrConditionAndFilterCondition [

	| exceptionSet |
	
	exceptionSet := (Error, Notification), (ArithmeticError - ZeroDivide).
	
	self assert: (exceptionSet handles: Error new).
	self assert: (exceptionSet handles: Notification new).
	self assert: (exceptionSet handles: ArithmeticError new).
	self deny: (exceptionSet handles: ZeroDivide new).

	
]

{ #category : 'or condition tests' }
ExceptionHandlingConditionTest >> testCanCreateOrConditionWithOrConditionAndOrCondition [

	| handlingCondition |
	
	handlingCondition := (Error, Halt), (Notification, UnhandledError).
	
	self assert: (handlingCondition handles: Error new).
	self assert: (handlingCondition handles: Halt new).
	self assert: (handlingCondition handles: Notification new).
	self assert: (handlingCondition handles: UnhandledError new).
	
]

{ #category : 'environment preconditions' }
ExceptionHandlingConditionTest >> testExceptionsFollowTheExpectedHierarchy [

	"This test exits because all the others use existing exceptions and assume a subclass relationship
	If the relationship is not the assumed one, false positives or negatives could happen"
	
	self assert: (Error inheritsFrom: Exception).
	self assert: (Notification inheritsFrom: Exception).
	self assert: (UnhandledError inheritsFrom: Exception).
	self assert: (Halt inheritsFrom: Exception).
	self assert: (ArithmeticError inheritsFrom: Error).
	self assert: (DomainError inheritsFrom: ArithmeticError).
	self assert: (ArithmeticMessageError inheritsFrom: Error).
	self assert: (NegativePowerError inheritsFrom: ArithmeticMessageError).
	self assert: (ZeroDivide inheritsFrom: ArithmeticMessageError)
]

{ #category : 'exception handling tests' }
ExceptionHandlingConditionTest >> testFilterConditionWorksAsExcpetedWhenHandlingException [

	self
		shouldnt: [ [ Error signal ] on: Error - ZeroDivide do: [ :anError |  ] ]
		raise: Error.
		
	self
		should: [ [ ZeroDivide signal ] on: Error - ZeroDivide do: [ :anError | self fail ]]
		raise: Error.


]

{ #category : 'exception handling tests' }
ExceptionHandlingConditionTest >> testOrConditionWorksAsExcpetedWhenHandlingException [

	self 
		shouldnt: [ [ Error signal ] on: Error, Notification do: [ :anError | ] ]
		raise: Error.
		
	self 
		shouldnt: [ [ Notification signal ] on: Error, Notification do: [ :anError |  ] ]
		raise: Notification 

	
]
