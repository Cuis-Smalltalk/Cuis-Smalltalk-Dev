"
This is the main class used to implement the exception handling system (EHS).  It plays two distinct roles:  that of the exception, and that of the exception handler.  More specifically, it implements the bulk of the protocols laid out in the ANSI specification - those protocol names are reflected in the message categories.

Exception is an abstract class.  Instances should neither be created nor trapped.  In most cases, subclasses should inherit from Error or Notification rather than directly from Exception.

In implementing this EHS, The Fourth Estate Inc. incorporated some ideas and code from Craig Latta's EHS.  His insights were crucial in allowing us to implement BlockClosure>>valueUninterruptably (and by extension, #ensure: and #ifCurtailed:), and we imported the following methods with little or no modification:

ContextPart>>terminateTo:
ContextPart>>terminate
MethodContext>>receiver:
MethodContext>>answer:

Thanks, Craig!
"
Class {
	#name : 'Exception',
	#superclass : 'Object',
	#instVars : [
		'messageText',
		'tag',
		'signalContext',
		'handlerContexts',
		'outerContext',
		'handlerBlockNotCurtailed'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'handling condition creation' }
Exception class >> , anExceptionHandlingCondition [

	"See ExceptionHandlingCondition>>,"
	
	^anExceptionHandlingCondition createOrConditionWithExceptionType: self
]

{ #category : 'handling condition creation' }
Exception class >> - anExceptionHandlingCondition [

	"See ExceptionHandlingCondition>>-"

	^anExceptionHandlingCondition createFilterConditionWithExceptionType: self
]

{ #category : 'handling condition creation - private' }
Exception class >> createFilterConditionWithExceptionType: anExceptionType [

	^FilterExceptionHandlingCondition handling: anExceptionType filtering: self
]

{ #category : 'handling condition creation - private' }
Exception class >> createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition  [
	
	^FilterExceptionHandlingCondition handling: aFilterExceptionHandlingCondition filtering: self
]

{ #category : 'handling condition creation - private' }
Exception class >> createFilterConditionWithOrCondition: anOrExceptionHandlingCondition  [
	
	^FilterExceptionHandlingCondition handling: anOrExceptionHandlingCondition filtering: self
]

{ #category : 'handling condition creation - private' }
Exception class >> createOrConditionWithExceptionType: anExceptionType [

	^OrExceptionHandlingCondition handling: anExceptionType or: self

]

{ #category : 'handling condition creation - private' }
Exception class >> createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition  [
	
	^aFilterExceptionHandlingCondition handleCondition, self - aFilterExceptionHandlingCondition filterCondition 
	
	
]

{ #category : 'handling condition creation - private' }
Exception class >> createOrConditionWithOrCondition: anOrExceptionHandlingCondition  [
	
	^OrExceptionHandlingCondition handling: anOrExceptionHandlingCondition or: self
]

{ #category : 'error handling' }
Exception class >> handles: exception [
	"Determine whether an exception handler will accept a signaled exception."

	^exception isKindOf: self
]

{ #category : 'exceptionInstantiator' }
Exception class >> signal [
	"Signal the occurrence of an exceptional condition."

	^ self new signal
]

{ #category : 'exceptionInstantiator' }
Exception class >> signal: signalerText [
	"Signal the occurrence of an exceptional condition with a specified textual description."

	^ self new signal: signalerText
]

{ #category : 'Camp Smalltalk' }
Exception class >> sunitSignalWith: aString [

        ^self signal: aString
]

{ #category : 'debug support' }
Exception >> canSearchForSignalerContext [
	"This method is /only/ to support the debugger's catching of exceptions in stepIntoBlock."
	^signalContext isContext
]

{ #category : 'exceptionDescription' }
Exception >> defaultAction [
	"The default action taken if the exception is signaled."

	self subclassResponsibility
]

{ #category : 'defaults' }
Exception >> defaultResumeValue [
	"Answer the value that by default should be returned if the exception is resumed"
	^nil
]

{ #category : 'exceptionDescription' }
Exception >> description [
	
"Return a textual description of the exception."

	| desc |
	desc := self class name asPlainString.
	^ self messageText
		ifNil: [ desc]
		ifNotNil: [ :mt | desc , ': ' , mt ]
]

{ #category : 'priv handling' }
Exception >> evaluateHandlerBlock: aBlock [

	| handlerEx |
	handlerBlockNotCurtailed := false.
	^[
			| answer |
			answer := [aBlock valueWithPossibleArgument: self] on: Exception do: [:ex | handlerEx := ex.  ex pass].
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			handlerBlockNotCurtailed := true.
			answer
	] ifCurtailed:
		[
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			(handlerBlockNotCurtailed not and: [handlerEx isNil or: [handlerEx handlerBlockNotCurtailed not]])
				ifTrue: [
					"Please see
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000800.html
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000809.html
					Also see the rest of the tread in detail.
					This is work in progress."
					(Preferences at: #allowNonLocalReturnsInExceptionHandlers)
						ifFalse: [ self error: 'Exception handler blocks must not do non local returns' ]
						ifTrue: [
							(Preferences at: #warnAboutNonLocalReturnsInExceptionHandlers)
								ifTrue: [ 'It is advisable to avoid method returns (non local returns) in exception handler blocks' print ].
							handlerBlockNotCurtailed := true ].
					]
				ifFalse: [handlerBlockNotCurtailed := true]
		]
]

{ #category : 'priv handling' }
Exception >> handlerBlockNotCurtailed [

	^handlerBlockNotCurtailed
]

{ #category : 'priv handling' }
Exception >> handlerContexts [

	^handlerContexts
]

{ #category : 'priv handling' }
Exception >> handlerContexts: aCollection [

	handlerContexts := aCollection
]

{ #category : 'priv handling' }
Exception >> initialize [

	self handlerContexts: OrderedCollection new
]

{ #category : 'handling' }
Exception >> isNested [
	"Determine whether the current exception handler is within the scope of another handler for the same exception."

	^self topHandlerContext nextHandlerContext canHandleSignal: self
]

{ #category : 'priv handling' }
Exception >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]

{ #category : 'exceptionDescription' }
Exception >> messageText [
	
"Return an exception's message text."

	^messageText
]

{ #category : 'exceptionBuilder' }
Exception >> messageText: signalerText [
	
"Set an exception's message text."

	messageText := signalerText
]

{ #category : 'exceptionDescription' }
Exception >> noHandler [
	"No one has handled this error, but now give them a chance to decide how to debug it.  If none handle this either then open debugger (see UnhandedError-defaultAction)"

	UnhandledError signalForException: self
]

{ #category : 'handling' }
Exception >> outer [
	"Evaluate the enclosing exception action and return to here instead of signal if it resumes (see #resumeUnchecked:)."

	| prevOuterContext |
	handlerBlockNotCurtailed := true.
	self isResumable ifTrue: [
		prevOuterContext := outerContext.
		outerContext := thisContext contextTag.
	].
	self topHandlerContext nextHandlerContext handleSignal: self
]

{ #category : 'handling' }
Exception >> pass [
	"Yield control to the enclosing exception action for the receiver."

	| nextHandler |
	handlerBlockNotCurtailed := true.
	nextHandler := self topHandlerContext nextHandlerContext.
	self popHandlerContext.
	nextHandler handleSignal: self
]

{ #category : 'priv handling' }
Exception >> popHandlerContext [

	self handlerContexts removeLast
]

{ #category : 'printing' }
Exception >> printOn: stream [

	stream nextPutAll: self description
]

{ #category : 'priv handling' }
Exception >> pushHandlerContext: aContextTag [

	self handlerContexts add: aContextTag
]

{ #category : 'printing' }
Exception >> receiver [

	^ self signalerContext receiver
]

{ #category : 'handling' }
Exception >> resignalAs: replacementException [
	"Signal an alternative exception in place of the receiver."

	handlerBlockNotCurtailed := true.
	signalContext resumeEvaluating: [replacementException signal]
]

{ #category : 'handling' }
Exception >> resume [
	"Return from the message that signaled the receiver."

	self resume: self defaultResumeValue
]

{ #category : 'handling' }
Exception >> resume: resumptionValue [
	"Return resumptionValue as the value of the signal message."

	handlerBlockNotCurtailed := true.
	self isResumable ifFalse: [IllegalResumeAttempt signal].
	self resumeUnchecked: resumptionValue
]

{ #category : 'handling' }
Exception >> resumeUnchecked: resumptionValue [
	"Return resumptionValue as the value of #signal, unless this was called after an #outer message, then return resumptionValue as the value of #outer."

	| ctxt |
	handlerBlockNotCurtailed := true.
	outerContext ifNil: [
		signalContext return: resumptionValue
	] ifNotNil: [
		ctxt := outerContext.
		outerContext := ctxt tempAt: 1. "prevOuterContext in #outer"
		ctxt return: resumptionValue
	].

]

{ #category : 'handling' }
Exception >> retry [
	"Abort an exception handler and re-evaluate its protected block."

	handlerBlockNotCurtailed := true.
	self topHandlerContext restart
]

{ #category : 'handling' }
Exception >> retryUsing: alternativeBlock [
	"Abort an exception handler and evaluate a new block in place of the handler's protected block."

	handlerBlockNotCurtailed := true.
	self topHandlerContext restartWithNewReceiver: alternativeBlock

]

{ #category : 'handling' }
Exception >> return [
	"Return nil as the value of the block protected by the active exception handler."

	self return: nil
]

{ #category : 'handling' }
Exception >> return: returnValue [
	"Return the argument as the value of the block protected by the active exception handler."

	handlerBlockNotCurtailed := true.
	self topHandlerContext return: returnValue
]

{ #category : 'signaling' }
Exception >> signal [

	^self signalIn: thisContext
]

{ #category : 'signaling' }
Exception >> signal: signalerText [
	"Signal the occurrence of an exceptional condition with a specified textual description."

	self messageText: signalerText.
	^ self signal
]

{ #category : 'signaling' }
Exception >> signalIn: aContext [
	"Ask ContextHandlers in the sender chain to handle this signal.  The default is to execute and return my defaultAction."

	signalContext ifNotNil: [self error: 'This exception has already been signaled and its handler block is being executed.'].
	signalContext := aContext contextTag.
	^ aContext nextHandlerContext handleSignal: self
]

{ #category : 'printing' }
Exception >> signalerContext [
	"Find the first sender of signal(:)"

	^ signalContext findContextSuchThat: [:ctxt |
		(ctxt receiver == self or: [ctxt receiver == self class]) not]
]

{ #category : 'Camp Smalltalk' }
Exception >> sunitExitWith: aValue [

        self return: aValue
]

{ #category : 'exceptionDescription' }
Exception >> tag [
       "Return an exception's tag value."

	^tag ifNil: [ self messageText ]
]

{ #category : 'exceptionBuilder' }
Exception >> tag: t [
	"This message is not specified in the ANSI protocol, but that looks like an oversight because #tag is specified, and the spec states that the signaler may store the tag value."

	tag := t
]

{ #category : 'priv handling' }
Exception >> topHandlerContext [

	^self handlerContexts isEmpty ifTrue: [nil] ifFalse: [self handlerContexts last]
]
