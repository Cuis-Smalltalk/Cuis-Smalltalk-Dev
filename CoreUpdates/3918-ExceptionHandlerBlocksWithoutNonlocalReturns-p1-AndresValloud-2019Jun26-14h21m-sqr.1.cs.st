'From Cuis 5.0 of 7 November 2016 [latest update: #3813] on 26 June 2019 at 2:22:04 pm'!
!classDefinition: #Exception category: #'Exceptions Kernel'!
Object subclass: #Exception
	instanceVariableNames: 'messageText tag signalContext handlerContexts outerContext handlerBlockNotCurtailed '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:19:08'!
outer
	"Evaluate the enclosing exception action and return to here instead of signal if it resumes (see #resumeUnchecked:)."

	| prevOuterContext |
	handlerBlockNotCurtailed _ true.
	self isResumable ifTrue: [
		prevOuterContext _ outerContext.
		outerContext _ thisContext contextTag.
	].
	self topHandlerContext nextHandlerContext handleSignal: self! !

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:19:23'!
pass
	"Yield control to the enclosing exception action for the receiver."

	| nextHandler |
	handlerBlockNotCurtailed _ true.
	nextHandler := self topHandlerContext nextHandlerContext.
	self popHandlerContext.
	nextHandler handleSignal: self! !

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:19:27'!
resignalAs: replacementException
	"Signal an alternative exception in place of the receiver."

	handlerBlockNotCurtailed _ true.
	signalContext resumeEvaluating: [replacementException signal]! !

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:23:41'!
resume: resumptionValue
	"Return resumptionValue as the value of the signal message."

	handlerBlockNotCurtailed _ true.
	self isResumable ifFalse: [IllegalResumeAttempt signal].
	self resumeUnchecked: resumptionValue! !

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:20:01'!
resumeUnchecked: resumptionValue
	"Return resumptionValue as the value of #signal, unless this was called after an #outer message, then return resumptionValue as the value of #outer."

	| ctxt |
	handlerBlockNotCurtailed _ true.
	outerContext ifNil: [
		signalContext return: resumptionValue
	] ifNotNil: [
		ctxt _ outerContext.
		outerContext _ ctxt tempAt: 1. "prevOuterContext in #outer"
		ctxt return: resumptionValue
	].
! !

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:20:09'!
retry
	"Abort an exception handler and re-evaluate its protected block."

	handlerBlockNotCurtailed _ true.
	self topHandlerContext restart! !

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:20:14'!
retryUsing: alternativeBlock
	"Abort an exception handler and evaluate a new block in place of the handler's protected block."

	handlerBlockNotCurtailed _ true.
	self topHandlerContext restartWithNewReceiver: alternativeBlock
! !

!Exception methodsFor: 'handling' stamp: 'sqr 6/26/2019 11:18:21'!
return: returnValue
	"Return the argument as the value of the block protected by the active exception handler."

	handlerBlockNotCurtailed _ true.
	self topHandlerContext return: returnValue! !

!classDefinition: #Exception category: #'Exceptions Kernel'!
Object subclass: #Exception
	instanceVariableNames: 'messageText tag signalContext handlerContexts outerContext handlerBlockNotCurtailed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!
