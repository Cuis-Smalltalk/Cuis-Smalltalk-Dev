'From Cuis 4.2 of 25 July 2013 [latest update: #2439] on 15 November 2015 at 12:10:02.504042 pm'!
!classDefinition: #Exception category: #'System-Exceptions Kernel'!
Object subclass: #Exception
	instanceVariableNames: 'messageText tag signalContext handlerContext outerContext handlerContexts '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Exceptions Kernel'!

!Exception methodsFor: 'priv handling' stamp: 'SqR 11/15/2015 08:17'!
handlerContexts

	^handlerContexts! !

!Exception methodsFor: 'priv handling' stamp: 'SqR 11/15/2015 08:18'!
handlerContexts: aCollection

	handlerContexts _ aCollection! !

!Exception methodsFor: 'priv handling' stamp: 'SqR 11/15/2015 08:18'!
initialize

	self handlerContexts: OrderedCollection new! !

!Exception methodsFor: 'priv handling' stamp: 'SqR 11/15/2015 08:45'!
popHandlerContext

	self handlerContexts removeLast! !

!Exception methodsFor: 'priv handling' stamp: 'SqR 11/15/2015 08:45'!
pushHandlerContext: aContextTag

	self handlerContexts add: aContextTag! !

!Exception methodsFor: 'priv handling' stamp: 'SqR 11/15/2015 08:41'!
topHandlerContext

	^self handlerContexts isEmpty ifTrue: [nil] ifFalse: [self handlerContexts last]! !

!Exception methodsFor: 'signaling' stamp: 'SqR 10/10/2015 14:19'!
signalIn: aContext
	"Ask ContextHandlers in the sender chain to handle this signal.  The default is to execute and return my defaultAction."

	signalContext _ aContext contextTag.
	^ aContext nextHandlerContext handleSignal: self! !


!ContextPart methodsFor: 'controlling' stamp: 'SqR 11/15/2015 11:43'!
resumeEvaluating: aBlock
	"Unwind thisContext to self and resume with aBlock value as result of last send.  Execute unwind blocks when unwinding.  ASSUMES self is a sender of thisContext"

	| ctxt unwindBlock |
	self isDead ifTrue: [self cannotReturn: aBlock value to: self].
	ctxt := thisContext.
	[
		ctxt := ctxt findNextUnwindContextUpTo: self.
		ctxt isNil
	] whileFalse: [
		(ctxt tempAt: 2) isNil ifTrue:
			[
				ctxt tempAt: 2 put: true.
				unwindBlock := ctxt tempAt: 1.
				thisContext terminateTo: ctxt.
				unwindBlock value
			]
	].
	thisContext terminateTo: self.
	^aBlock value! !


!Exception methodsFor: 'handling' stamp: 'SqR 11/15/2015 08:44'!
isNested
	"Determine whether the current exception handler is within the scope of another handler for the same exception."

	^self topHandlerContext nextHandlerContext canHandleSignal: self! !

!Exception methodsFor: 'handling' stamp: 'SqR 11/15/2015 08:44'!
outer
	"Evaluate the enclosing exception action and return to here instead of signal if it resumes (see #resumeUnchecked:)."

	| prevOuterContext |
	self isResumable ifTrue: [
		prevOuterContext _ outerContext.
		outerContext _ thisContext contextTag.
	].
	self topHandlerContext nextHandlerContext handleSignal: self! !

!Exception methodsFor: 'handling' stamp: 'SqR 11/15/2015 08:46'!
pass
	"Yield control to the enclosing exception action for the receiver."

	| nextHandler |
	nextHandler := self topHandlerContext nextHandlerContext.
	self popHandlerContext.
	nextHandler handleSignal: self! !

!Exception methodsFor: 'handling' stamp: 'SqR 10/10/2015 15:26'!
resignalAs: replacementException
	"Signal an alternative exception in place of the receiver."

	signalContext resumeEvaluating: [replacementException signal]! !

!Exception methodsFor: 'handling' stamp: 'SqR 11/15/2015 08:44'!
retry
	"Abort an exception handler and re-evaluate its protected block."

	self topHandlerContext restart! !

!Exception methodsFor: 'handling' stamp: 'SqR 11/15/2015 08:44'!
retryUsing: alternativeBlock
	"Abort an exception handler and evaluate a new block in place of the handler's protected block."

	self topHandlerContext restartWithNewReceiver: alternativeBlock
! !

!Exception methodsFor: 'handling' stamp: 'SqR 11/15/2015 08:44'!
return: returnValue
	"Return the argument as the value of the block protected by the active exception handler."

	self topHandlerContext return: returnValue! !

!Exception methodsFor: 'signaling' stamp: 'SqR 10/10/2015 14:19'!
signal

	^self signalIn: thisContext! !


!ContextPart methodsFor: 'controlling' stamp: 'SqR 10/10/2015 15:25'!
resume: value

	self resumeEvaluating: [value]! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 11/15/2015 08:45'!
evaluateSignal: exception
	"The following primitive is just a marker used to find the evaluation context. 
	See MethodContext>>#isHandlerOrSignalingContext. "

	<primitive: 199>
	| value |
	exception pushHandlerContext: self contextTag.
	value := self exceptionHandlerBlock valueWithPossibleArgument: exception.	
	"return from self if not otherwise directed in handle block"
	self return: value! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 11/15/2015 08:58'!
findNextHandlerContext
	"Return the next handler marked context, returning nil if there is none.  Search starts
	with self and proceeds up to nil.  If context isn't a handler context, it must be a signaling
	context.  When we reach a signaling context we must skip over any handlers that might
	be on the stack between the signaling context and the handler context for that signal."

	| context searchStartContext |
	context := self findNextHandlerOrSignalingContext.
	context isNil ifTrue: [^nil].
	context isHandlerContext ifTrue: [^context]. 
	searchStartContext := context exceptionClass topHandlerContext ifNil: [context].
	^searchStartContext nextHandlerContext! !

!methodRemoval: Exception #handlerContext!
Exception removeSelector: #handlerContext!
!methodRemoval: Exception #handlerContext:!
Exception removeSelector: #handlerContext:!
!methodRemoval: Exception #privHandlerContext!
Exception removeSelector: #privHandlerContext!
!methodRemoval: Exception #privHandlerContext:!
Exception removeSelector: #privHandlerContext:!
!classDefinition: #Exception category: #'System-Exceptions Kernel'!
Object subclass: #Exception
	instanceVariableNames: 'messageText tag signalContext handlerContexts outerContext'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Exceptions Kernel'!

!Exception reorganize!
('*Assessments' handlerActionForEvaluation handlerActionForPrerequisiteEvaluation handlerActionForRunningToFailure messageTextForHandling occurredWhileEvaluating:inTheContextOf: occurredWhileEvaluatingPrerequisiteAction:inTheContextOf: occurredWhileRunning:toFailureInTheContextOf: preventsCheckSuccess resignalAsAssessmentMalfunctionNotification)
('Camp Smalltalk' sunitExitWith:)
('defaults' defaultResumeValue)
('exceptionBuilder' messageText: tag:)
('exceptionDescription' defaultAction description messageText noHandler tag)
('handling' isNested outer pass resignalAs: resume resume: resumeUnchecked: retry retryUsing: return return:)
('printing' printOn: receiver signalerContext)
('priv handling' handlerContexts handlerContexts: initialize isResumable popHandlerContext pushHandlerContext: topHandlerContext)
('signaling' signal signal: signalIn:)
!

