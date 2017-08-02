'From Cuis 5.0 of 7 November 2016 [latest update: #3094] on 29 July 2017 at 9:36:44 pm'!

!Exception methodsFor: 'debug support' stamp: 'HAW 7/29/2017 15:47:08'!
canSearchForSignalerContext
	"This method is /only/ to support the debugger's catching of exceptions in stepIntoBlock."
	^signalContext isContext! !


!Debugger methodsFor: 'context stack menu' stamp: 'HAW 7/29/2017 20:49:55'!
doStep
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	| currentContext newContext |

	currentContext := self selectedContext.
	newContext := self handleLabelUpdatesIn: [interruptedProcess completeStep: currentContext]
						whenExecuting: currentContext.
	newContext == currentContext ifTrue:
		[newContext := interruptedProcess stepToSendOrReturn].
	self contextStackIndex > 1
		ifTrue: [self resetContext: newContext]
		ifFalse:
			[newContext == currentContext
				ifTrue: [self changed: #contentsSelection.
						self updateInspectors]
				ifFalse: [self resetContext: newContext]].
! !

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 7/29/2017 20:50:01'!
handleLabelUpdatesIn: aBlock whenExecuting: aContext
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	^aBlock
		on: Notification
		do: [:ex|
			(ex tag isArray
			 and: [ex tag size = 2
			 and: [(ex tag first == aContext or: [ex tag first hasSender: aContext])]])
				ifTrue:
					[self labelString: ex tag second description.
					 ex resume]
				ifFalse:
					[ex pass]]! !

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 7/29/2017 20:50:15'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method."

	self 
		handleLabelUpdatesIn: [interruptedProcess stepToHome: self selectedContext]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn! !


!Inspector methodsFor: 'initialization' stamp: 'HAW 7/29/2017 15:28:27'!
inspect: anObject 
	"Initialize the receiver so that it is inspecting anObject. There is no current selection.
	
	Normally the receiver will be of the correct class (as defined by anObject inspectorClass),
	because it will have just been created by sedning inspect to anObject.   However, the
	debugger uses two embedded inspectors, which are re-targetted on the current receiver
	each time the stack frame changes.  The left-hand inspector in the debugger has its
	class changed by the code here.  Care should be taken if this method is overridden to
	ensure that the overriding code calls 'super inspect: anObject', or otherwise ensures that 
	the class of these embedded inspectors are changed back."

	| c |
	c := anObject inspectorClass.
	self class ~= c ifTrue: [
		self class format = c format
			ifTrue: [self primitiveChangeClassTo: c basicNew]
			ifFalse: [self becomeForward: (c basicNew copyFrom: self)]].
	
	"Set 'object' before sending the initialize message, because some implementations
	of initialize (e.g., in DictionaryInspector) require 'object' to be non-nil."
	
	object := anObject. 
	self initialize! !


!OrderedCollectionInspector methodsFor: 'as yet unclassified' stamp: 'HAW 7/29/2017 15:13:36'!
fieldList

	| fieldsHere |
	object isNil ifTrue: [^OrderedCollection new].
	fieldsHere _
		[
			(object size <= (self i1 + self i2)
				ifTrue: [(1 to: object size) collect: [:i | i printString]]
				ifFalse: [(1 to: self i1) , (object size-(self i2-1) to: object size) collect: [:i | i printString]])
		] on: Error do: [:ex | ex return: OrderedCollection new].	
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"! !


!Process methodsFor: 'changing suspended state' stamp: 'HAW 7/29/2017 16:00:49'!
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| home anError |
	
	home := aContext home.
	[suspendedContext := suspendedContext step.
	home == suspendedContext home or: [home isDead]] whileFalse:
		[(suspendedContext selector == #signalForException:
			and: [(suspendedContext receiver isBehavior 
			and: [suspendedContext receiver includesBehavior: UnhandledError])
			and: [anError := suspendedContext tempAt: 1.
				((suspendedContext objectClass: anError) includesBehavior: Exception)
			and: [anError canSearchForSignalerContext]]]) ifTrue:
				[anError signalerContext ifNotNil: [:unhandledErrorSignalerContext|
					[unhandledErrorSignalerContext == suspendedContext] whileFalse:
						[self completeStep: suspendedContext].
					"Give a debugger a chance to update its title to reflect the new exception"
					 Notification new
						tag: {unhandledErrorSignalerContext. anError};
						signal.
					^unhandledErrorSignalerContext]]].
		
	^suspendedContext! !


!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend terminate)
('changing suspended state' activateReturn:value: complete: completeStep: completeTo: install: popTo: popTo:value: restartTop restartTopWith: return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString)
('private' suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debug:title: debug:title:full: debugFullWithTitle:)
('signaling' pvtSignal:list: signal: signalException:)
('test support' suspendPrimitivelyOrFail)
('UI support' animatedUI animatedUI:)
('testing' isActiveProcess isBlocked isReady isRunning isSuspended isTerminated isTheLastPreempted)
!


!Exception reorganize!
('Camp Smalltalk' sunitExitWith:)
('defaults' defaultResumeValue)
('exceptionBuilder' messageText: tag:)
('exceptionDescription' defaultAction description messageText noHandler tag)
('handling' isNested outer pass resignalAs: resume resume: resumeUnchecked: retry retryUsing: return return:)
('debug support' canSearchForSignalerContext)
('printing' printOn: receiver signalerContext)
('priv handling' handlerContexts handlerContexts: initialize isResumable popHandlerContext pushHandlerContext: topHandlerContext)
('signaling' signal signal: signalIn:)
!

