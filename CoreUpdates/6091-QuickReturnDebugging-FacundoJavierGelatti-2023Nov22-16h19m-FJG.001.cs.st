'From Cuis 6.0 [latest update: #6090] on 22 November 2023 at 4:22:42 pm'!

!Debugger class methodsFor: 'instance creation' stamp: 'FJG 11/13/2023 14:00:02'!
newDebugging: aBlock
	"Create a new process to debug aBlock"

	| processToDebug debugger |
	debugger := self new.
	processToDebug := aBlock newProcess name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
		
	^ debugger! !


!Process methodsFor: 'private' stamp: 'FJG 11/8/2023 14:55:01'!
resolvePendingQuickReturns
	| method |	
	method := suspendedContext method.
	method isQuick ifTrue: [
		suspendedContext := suspendedContext callPrimitive: method primitive
	]! !


!Debugger methodsFor: 'private' stamp: 'jmv 11/21/2023 16:26:52'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.
	interruptedProcess isTerminated ifFalse: [
		interruptedProcessUI notNil ifTrue: [
			(interruptedProcess name withoutPrefix: 'Formerly: ')
				ifNotNil: [ :originalName | interruptedProcess name: originalName ].
			interruptedProcess priority: Processor userSchedulingPriority.
			UISupervisor isUIProcessRunning
				ifTrue: [ mustTerminateActive := true]
				ifFalse: [ UISupervisor stopUIProcess].
			UISupervisor uiProcess: interruptedProcess ].
		interruptedProcess resolvePendingQuickReturns; resume ].
	interruptedProcess := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	mustRedisplay ifNotNil: [ :w |
		[UISupervisor whenUIinSafeState: [ w invalidateBounds ]]
			forkAt: Processor userSchedulingPriority-1 ].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !


!Debugger class methodsFor: 'opening' stamp: 'FJG 11/13/2023 14:00:38'!
openDebugging: aBlock label: aString

	| debugger |
	debugger := self newDebugging: aBlock.
	debugger openFullMorphicLabel: aString.
		
	^ debugger! !


!InstructionStream methodsFor: 'testing' stamp: 'FJG 11/8/2023 14:47:28'!
willReturn
	"Answer whether the next bytecode is a return."
	| method |
	method := self method.
	^method isQuick or: [ method encoderClass isReturnAt: pc in: method ].! !


!ContextPart methodsFor: 'controlling' stamp: 'FJG 11/16/2023 09:15:50'!
runUntilErrorOrReturnFrom: aSender 
	"ASSUMES aSender is a sender of self.  Execute self's stack until aSender returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSender returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSender that jumps back to thisContext when evaluated.  We also insert an exception handler under aSender that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error ctxt here topContext |
	aSender method isQuick ifTrue: [ 
		^ {aSender step. nil}
	].
	
	here := thisContext.

	"Insert ensure and exception handler contexts under aSender"
	error := nil.
	ctxt := aSender insertSender: (ContextPart
		contextOn: UnhandledError do: [:ex |
			error ifNil: [
				error := ex exception.
				topContext := thisContext.
				ex resumeUnchecked: here jump]
			ifNotNil: [ex pass]
		]).
	ctxt := ctxt insertSender: (ContextPart
		contextEnsure: [error ifNil: [
				topContext := thisContext.
				here jump]
		]).
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[ctxt isDead or: [topContext isNil]] whileFalse: [topContext := topContext stepToCallee].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSender terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].
! !

!ContextPart methodsFor: 'controlling' stamp: 'FJG 11/8/2023 14:47:45'!
send: selector to: rcvr with: arguments lookupIn: lookupClass
	"Simulate the action of sending a message with selector and arguments
	 to rcvr. The argument, lookupClass, is the class in which to lookup the
	 message.  This is the receiver's class for normal messages, but for super
	 messages it will be some specific class related to the source method."

	| meth primIndex val ctxt isPrimitive |
	(meth := lookupClass lookupSelector: selector) ifNil:
		[^self send: #doesNotUnderstand:
				to: rcvr
				with: {Message selector: selector arguments: arguments}
				lookupIn: lookupClass].
	primIndex := meth primitive.
	isPrimitive := primIndex > 0 and: [ meth isQuick not ].
	isPrimitive ifTrue:
		[val := self doPrimitive: primIndex method: meth receiver: rcvr args: arguments.
		 (self isPrimFailToken: val) ifFalse:
			[^val]].
	(selector == #doesNotUnderstand: and: [lookupClass == ProtoObject]) ifTrue:
		[^self error: 'Simulated message ', arguments first selector, ' not understood'].
	ctxt := MethodContext sender: self receiver: rcvr method: meth arguments: arguments.
	isPrimitive ifTrue:
		[ctxt failPrimitiveWith: val].
	^ctxt! !

