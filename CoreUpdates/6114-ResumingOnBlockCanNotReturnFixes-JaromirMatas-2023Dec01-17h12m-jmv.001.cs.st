'From Cuis6.0 [latest update: #6113] on 1 December 2023 at 5:15:00 pm'!
!classDefinition: #BlockCannotReturn category: #'Exceptions-Kernel'!
Error subclass: #BlockCannotReturn
	instanceVariableNames: 'result deadHome finalContext pc '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Kernel'!

!ContextPart methodsFor: 'private' stamp: 'jar 12/1/2023 18:56:05'!
stepToCalleeOrNil
	"Step to callee or sender; step to return and answer nil in case sender cannot be returned to."

	| ctxt |
	ctxt := self.
	[ctxt willFailReturn not and: [(ctxt := ctxt step) == self]] whileTrue.
	ctxt == self ifTrue: [^nil].
	^ctxt! !

!ContextPart methodsFor: 'private' stamp: 'jar 12/1/2023 18:55:51'!
willFailReturn
	"Answer whether self step will cause an illegal return."

	^self willReturn and: [self sender isNil or: [self sender isDead]]! !


!BlockCannotReturn methodsFor: 'accessing' stamp: 'jar 11/30/2023 23:54:52'!
finalContext

	^ finalContext! !

!BlockCannotReturn methodsFor: 'accessing' stamp: 'jar 11/30/2023 23:55:02'!
finalContext: context

	finalContext _ context! !

!BlockCannotReturn methodsFor: 'accessing' stamp: 'jar 11/30/2023 23:56:16'!
pc

	^pc! !

!BlockCannotReturn methodsFor: 'accessing' stamp: 'jar 11/30/2023 23:55:57'!
pc: pcIndex

	pc _ pcIndex! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'jar 12/1/2023 18:55:27'!
return: value from: aSender 
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop |
	newTop := aSender sender.
	(aSender isDead or: [newTop isNil or: [newTop isDead]]) ifTrue: [
		^ self pc: nil; send: #cannotReturn: to: self with: {value} super: false].
	newTop := aSender sender.
	(self findNextUnwindContextUpTo: newTop) ifNotNil: [
		^ self send: #aboutToReturn:through: to: self with: {value. nil} super: false].
	self releaseTo: newTop.
	newTop ifNotNil: [newTop push: value].
	^ newTop! !

!ContextPart methodsFor: 'controlling' stamp: 'jar 12/1/2023 18:58:43'!
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
		[ctxt isDead or: [topContext isNil]] whileFalse: [topContext := topContext stepToCalleeOrNil].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSender terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].
! !

!ContextPart methodsFor: 'private' stamp: 'jar 12/1/2023 18:54:52'!
cannotReturn: result to: homeContext
	"The receiver tried to return result to homeContext that cannot be returned from.
	 Capture the return context/pc in a BlockCannotReturn. Nil the pc to prevent repeat
	 attempts and/or invalid continuation. Answer the result of raising the exception."

	| exception previousPc |
	exception := BlockCannotReturn new.
	previousPc := pc ifNotNil: [self previousPc].
	exception
		result: result;
		deadHome: homeContext;
		finalContext: self;
		pc: previousPc.
	pc := nil.
	^exception signal! !


!MethodContext methodsFor: 'private' stamp: 'jar 12/1/2023 18:53:25'!
cannotReturn: result
	closureOrNil ifNotNil: [
		^self cannotReturn: result to: sender].
	self error: 'computation has been terminated'! !


!Process methodsFor: 'accessing' stamp: 'jar 12/1/2023 18:51:14'!
suspendedContext
	"Answer the context I was running when last preempted.
	For processes ReadyToRun or Suspended, it is the context that will be resumed.
	For Running processes, answer nil.

	Note: 	Possibly notNil for Terminated processes:
	Reverted back to the original Squeak version as a part of fixing #return:from:
	 bug (see also #testResumeWithEnsureAfterBCR for more comments);
	 the previous version from 2013 broke debugging processes with dead
	 contexts in the middle of the sender chain.
	 Example:
		 [[self halt. ^ 1] on: BlockCannotReturn do: [:ex | ex resume] ] fork
	 If you keep stepping over, you get into such a situation.	"

	^suspendedContext! !

!classDefinition: #BlockCannotReturn category: #'Exceptions-Kernel'!
Error subclass: #BlockCannotReturn
	instanceVariableNames: 'result deadHome finalContext pc'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Kernel'!
