"
I hold test cases for generic Process-related behaviour.
"
Class {
	#name : 'ProcessTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Processes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Processes'
}

{ #category : 'tests' }
ProcessTest >> testAtomicSuspend [
	"Test atomic suspend of foreign processes"

	| list p sema |
	sema := Semaphore new.
	p := [sema wait] newProcess.
	p resume.
	Processor yield.
	Smalltalk processSuspensionUnblocks ifFalse: [
		self shouldnt: [list := p suspendPrimitivelyOrFail] raise: Error ].
	self assert: list == nil.

]

{ #category : 'tests' }
ProcessTest >> testIsTerminated [
	"Test semantics of isTerminated.
	 A process is considered terminated if the suspendedContext has no unexecuted code left,
	 a situation tested by the Context >> #isDone method."
	
	"Note: If the suspended context's pc is anywhere but the last instruction, the process is NOT
	 considered terminated regardless of the status of the sender chain contexts, while on the
	 other hand if the suspended context's pc is nil, the process IS considered terminated
	 regardless of the 	status of the sender chain contexts. In all other cases the sender chain
	 is checked recursively for any unexecuted code.
	
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006822.html"
	
	| p ctx bottom |
	"let's create a test process consisting of a bottom context only:"
	ctx _ [4] asContext.
	p _ Process forContext: ctx priority: Processor activePriority.

	"initial situation: pc = startpc, sender = nil"
	self deny: p isTerminated.
	ctx pc: ctx endpc. "pc points to the last instruction now"
	self assert: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.

	"let's add a new bottom context..."
	bottom _ [2] asContext.
	ctx privSender: bottom.

	"... and reset ctx pc:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	ctx pc: ctx endpc.
	self deny: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.

	"let's point bottom's pc to its last instruction..."
	bottom pc: bottom endpc.

	"... and reset ctx pc again:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	ctx pc: ctx endpc.
	self assert: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.

	"let's nil bottom's pc..."
	bottom pc: nil.

	"... and reset ctx pc one more time:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	ctx pc: ctx endpc.
	self assert: p isTerminated.
	ctx pc: nil.
	self assert: p isTerminated.
	
	"let's test boundary situations:"
	ctx pc: ctx startpc.
	self deny: p isTerminated.
	p suspendedContext: nil.
	self assert: p isTerminated.

	"... and the active process is never considered terminated"
	self deny: Processor activeProcess isTerminated
]

{ #category : 'tests' }
ProcessTest >> testProcessFaithfulTermination [
	"When terminating a process, unwind blocks should be evaluated as if they were executed by the process being terminated."

	| process |
	[
		[process _ Processor activeProcess.
		Processor activeProcess suspend] ensure: [
			self assert: process = Processor activeProcess]	
	] fork.
	Processor yield.
	process terminate.
	self assert: process isTerminated
]

{ #category : 'tests' }
ProcessTest >> testResumeTerminatingProcess [
	"An attempt to resume a terminating process in the middle of preparing its suspended context should raise an error;
	one possible solution is to nil the suspended context of the terminating process while executing the vulnerable code"
	
	"Note: another solution could be to run the vulnerable part of the code in #terminate using #valueUnpreemptively;
	currently, however, #valueUnpreemptively is affected by the #priority: bug described here: 	http://lists.squeakfoundation.org/pipermail/squeak-dev/2021-December/217473.html"

	| terminatee terminator unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ unwound not]] newProcess.
	terminatee priority: Processor activePriority + 1. 
	terminatee resume.
	terminator _ [terminatee terminate] newProcess.
	terminator priority: Processor activePriority + 1. 
	self assert: terminatee isSuspended.
	self assert: terminator isSuspended. 
	"run terminator and stop inside #terminate"
	[terminator suspendedContext selectorToSendOrSelf = #bottomContext] whileFalse: [terminator step].
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"resuming the terminatee process should causes the VM to raise an error while its suspended context is nil"
	self should: [terminatee resume] raise: Error.
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"now let the terminator finish its interrupted termination of the terminatee process"
	terminator resume.
	self assert: terminatee isTerminated.
	self assert: unwound.
	self assert: terminator isTerminated
]

{ #category : 'tests' }
ProcessTest >> testResumeWithEnsureAfterBCR [
	"This test verifies #cannotReturn: context sender's pc has been nilled
	 before returning to make sure its code won't continue executing,
	 an unwind block will be run, and an Error will be raised.
	
	Without nilling the pc this example would crash the VM after resumption:
		[[^ 1] on: BlockCannotReturn do: [:ex | ex resume] ] fork
	
	and this example would happily execute an illegal return after resumption:
		[[true ifTrue: [^ 1]] on: BlockCannotReturn do: [:ex | ex resume] ] fork
	
	Note: push a fake stack top to prevent a VM crash when running this test in
	an image without the amended #cannotReturn:to: nilling the receiver's pc;
	the functionality of the test is not affected by this precautionary measure.	"

	| expr executedReturnTwo error unwound sync |
	expr := true.
	executedReturnTwo := false.
	error := false.
	unwound := false.
	sync := Semaphore new.
	[	[sync signal.
	      [[expr ifTrue: [thisContext push: nil. ^1].  "see the note above"
	       executedReturnTwo := true. ^2]
			ensure: [unwound := true]]
			        on: BlockCannotReturn
			        do: [:ex | ex resume]]
					on: UnhandledError
					do: [error := true]] fork.
	sync wait.
	self deny: executedReturnTwo.
	self assert: unwound.
	self assert: error
]

{ #category : 'tests' }
ProcessTest >> testRevisedSuspendExpectations [
	"Test revised suspend expectations vs. pre-2022 VM's suspend"

	| sema p list |
	sema _ Semaphore new.
	p _ [sema critical:[]] newProcess.
	p priority: Processor activePriority + 1.
	p resume.
	list _ p suspend. "this is a list the process p was waiting on prior to being suspended"

	Smalltalk processSuspensionUnblocks 
		ifFalse: [ "this is the newer, post-2022 suspend semantics"
			self assert: p suspendingList isNil.
			self assert: list isNil.
			self assert: p suspendedContext selectorToSendOrSelf = #wait
			"note the list has been cleared and the pc moved back before the wait"
			]
		ifTrue: [ "this is the older, pre-2022 suspend semantics"
			self assert: p suspendingList isNil.
			self assert: list = sema.
			self deny: p suspendedContext selectorToSendOrSelf = #wait
			"note the list confirms that process p was waitng on a semaphore"
			]
]

{ #category : 'tests' }
ProcessTest >> testTerminateByHighestPriorityProcess [
	"Test temination by a highest priority process."

	| p q |
	p _ [Semaphore new wait] newProcess.
	p resume. Processor yield.
	self assert: p isBlocked.

	q _ [p terminate] newProcess.
	q priority: Processor highestPriority.
	q resume.
	self assert: p isTerminated.
	self assert: q isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateEnsureAsStackTop [
	"Test #ensure unwind block is executed even when #ensure context is on stack's top."

	| p1 p2 p3 x1 x2 x3 |
	x1 _ x2 _ x3 _ false.
	
	"p1 is at the beginning of the ensure block; the unwind block hasn't run yet"
	p1 _ [[] ensure: [x1 _ x1 not]] newProcess.
	[[:ctx | ctx isUnwindContext and: [(ctx tempAt: 2) isNil]] value: p1 suspendedContext] whileFalse: [p1 step].

	"p2 has already set complete to true (tempAt: 2) but the unwind block hasn't run yet"
	p2 _ [[] ensure: [x2 _ x2 not]] newProcess.
	[[:ctx | ctx isUnwindContext and: [(ctx tempAt: 2) notNil]] value: p2 suspendedContext] whileFalse: [p2 step].

	"p3 has already set complete to true AND the unwind block has run already run;
	we have to verify the unwind block is not executed again during termination"
	p3 _ [[] ensure: [x3 _ x3 not]] newProcess.
	[[:ctx | ctx isUnwindContext and: [ctx willReturn]] value: p3 suspendedContext] whileFalse: [p3 step].

	"make sure all processes are running and only the p3's unwind block has finished"
	self deny: p1 isTerminated | p2 isTerminated | p3 isTerminated.
	self deny: x1 | x2.
	self assert: x3. "p3 has already run its unwind block; we test it won't run it again"
	"terminate all processes and verify all unwind blocks have finished correctly"
	p1 terminate. p2 terminate. p3 terminate.
	self assert: p1 isTerminated & p2 isTerminated & p3 isTerminated.
	self assert: x1 & x2 & x3
]

{ #category : 'tests' }
ProcessTest >> testTerminateEnsureOnTopOfEnsure [
	"Test two ensure contexts on top of each other unwind correctly, i.e. that
	both their unwind blocks get executed."
	
	| beenHere beenHereToo bottom p top |
	beenHere _ beenHereToo _ false.
	bottom _ ContextPart contextEnsure: [beenHereToo _ true].
	top _ ContextPart contextEnsure: [Processor activeProcess suspend. beenHere _ true].
	top privSender: bottom.
	p _ Process forContext: top priority: Processor activeProcess priority.
	p resume.
	Processor yield.
	self assert: p isSuspended. 
	p terminate.
	self assert: beenHere & beenHereToo.
	self assert: p isTerminated


]

{ #category : 'tests' }
ProcessTest >> testTerminateEverywhere [

	| block steps |
	block := [([[Notification signal]
		ensure: []]
			on: Notification do: [:ex | ex resume: 6])
				* 7].
	
	steps := 0.
	[steps] whileNotNil:
		[| process |
		process := Process forContext: block asContext priority: Processor activePriority.
		steps timesRepeat:
			[(process suspendedContext stackPtr > 0 and: [process suspendedContext top = 42])
				ifTrue: [steps := nil]
				ifFalse: [process step]].
		steps ifNotNil: [steps := steps + 1].
		"we need to skip quick returns to prevent VM crash; they are not suspension points."
		process suspendedContext method isQuick ifTrue: [process step. steps := steps + 1].
		
		process terminate.
		self assert: process isTerminated].
]

{ #category : 'tests' }
ProcessTest >> testTerminateHandlingUnwindError [
	"Test an error inside an unwind block is handled correctly."
	
	"Workspace example:
		[ [[Processor activeProcess terminate] ensure: [1/0]] on: ZeroDivide do: [] ] fork
	
	ZeroDivide error should get caught by the handler without opening the Debugger.
	
	To model this example as a test case we have to keep in mind that an error signal 
	in one thread cannot be caught in a different thread: if process 'p' signals an error
	it won't be searching for a handler in the thread that sent 'p terminate' message.
	So we can't do something like:
		p := [ [ [Semaphore new wait] ensure: [1/0] ] on: ZeroDivide do: [] ] newProcess.
		p resume. Processor yield.
		self shouldnt: [p terminate] raise: Error
	Instead, in order to catch the situation the ZeroDivide error is not caught within 'p',
	we try to catch the UnhandledError raised in 'p' indicating the ZeroDivide has been
	missed.	"
	
	| p error unwindBlock |
	unwindBlock _ [[1/0] on: UnhandledError do: [error _ true]].
	p _ [ [ [Semaphore new wait] ensure: unwindBlock ] on: ZeroDivide do: [] ] newProcess.
	p resume. Processor yield.
	self assert: p isBlocked. 
	error _ false.
	p terminate.
	self deny: error.
	self assert: p isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateHighestPriorityProcess [
	"Test termination of a highest priority process."
	
	| p |
	p _ [Semaphore new wait] newProcess.
	p priority: Processor highestPriority.
	p resume.
	self assert: p isBlocked.
	p terminate.
	self assert: p isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateInEnsure [
	"As shown in
	Martin McClure's 'Threads, Critical Sections, and Termination' (Smalltalks 2019 conference)
	https://youtu.be/AvM5YrjK9AE
	at 23:17
	self new testTerminateInEnsure
	"
	| process count delay |
	10 timesRepeat: [
		process := [
			count := 0.
			[] ensure: [
				10 timesRepeat: [
					count := count + 1.
					1000000 timesRepeat: [12 factorial]].
				count := count + 1]
			] newProcess.
		process resumeAt: Processor activeProcess priority - 1.
		delay := Delay forMilliseconds: 10.
		[ count isNil ] whileTrue: [
			delay wait ].
		self assert: process isTerminated not.
		process terminate.
		process priority: Processor activeProcess priority + 1.
		self
			assert: process isTerminated;
			assert: count equals: 11 ]
]

{ #category : 'tests' }
ProcessTest >> testTerminateInTerminate [
	"Terminating a terminator process should unwind both the terminator and its terminatee process"
	
	| terminator terminatee unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ true]] newProcess.
	terminator _ [terminatee terminate] newProcess.
	terminatee resume. Processor yield.
	self assert: terminatee isSuspended.
	self assert: terminator isSuspended.
	"run terminator and stop inside #terminate" 
	[terminator suspendedContext selectorToSendOrSelf = #bottomContext] whileFalse: [terminator step].
	self assert: terminator isSuspended.
	self deny: unwound.
	terminator terminate.
	self assert: terminatee isTerminated. 
	self assert: terminator isTerminated. 
	self assert: unwound
]

{ #category : 'tests' }
ProcessTest >> testTerminateNiledSuspendedContextProcess [
	"Test a process with niled suspendedContext terminates correctly."
	
	| p |
	p _ [] newProcess.
	self assert: p isSuspended.
	p suspendedContext: nil.
	p terminate.
	self assert: p isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateProcessTerminatedViaNiledPc [
	"Terminating a terminated process may still run unexecuted unwinds.
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006825.html"
	
	| terminatee unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ true]] newProcess.
	terminatee resume. Processor yield.
	self assert: terminatee isSuspended.
	self deny: unwound.
	terminatee suspendedContext pc: nil.
	self assert: terminatee isTerminated.
	self deny: unwound.
	terminatee terminate.
	self assert: terminatee isTerminated. 
	self assert: unwound
]

{ #category : 'tests' }
ProcessTest >> testTerminateSingleEnsure [
	"Test a stack consisting of a single ensure context unwinds correctly."
	
	| beenHere p singleton |
	beenHere _ false.
	singleton _ ContextPart contextEnsure: [beenHere _ true].
	p _ Process forContext: singleton priority: Processor activeProcess priority.
	self assert: p isSuspended. 
	self assert: p suspendedContext sender isNil. 
	p terminate.
	self assert: beenHere.
	self assert: p isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateTerminatingProcess [
	"An attempt to terminate a terminating process in the middle of preparing its suspended context should do nothing;
	one possible solution is to nil the suspended context of the terminating process while executing the vulnerable code"
	
	"Note: another solution could be to run the vulnerable part of the code in #terminate using #valueUnpreemptively;
	currently, however, #valueUnpreemptively is affected by the #priority: bug described here: 	http://lists.squeakfoundation.org/pipermail/squeak-dev/2021-December/217473.html"

	| terminatee terminator unwound |
	unwound _ false.
	terminatee _ [[Processor activeProcess suspend] ensure: [unwound _ unwound not]] newProcess.
	terminatee priority: Processor activePriority + 1. 
	terminatee resume.
	terminator _ [terminatee terminate] newProcess.
	terminator priority: Processor activePriority + 1. 
	self assert: terminatee isSuspended.
	self assert: terminator isSuspended. 
	"run terminator and stop inside #terminate"
	[terminator suspendedContext selectorToSendOrSelf = #bottomContext] whileFalse: [terminator step].
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"terminating the terminatee process again should make no difference while its suspended context is nil"
	terminatee terminate.
	self assert: terminatee suspendedContext isNil.
	self deny: unwound.
	self assert: terminator isSuspended.
	"now let the terminator finish its interrupted termination of the terminatee process"
	terminator resume.
	self assert: terminatee isTerminated.
	self assert: unwound.
	self assert: terminator isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateTerminatingProcessAfterUnwindStarted [
	"Terminating a terminatee process after the terminator process restarted the terminatee
	 process should unwind the terminatee process and let the terminator process terminate."
	
	"Such a situation may occur e.g. when a terminating process encounters an error and
	 opens a debugger (or calls another recovery machinery). In such case it's legitimate
	 to terminate the terminating process again (by closing the debugger or as a termination
	 by another recovery tool).

	 Note: this is a different situation than in 'testTerminateTerminatingProcess' where
	 the second termination is invoked 'too early' and may cause unpredictable outcome."
	
	| terminator terminatee unwound unwindBlock |
	unwound _ false.
	unwindBlock _ [Processor activeProcess suspend. unwound _ true].
	terminatee _ [[Semaphore new wait] ensure: unwindBlock] newProcess.
	terminatee resume. Processor yield.
	terminator _ [terminatee terminate] newProcess.
	self assert: terminatee isBlocked.
	self assert: terminator isSuspended.
	terminator resume.
	Processor yield.
	"terminator starts terminatee's unwind and gets blocked on a semaphore"
	self assert: terminator isBlocked.
	self assert: terminatee isReady.
	Processor yield.
	"terminatee resumes and stops at unwindBlock's #suspend"
	self assert: terminatee isSuspended.
	self assert: terminator isBlocked.
	terminatee terminate.
	"terminatee finishes unwinding and terminates which unblocks the terminator"
	self assert: terminatee isTerminated. 
	self assert: unwound.
	self assert: terminator isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateWithDelayInUnwind [
	"Test the process that invoked the termination of another process waits
	for the other process to finish unwinding."
	
	"Insert delay into the unwind block to force rescheduling"
	
	| delay p |
	delay _ Delay forMilliseconds: 10.
	p _ [[Processor activeProcess suspend] ensure: [delay wait]] newProcess.
	p resume.
	Processor yield.
	self assert: p isSuspended.
	p terminate.
	self assert: p isTerminated
]

{ #category : 'tests' }
ProcessTest >> testTerminateWithEmptyUnwindBlock [
	"Test termination of a process containing an unwind block that is empty.
	Note: A previous test with nil as the unwind block was removed, as this is no longer supported
	See email from Jaromir to the Cuis list on 19-Nov-2025.
	
	Workspace example:
	p := [[Processor activeProcess suspend] ensure: []] newProcess.
	p resume.
	[p terminate] fork
	"
	
	| p |
	p := [[Semaphore new wait] ensure: []] newProcess.
	p priority: Processor activePriority + 1.
	p resume.
	self assert: p isBlocked.
	[p terminate] forkAt: Processor activePriority + 1.
	self assert: p isTerminated
]

{ #category : 'tests' }
ProcessTest >> testValueEnsured [
	"Based on #testTerminateInEnsure
	Tweaked to raise likelihood of failure, besides testing #valueEnsured
	self new testValueEnsured
	"
	| process count delay |
	100 timesRepeat: [
		process := [
			count := 0.
			[
				10 timesRepeat: [
					count := count + 1.
					100000 timesRepeat: [12 factorial]].
				count := count + 1 ] valueEnsured
			] newProcess.
		process resumeAt: Processor activeProcess priority - 1.
		delay := Delay forMilliseconds: 10.
		[ count isNil ] whileTrue: [
			delay wait ].
		self assert: process isTerminated not.
		process terminate.
		process priority: Processor activeProcess priority + 1.
		self
			assert: process isTerminated;
			assert: count equals: 11 ]
]
