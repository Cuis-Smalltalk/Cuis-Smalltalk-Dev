"
I represent an independent path of control in the system. This path of control may be stopped (by sending the message suspend) in such a way that it can later be restarted (by sending the message resume). When any one of several paths of control can be advanced, the single instance of ProcessorScheduler named Processor determines which one will actually be advanced, partly using the value of priority.

Instance Variables: N.B. The first four are used by the virtual machine. They must be defined, and defined in this order,
	suspendedContext:		<Context|nil>
	priority:					<Integer>
	myList:					<LinkedList|nil>
	threadAffinity:			<Integer|nil>
	name:					<String|nil>
	env:						<Dictionary|nil>

suspendedContext
	- if nil, then I am either the active process or I have been terminated.  If not nil it is the Context at the hot end of my stack.

priority
	- my scheduling priority, which corresponds to the index in ProcessorScheduler quiescentProcesses that holds the LinkedList of processes at this priority.  If I am running but quiescent, changing priority involves changing priority and myList.

myList
	- if nil, I am either suspended or the active process.  If not nil it is either some condition variable (Semaphore, Mutex) that I am blocked waiting on, or it is the LinkedList in ProcessorScheduler quiescentProcesses that holds the processes for my priority that are not the current active process but still runnable.

threadAffinity
	- if not nil then it is an integer value set by the VM that controls how I interact with underlying native threads. If positive, the threadAffinity is that of some native thread known to the VM, and the VM will ensure that when I am running, I will be running on that native thread.  If negative, the absolute value of threadAffinity is that of some thread known to the VM, and the VM will ensure that when I am running, I will *not* be running on that native thread.  Set by the VM.  Not to be set manually.  Meaningful only on threaded VMs (which do not mean VMs with a threaded heartbeat).  It is required to be my fourth instance variable. See SmalltalkImage >> #processHasThreadAffinityInstVar: for further information.

name
	- if not nil this is my name, used for information, see e.g. the names of processes displayed in the ProcessBrowser

env
	- if not nil this is a Dictionary providing process-specific variables.  See e.g. environmentAt:*, DynamicVariable and ProcessSpecificVariable.
"
Class {
	#name : 'Process',
	#superclass : 'Link',
	#instVars : [
		'suspendedContext',
		'priority',
		'myList',
		'threadAffinity',
		'name',
		'env'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'instance creation' }
Process class >> forContext: aMethodContext priority: anInteger  [
	"Answer an instance of me that has suspended aContext at priority anInteger."

	| newProcess |
	newProcess := self new.
	newProcess priority: anInteger.
	newProcess suspendedContext: aMethodContext.
	^newProcess
]

{ #category : 'documentation' }
Process class >> terminateExamples [
"
This is the contents of the examples posted by Jaromir to exercise his worn on process termination and exceptions.
It was posted to the Cuis mail list at https://lists.cuis.st/mailman/archives/cuis-dev/2021-May/003171.html
(or maybe, it is a later version of edition of it if this comment is not up to date!)

Thanks Jaromir for this great contribution!

Workspace new
		contents: Process terminateExamples;
		openLabel: 'Jaromir Matas''s Process - terminate examples Cuis'.
"
^


'"
The following examples illustrate the behavior and semantics of the new #terminate.
"
"
Part 1: terminate a healthy process inside an unwind block
======================================
Show that all pending unwind blocks get executed regardless whether they have already started or not.
"
"
terminate a suspended process:
....................................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor activeProcess suspend. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

"
terminate a runnable process:
.................................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor yield] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor yield. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

"
terminate a blocked process:
...............................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Semaphore new wait] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Semaphore new wait. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

"
terminate the active process:
................................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor activeProcess terminate. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"


"
Part 2: terminate a process after encountering an unhandled error inside an unwind block
===========================================================
Show that all unhandled errors inside unwind blocks get caught and all pending unwind blocks get executed as expected.
"
"
Termination happens when you hit Abandon on the Debugger window.
Note: when you hit Proceed the computation will continue normally and will print x1 x2 x3 x4
"
"
one unhandled error scenario:
.................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test.
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"

"
multiple nested unhandled errors:
........................................................
"
[
	[
		[ ] ensure: [
			[self error: #outer_error] ensure: [
				self error: #inner_error.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] fork

"prints x1 x2 x3"

[
	[
		[self error: #outer_error] ensure: [
			[self error: #middle_error] ensure: [
				self error: #inner_error.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] fork
"prints x1 x2 x3"


[self error: #outer_error] ensure: [
    [self error: #middle_error] ensure: [
        [self error: #inner_error] ensure: [
            Transcript show: #x1].
        Transcript show: #x2].
    Transcript show: #x3].
Transcript show: #x4
"prints x1 x2 x3"
"same result when wrapped in fork"


[] ensure: [
[self error: #outer_error] ensure: [
    [self error: #middle_error] ensure: [
        [self error: #inner_error] ensure: [
            Transcript show: #x1].
        Transcript show: #x2].
    Transcript show: #x3].
Transcript show: #x4]
"prints x1 x2 x3 x4"
"same result when wrapped in fork"


"
Part 3: terminate a process after encountering an unhandled error AND a non-local return inside an unwind block
==========================================================================
Show the semantics of non-local returns inside unwind blocks combined with unhandled error during termination.
"
"
Termination happens when you hit Abandon on the Debugger window.
Note: when you hit Proceed the computation will continue normally and will print x1 x2 x3 x4"
"

"
non-local return inside inner-most halfway-thru unwind block:
......................................................................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			^Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test. 
			^Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x3"

"
non-local return inside outer-most halfway-thru unwind block:
......................................................................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			Transcript show: #x1]. 
		^Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test. 
			Transcript show: #x1]. 
		^Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4


"prints x1 x2 x3"

"
one more level...:
............................
"
[
	[
		[ ] ensure: [
			[ ] ensure: [
				self error: #unwind_test. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		^Transcript show: #x3]
] ensure: [
	Transcript show: #x4].
Transcript show: #x5

"prints x1 x2 x3 x4 (even if wrapped in #fork)"

"
non-local return outside halfway-thru unwind blocks:
......................................................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	^Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test. 
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	^Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


"
Part 4: COMPARISON with the behavior when encountering a handled error or a healthy non-local return inside an unwind block
===================================================================================
Show that ONLY pending unwind blocks that have not yet started get executed during an exception or non-local return unwind.
Why? Handled exceptions and non-local returns do not use #terminate and so do not follow the new terminate semantics and use the traditional simpler semantics instead, skipping unwind blocks halfway through their execution (see #resume[:through:] methods).
"

"
one single handled error scenario:
.......................................................
"
[
	[
		[ ] ensure: [
			[self error: #unwind_test] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3]
] on: Error do: [].
Transcript show: #x4

"prints x1 x3 x4 only, skips x2"

[
	[
		[ ] ensure: [
			[] ensure: [
				self error: #unwind_test.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3]
] on: Error do: [].
Transcript show: #x4

"prints x3 x4 only, skips x1 and x2"

"
multiple nested handled errors:
...................................................
"
[
	[
		[ ] ensure: [
			[self error: #unwind_test_outer] ensure: [
				self error: #unwind_test_inner.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3]
] on: Error do: [].
Transcript show: #x4

"prints x3 x4 only, skips x1 and x2"

"
non-local return scenario:
.........................................
"
[
	[ ] ensure: [
		[^Transcript show: #x0] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x0 x1 x3 only, skips x2"


[
	[ ] ensure: [
		[] ensure: [
			^Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x3 only, skips x2"


"
Part 4: Crazies - explore new teminate behavior and semantics under more extreme circumstances:
==================================================================
Show that the system will not crash and execute all pending unwind blocks as expected.
"

"
Unwind after active process termination; run in the UI. CAUTION: save image first!
All examples following this one can be run in the UI; forking them is for convenience.
"
[ ] ensure: [
	[Processor activeProcess terminate] ensure: [
		Transcript show: #x1]. 
	Transcript show: #x2
	]
"prints x1 x2 and suspends the UI; recover via Alt+."


"
Unwind after active process termination; run forked
"
[
	[ ] ensure: [
		[Processor activeProcess terminate] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2
		]
] forkAt: Processor activePriority + 1
"prints x1 x2"


"
Unwind after double active process termination:
"
| p |
p := [
	[Processor activeProcess terminate] 	ensure: [
		Processor activeProcess terminate. Transcript show: #x1 ]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 true"


"
Unwind after double active process termination (equivalent to previous):
"
| p ap |
p := [
	[ap := Processor activeProcess. ap terminate] 	ensure: [
		ap terminate. Transcript show: #x1]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 true"


"
Unwind after double active process termination inside the same unwind block:
"
| p |
p := [
	[] ensure: [ 
		Processor activeProcess terminate. Transcript show: #x1. 
		Processor activeProcess terminate. Transcript show: #x2]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 x2 true"


"
Unwind after double active process termination inside nested unwind blocks:
"
| p |
p := [
	[
		[] ensure: [
			[Processor activeProcess terminate] ensure: [
				Processor activeProcess terminate. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 x2 x3 true"


"
Unwind after active process termination combined with non-local return:
"
| p |
p := [
	[] ensure: [ 
		Processor activeProcess terminate. Transcript show: #x1. 
		true ifTrue: [^Transcript show: #x2]]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume
"prints x1 x2 and raises BlockCannotReturn error as expected"


"
Unwind after triple nested active process termination - run forked:
"
[
	[Processor activeProcess terminate] ensure: [
	    [Processor activeProcess terminate] ensure: [
	        [Processor activeProcess terminate] ensure: [
	            Transcript show: #x1].
	        Transcript show: #x2].
	    Transcript show: #x3].
	Transcript show: #x4
] forkAt: Processor activePriority + 1
"prints x1 x2 x3"


"
Unwind after triple nested active process termination - run in the UI:
"
[Processor activeProcess terminate] ensure: [
    [Processor activeProcess terminate] ensure: [
        [Processor activeProcess terminate] ensure: [
            Transcript show: #x1].
        Transcript show: #x2].
    Transcript show: #x3].
Transcript show: #x4
"prints x1 x2 x3 and suspends the UI; recover via Alt+."


"
Unwind after active process suspension during termination inside nested unwind blocks:
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				Processor activeProcess suspend. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
"first terminate gets suspended inside the unwind block as expected"
p terminate.
"second terminate (or resume) then finishes the termination"
Transcript show: p isTerminated printString
"prints x1 x2 x3 true"


"
Unwind multiple nested active process suspension:
"
| p |
p := [
	[Processor activeProcess suspend] ensure: [
	    [Processor activeProcess suspend] ensure: [
	        [Processor activeProcess suspend] ensure: [
	            Transcript show: #x1].
	        Transcript show: #x2].
	    Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
"p stops at the first suspend"
p terminate.
"termiination stops at the second suspend"
p resume.
"after resuming, p stops at the next suspend"
p resume

"prints x1 x2 x3"


"
Part 5: Some additional unsorted examples
============================
Show that the system will not crash and execute all pending unwind blocks as expected.
"

"
Raise a ZeroDivide exception after 1 second.
"
| p |
p := [ [ (Delay forSeconds: 100) wait ] ensure: [ 1 / 0 ] ] newProcess resume.
(Delay forSeconds: 1) wait.
p terminate.


"
BlockCannotReturn error inside nested unwind block during termination:
(this is one of many similar test scenarios in ProcessTerminateUnwindNonLocalReturn class)
"
| p a |
a := Array new: 4 withAll: false.
p := [	[[
			[ ] ensure: [
				[Processor activeProcess suspend] ensure: [
					^a at: 1 put: true].
				a at: 2 put: true]
		] ensure: [
			a at: 3 put: true].
		a at: 4 put: true
	] ensure: [
		Transcript show: a]
] newProcess.
p resume.
Processor yield.
p terminate
"prints #(true false true false)"
"!!! without fork crashes image"

"
Show the examples terminate peacefully after Abandoning the BlockCannotReturn notifications.
CAUTION: Proceeding BlockCannotReturn debugger notifications WILL crash your image !!! 
Save the image first if you want to try.
"

[^2] fork

[[self error: #error] ensure: [^2]] fork

[[[] ensure: [^2]] ensure: [^42]] fork

[[] ensure: [[^2] fork]] fork

"
Guess the result of this test:
"
"Both statements need to be executed separately."
a := [true ifTrue: [^ 1]. 2]
"execute separately"
a value
"What would you say should be the right answer and why??"


"
This example will immediately crash your image !!!
"
"Both statements need to be executed separately."
a := [true ifTrue: [^ 1] yourself]
"execute separately"
[a value] on: BlockCannotReturn do: [:ex | ex resume]


"
Note: The following examples require a fix in #doesNotUnderstand: preventing an infinite recursion of the MNU error.
"

"
Show both ZeroDivide and MessageNotUnderstood errors are raised and all unwind blocks executed after closing the debuggers.
"
[
	[
		[ ] ensure: [ "halfway through completion when suspended"
			[ ] ensure: [ "halfway through completion when suspended"
				Transcript show: (1 / 0) > 0]. "raise ZeroDivide AND MessageNotUnderstood errors"
			Transcript show: #x2]
	] ensure: [ "not started yet when suspended"
		Transcript show: #x3]
] fork
"prints Error: infinite recursion in doesNotUnderstand: x2 x3"


"
Show the following examples deal with MessageNotUnderstood error correctly and do nott crash or freeze the image or cycle through the MessageNotUnderstood error indefinitely. (Note: a fix in #doesNotUnderstand: preventing an infinite recursion of the MNU error needs to be applied.)
Accept gotcha when asked (it represents an unknown method) and Proceed or Abandon as you like...
"
[] ensure: [self gotcha. Transcript show: #done]

[] ensure: [[self gotcha] ensure: [self halt. Transcript show: #done]

[self error: #error] ensure: [self gotcha. Transcript show: #done]

[self error: #error] ensure: [[self gotcha] ensure: [Transcript show: #done]]

[[] ensure: [[self gotcha] ensure: [self halt. Transcript show: #done]]] fork
"This one used to freeze Cuis UI after Halt -> Proceed but recoverable via Alt+.
Latest terminate with the above mentioned #dnu fix executes as expected."


'
]

{ #category : 'simulate execution' }
Process >> activateReturn: aContext value: value [
	"Activate 'aContext return: value', so execution will return to aContext's sender"

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext activateReturn: aContext value: value]
		onBehalfOf: self
]

{ #category : 'accessing' }
Process >> basicSuspendedContext [
	"Answer the context the receiver has suspended.
	The debugger needs access to this ivar even if the process appears to be terminated.
	To be better understood some day.
	This method was added when #suspendedContext was modified to answer nil for terminated processes."
	^suspendedContext
]

{ #category : 'printing' }
Process >> browserPrintString [
	^self browserPrintStringFull: true
]

{ #category : 'printing' }
Process >> browserPrintStringFull: aBoolean [

	^String streamContents: [ :stream |
		stream
			nextPutAll: self statusString;
			space.
		priority printOn: stream.
		stream nextPutAll: ' ('.
		self hash printOn: stream.
		stream
			nextPutAll: ') ';
			nextPutAll: self name.
		(UISupervisor animatedUIOf: self) ifNotNil: [ :ui |
			stream nextPutAll: ' - '.
			ui printOn: stream ].
		aBoolean ifTrue: [
			stream
				nextPutAll: ': ';
				nextPutAll: suspendedContext printString ]]
]

{ #category : 'accessing' }
Process >> calleeOf: aContext [
	"Return the context whose sender is aContext.  Return nil if aContext is on top.  Raise error if aContext is not in process chain."

	suspendedContext == aContext ifTrue: [^ nil].
	^ (suspendedContext findContextSuchThat: [:c | c sender == aContext])
		ifNil: [self error: 'aContext not in process chain']
]

{ #category : 'simulate execution' }
Process >> canSearchForSignalerContextOf: anException [

	^ (anException class includesBehavior: Exception)
		and: [anException canSearchForSignalerContext]
]

{ #category : 'simulate execution' }
Process >> complete: aContext [
	"Run self until aContext is popped or an unhandled error is raised.  Return self's new top context, unless an unhandled error was raised then return the signaler context (rather than open a debugger)."

	| ctxt pair error returnContext |
	ctxt := suspendedContext.
	suspendedContext := nil. "disable this process while running its stack in active process below"
	"adjust the returning context to account for a non-local return to avoid the BCR error when stepping over ^2 in [^2] ensure: []"
	((returnContext := aContext) 
		findContextSuchThat: [:ctx | ctx selector == #aboutToReturn:through:]) 
		ifNotNil: [:ctx | returnContext := ctx tempAt: 2].
	pair := Processor activeProcess
		evaluate: [ctxt runUntilErrorOrReturnFrom: returnContext]
		onBehalfOf: self.
	suspendedContext := pair first.
	error := pair second.
	error ifNotNil: [^ error signalerContext].
	^ suspendedContext
]

{ #category : 'simulate execution' }
Process >> completeStep: aContext  [
	"Resume self until aContext is on top, or if already on top, complete next step"

	| callee |
	self isTerminated ifTrue:
		[^ nil].
	self suspendedContext == aContext ifFalse:
		[^ self complete: (self calleeOf: aContext)].
	callee := self step.
	callee == aContext ifTrue: [^ callee].
	aContext isDead ifTrue: [^ self suspendedContext].  "returned"
	^ self complete: callee  "finish send"
]

{ #category : 'simulate execution' }
Process >> completeStepUpTo: aContext [
	
	[aContext == suspendedContext] 
		whileFalse: [self completeStep: suspendedContext].
]

{ #category : 'simulate execution' }
Process >> completeTo: aContext  [
	"Resume self until aContext is on top"

	self suspendedContext == aContext ifTrue: [^ aContext].
	^ self complete: (self calleeOf: aContext)
]

{ #category : 'accessing' }
Process >> copyStack [

	^ self copy install: suspendedContext copyStack
]

{ #category : 'debugging' }
Process >> debugContext: context title: title [
	"Open debugger on self with context shown on top"

	self isTerminated ifTrue: [^ self error: 'can not debug a terminated process'].
	Debugger openProcess: self context: context label: title
]

{ #category : 'debugging' }
Process >> debugWithTitle: title [
	"Open debugger on self"

	| context |
	context := self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	self debugContext: context title: title
]

{ #category : 'debugging' }
Process >> effectiveProcess [
	"If we are debugging a Process, simulating its execution as we step in the Debugger even if it is suspended, answer it.
	Otherwise answer self.
	See #evaluate:onBehalfOf:
	"

	^self environmentAt: #effectiveProcess ifAbsent: self
]

{ #category : 'process state' }
Process >> environmentAt: key  ifAbsent: aBlock [
	
	^(env ifNil: [ ^aBlock value ]) at: key ifAbsent: aBlock.
]

{ #category : 'process state' }
Process >> environmentAt: key ifAbsentPut: aBlock [
	
	^(env ifNil: [ env := Dictionary new ]) at: key ifAbsentPut: aBlock.
]

{ #category : 'process state' }
Process >> environmentAt: key put: value [
	
	^(env ifNil: [ env := Dictionary new ]) at: key put: value.
]

{ #category : 'process state' }
Process >> environmentRemoveKey: key ifAbsent: errorBlock [
	
	^(env ifNil: [ ^errorBlock value ]) removeKey: key ifAbsent: errorBlock
]

{ #category : 'debugging' }
Process >> evaluate: aBlock onBehalfOf: aProcess [
	"Evaluate aBlock setting effectiveProcess to aProcess.
	Needed so #effectiveProcess gives the correct answer.
	See senders."

	| oldEffectiveProcess |
	aProcess == self ifTrue: [^ aBlock value]. "Optimization"
	
	oldEffectiveProcess := self environmentAt: #effectiveProcess ifAbsent: nil.
	self environmentAt: #effectiveProcess put: aProcess.
	^ aBlock ensure: [
		oldEffectiveProcess
			ifNotNil: [ self environmentAt: #effectiveProcess put: oldEffectiveProcess]
			ifNil: [ self environmentRemoveKey: #effectiveProcess ifAbsent: nil ]]
]

{ #category : 'simulate execution' }
Process >> ifSuspendedContextIsUnhandledErrorDo: aBlock [

	| unhandledError |
	
	self isSuspendedContextSignalUnhandledError ifTrue: [ 
		unhandledError := suspendedContext tempAt: 1.
		(self canSearchForSignalerContextOf: unhandledError) ifTrue: [ 
			aBlock value: unhandledError ]].
]

{ #category : 'simulate execution' }
Process >> install: aContext  [
	"Replace the suspendedContext with aContext."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot install contexts'].
	suspendedContext := aContext
]

{ #category : 'testing' }
Process >> isActiveProcess [
	"Are we the cunrrently running process?
	Just for compatibility."
	^self isRunning
]

{ #category : 'testing' }
Process >> isBlocked [
	"Answer true if blocked on a semaphore."
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList class == Semaphore
]

{ #category : 'testing' }
Process >> isInExternalCode [
	"In a threaded VM, a Smalltalk Process may run a primitive in the background.
	Answer whether this is currently the case for this Process."

	"Special Object 41 (40 in VM) is the ProcessInExternalCodeTag, which is an empty LinkedList."
	^ myList == (Smalltalk specialObjectsArray at: 41)
]

{ #category : 'testing' }
Process >> isReady [
	"True if not running right now, but might be scheduled anytime.

	Useful to play with:

	| s |	
	s := Semaphore new.
	s inspect.
	([ s wait . 10000 timesRepeat: [ 10000 factorial. (Delay forSeconds: 1) wait. 7 print ]] newProcess name: 'test5') inspect
	
	{ self isRunning . self isReady . self isSuspended . self isBlocked . self isTerminated } print
	"
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList class == LinkedList
]

{ #category : 'testing' }
Process >> isRunning [
	"Are we the cunrrently running process?"
	^ self == Processor activeProcess
]

{ #category : 'simulate execution' }
Process >> isSimulatingAnotherProcess [
	"Answer true if we are a debugger process, simulating the execution of another."

	^self effectiveProcess ~~ self
]

{ #category : 'testing' }
Process >> isSuspended [
	"Answer true if I was never scheduled yet (new process, never been sent #resume) or paused (was sent #suspend)"
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList isNil
]

{ #category : 'simulate execution' }
Process >> isSuspendedContextSignalUnhandledError [

	^ suspendedContext selector == #signalForException:
		and: [suspendedContext receiver isBehavior 
		and: [
			(suspendedContext receiver includesBehavior: UnhandledError) or: [
				suspendedContext receiver includesBehavior: UnhandledHalt ]]]
]

{ #category : 'testing' }
Process >> isTerminated [
	"Answer if the receiver is terminated. A healthy process is considered 
	 terminated if the suspendedContext has no unexecuted code left."

	self isRunning ifTrue: [^ false].
	^suspendedContext isNil or: [suspendedContext isDone]
]

{ #category : 'testing' }
Process >> isTheLastPreempted [
	"Answer wether I am the last process that was preempted"
	^self == Processor preemptedProcess
]

{ #category : 'printing' }
Process >> longPrintOn: stream [

	| ctxt |
	super printOn: stream.
	stream newLine.
	ctxt := self suspendedContext.
	[ctxt == nil] whileFalse: [
		stream space.
		ctxt printOn: stream.
		stream newLine.
		ctxt := ctxt sender.
	]
]

{ #category : 'accessing' }
Process >> name [

	^name ifNil: [ 'unnamed' ]
]

{ #category : 'accessing' }
Process >> name: aString [

	name := aString
]

{ #category : 'objects from disk' }
Process >> objectForDataStream: refStrm [
	"I am not allowed to be written on an object file."

	refStrm replace: self with: nil.
	^ nil
]

{ #category : 'simulate execution' }
Process >> popTo: aContext  [
	"Pop self down to aContext by remote returning from aContext's callee.  Unwind blocks will be executed on the way.
	This is done by pushing a new context on top which executes 'aContext callee return' then resuming self until aContext is reached.  This way any errors raised in an unwind block will get handled by senders in self and not by senders in the activeProcess.
	If an unwind block raises an error that is not handled then the popping stops at the error and the signalling context is returned, othewise aContext is returned."

	self == Processor activeProcess ifTrue:
		[^self error: 'The active process cannot pop contexts'].
	^(self calleeOf: aContext)
		ifNil: [aContext]  "aContext is on top"
		ifNotNil:
			[:callee|
			 Processor activeProcess
				evaluate: [self return: callee value: callee receiver]
				onBehalfOf: self]
]

{ #category : 'simulate execution' }
Process >> popTo: aContext value: aValue [
	"Replace the suspendedContext with aContext, releasing all contexts 
	 between the currently suspendedContext and it."

	self == Processor activeProcess ifTrue:
		[^self error: 'The active process cannot pop contexts'].
	^(self calleeOf: aContext)
		ifNil: [aContext]  "aContext is on top"
		ifNotNil:
			[:callee|
			 Processor activeProcess
				evaluate: [self return: callee value: aValue]
				onBehalfOf: self]
]

{ #category : 'changing process state' }
Process >> primitiveResume [
	"Primitive. Allow the process that the receiver represents to continue. Put 
	the receiver in line to become the activeProcess. Fail if the receiver is 
	already waiting in a queue (in a Semaphore or ProcessScheduler). 
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 87>
	self primitiveFailed
]

{ #category : 'printing' }
Process >> printOn: aStream [

	super printOn: aStream.
	aStream
		space;
		nextPutAll: (self browserPrintStringFull: false)
]

{ #category : 'accessing' }
Process >> priority [
	"Answer the priority of the receiver."
	^priority
]

{ #category : 'accessing' }
Process >> priority: anInteger  [
	"Set the receiver's priority to anInteger."
	priority := anInteger min: Processor highestPriority max: Processor lowestPriority.
]

{ #category : 'private' }
Process >> resolvePendingQuickReturns [
	| method |	
	method := suspendedContext method.
	method isQuick ifTrue: [
		Processor activeProcess
			evaluate: [suspendedContext := suspendedContext callPrimitive: method primitive]
			onBehalfOf: self ]
]

{ #category : 'simulate execution' }
Process >> restartTop [
	"Rollback top context and replace with new method.  Assumes self is suspended"

	suspendedContext privRefresh
]

{ #category : 'changing suspended state' }
Process >> restartTopWith: method [
	"Rollback top context and replace with new method.  Assumes self is suspended"

	method isQuick 
		ifTrue: [self popTo: suspendedContext sender]
		ifFalse:
			[suspendedContext method frameSize >= method frameSize
				ifTrue: [suspendedContext privRefreshWith: method]
				ifFalse:
					[self assert: suspendedContext isExecutingBlock not.
					 suspendedContext := MethodContext
												sender: suspendedContext sender
												receiver: suspendedContext receiver
												method: method
												arguments: ((1 to: method numArgs) collect:
																[:i| suspendedContext tempAt: i])]].

]

{ #category : 'changing process state' }
Process >> resume [
	"Allow the process that the receiver represents to continue. Put  
	the receiver in line to become the activeProcess. Check for a nil 
	suspendedContext, which indicates a previously terminated Process that 
	would cause a vm crash if the resume attempt were permitted.
	
	This resumes the receiver if
		- It was never scheduled
		- it is suspended (was sent #suspend)
		- It is ready to run (was preempted). In this case there is no effect.
		- it is waiting on a semaphore. In this case, the semaphore is ignored.
	"

	suspendedContext ifNil: [^ self primitiveFailed].
	^ self primitiveResume
]

{ #category : 'changing process state' }
Process >> resumeAt: aPriority [
	self priority: aPriority.
	^self resume
]

{ #category : 'simulate execution' }
Process >> return: aContext value: value [
	"Pop thread down to aContext's sender.  Execute any unwind blocks on the way.  See #popTo: comment and #runUntilErrorOrReturnFrom: for more details."

	suspendedContext == aContext ifTrue:
		[^Processor activeProcess
			evaluate: [suspendedContext := aContext return: value from: aContext]
			onBehalfOf: self].
	self activateReturn: aContext value: value.
	^self complete: aContext
]

{ #category : 'changing process state' }
Process >> run [
	"Suspend current process and execute self instead"

	| proc |
	proc := Processor activeProcess.
	[	proc suspend.
		self resume.
	] forkAt: Processor highestPriority
]

{ #category : 'signaling' }
Process >> signalException: anException [
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"

	"If we are the active process, go ahead and signal the exception"
	 self isActiveProcess ifTrue: [^anException signal].

	"One could raise an error here but that would force clients to employ a guard.
	 Since a terminated process can have no response to a signal (there is no
	 where for a handler to exist) it is legitimate to simply discard the signal."
	suspendedContext ifNil: [^self].

	"Suspend myself first to ensure that I won't run away
	 in the midst of the following modifications."
	self isSuspended ifFalse:
		[self suspend].

	"Add a new block context to the stack that will signal the exception (jar 6/27/2022)"
	suspendedContext := 
		[anException signal. thisContext sender jump] asContextWithSender: suspendedContext.
	"Since this block is not called in a normal way, we need to take care
	that it doesn't directly return to the caller (because that could have 
	the potential to push an unwanted object on the caller's stack).
	Workspace example:
		p := [Semaphore new wait] fork.
		Processor yield.
		p signalException: Notification
	"
	^self resume
]

{ #category : 'printing' }
Process >> statusString [
	self isRunning ifTrue: [ ^ 'running' ].
	self isReady ifTrue: [ ^ 'ready' ].
	self isSuspended ifTrue: [ ^ 'suspended' ].
	self isBlocked ifTrue: [ ^ 'blocked' ].
	self isTerminated ifTrue: [ ^'terminated' ].
	^'unknown (bug?)'
]

{ #category : 'simulate execution' }
Process >> step [

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext step]
		onBehalfOf: self
]

{ #category : 'simulate execution' }
Process >> step: aContext  [
	"Resume self until aContext is on top, or if already on top, do next step"

	^Processor activeProcess
		evaluate:
			[self suspendedContext == aContext
				ifTrue: [suspendedContext := suspendedContext step]
				ifFalse: [self complete: (self calleeOf: aContext)]]
		onBehalfOf: self
]

{ #category : 'simulate execution' }
Process >> stepToCallee [
	"Step until top context changes"

	Processor activeProcess
		evaluate:
			[| ctxt |
			ctxt := suspendedContext.
			[ctxt == suspendedContext] whileTrue: [
				suspendedContext := suspendedContext step]]
		onBehalfOf: self.
	^suspendedContext
]

{ #category : 'simulate execution' }
Process >> stepToHome: requestedContext [
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| requestedHome |
	Processor activeProcess
		evaluate:
			[
				requestedHome := requestedContext home.
				[
					suspendedContext := suspendedContext step.
				
					"We reached the home context of the block we are doing [Through] on."
					suspendedContext home == requestedHome ifTrue: [
						^suspendedContext ].
				
					"But if the requestedHome is dead, meaning that we'll never reach back there,
					stop when we get back to where [Through] was requested."
					requestedHome isDead ifTrue: [
						suspendedContext = requestedContext ifTrue: [
							^suspendedContext ]].
				
					"There's nowhere to continue stepping into. Don't do it."
					(suspendedContext sender isNil or: [suspendedContext sender isDead]) ifTrue: [
						^suspendedContext ].
				
					self ifSuspendedContextIsUnhandledErrorDo: [ :anError |
						anError signalerContext ifNotNil: [ :unhandledErrorSignalerContext |
							self completeStepUpTo: unhandledErrorSignalerContext.
							"Give a debugger a chance to update its title to reflect the new exception"
							 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
							^unhandledErrorSignalerContext ]]
				
				] repeat.
			]
		onBehalfOf: self.
]

{ #category : 'simulate execution' }
Process >> stepToSendOrReturn [

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext stepToSendOrReturn]
		onBehalfOf: self
]

{ #category : 'printing' }
Process >> storeOn: aStream [
	aStream nextPutAll: '("a Process. Can not be serialized")'.
]

{ #category : 'changing process state' }
Process >> suspend [
	"eem 1/3/2022 10:38:
	 Primitive. Suspend the receiver, aProcess, such that it can be executed again
	 by sending #resume. If the given process is not the active process, take it off
	 its corresponding list. If the list was not its run queue assume it was on some
	 condition variable (Semaphore, Mutex) and back up its pc to the send that
	 invoked the wait state the process entered.  Hence when the process resumes
	 it will reenter the wait state. Answer the list the receiver was previously on iff
	 it was not active and not blocked, otherwise answer nil."

	<primitive: 578 error: ec>
	"This is fallback code for VMs which only support the old primitiveSuspend 88.
	 Note: in this case some tests may fail and some methods assuming the revised
	 suspend semantics described above may not work entirely as expected (e.g.
	 Context >> #releaseCriticalSection or Process >> #signalException)."
	 ^self suspendAndUnblock
]

{ #category : 'private' }
Process >> suspendAndReleaseCriticalSection [
	"Figure out if we are terminating a process that is in the ensure: block of a critical section."

	| oldList |
	"Suspend and unblock the receiver from a condition variable using suspend primitive #88.
	 It answers the list the receiver was on before the suspension."
	oldList := 	self suspendAndUnblock.
	^suspendedContext ifNotNil: [:context |
		suspendedContext := nil.
		(oldList class == Semaphore and: [
			context method == (Semaphore compiledMethodAt: #critical:)])
				ifTrue: [context home	]
				ifFalse: [context]]
]

{ #category : 'changing process state' }
Process >> suspendAndUnblock [
	"ar 12/7/2007 17:10:
	 Primitive. Stop the process that the receiver represents in such a way 
	 that it can be restarted at a later time (by sending the receiver the 
	 message resume). If the receiver represents the activeProcess, suspend it. 
	 Otherwise remove the receiver from the list of waiting processes.
	 The return value of this method is the list the receiver was previously on (if any)."

	<primitive: 88 error: ec>
	"ar 12/7/2007 17:10:
	 This is fallback code for VMs which only support the old primitiveSuspend which 
	 would not accept processes that are waiting to be run."
	 ^myList ifNotNil: "this allows us to use suspend multiple times"
		[:oldList|
		 myList := nil.
		 oldList remove: self ifAbsent: nil.
		 oldList]
]

{ #category : 'test support' }
Process >> suspendPrimitivelyOrFail [
	"Test support. Execute primitive 578, or fail.
	(We used to call primitive 88, the old suspend primitive, replaced by 578 in 2022)"

	<primitive: 578 error: ec>
	^self primitiveFailed
]

{ #category : 'accessing' }
Process >> suspendedContext [
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

	^suspendedContext
]

{ #category : 'private' }
Process >> suspendedContext: aContext  [
	suspendedContext := aContext
]

{ #category : 'accessing' }
Process >> suspendingList [
	"Answer the list on which the receiver has been suspended."

	"myList can be:
		- A LinkedList: The Processor queue we are in. This, if we are Ready to Run.
		- A Semaphore. This, if we are Blocked waiting on it.
		- nil otherwise, i.e. if we are Running, Suspended, Terminated."
	^myList
]

{ #category : 'changing process state' }
Process >> terminate [
	"Stop the process that the receiver represents forever. Allow all pending unwind
	 blocks to run before terminating; if they are currently in progress, let them finish.
	 This is the kind of behavior we expect when terminating a healthy process.
	 See further comments in #terminateAggressively and #destroy methods dealing
	 with process termination when closing the debugger or after a catastrophic failure."
	
	self isActiveProcess ifTrue: [^thisContext unwindAndStop jump]. "Note 1"

	[[:contextToUnwind | "Note 2"
	self 
		priority: Processor activePriority;  "Note 3"
		suspendedContext: contextToUnwind unwindAndStop; "Note 4"
		resume
	] valueAndWaitWhileUnwinding: "Note 5" self suspendAndReleaseCriticalSection "Note 6"] valueEnsured

"Note 1: If terminating the active process, create a new stack and run unwinds from there.

Note 2: Execute the termination in #ensure: argument block to guarantee it completes even if the terminator process itself gets terminated before it's finished. Otherwise #testTerminateInTerminate would fail.

Note 3: Adjust the terminating process's priority to prevent the active process from getting stuck waiting on a lower priority process to complete its termination.

Note 4: To allow a suspended process to unwind itself, create a new, parallel stack and resume the suspended process from there to unwind itself.

Note 5: Use a semaphore to make the process that invoked the termination wait for self's completion.

Note 6: If terminating a suspended process (including runnable and blocked), always suspend the process being terminated first so it doesn't accidentally get woken up, and nil the suspended context to prevent accidental resumption or termination with a wrong suspended context while manipulating it. If the process is blocked, waiting to access the #critical: section of a condition variable (a Semaphore or a Mutex), release it properly.

In 2022 a new suspend semantics has been introduced: the revised #suspend backs up a process waiting on a condition variable to the send that invoked the wait state, while the previous #suspend simply removed the process from the condition variable's list it was previously waiting on; see #suspend and #suspendAndUnblock comments."

]
