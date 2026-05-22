"
My single instance, named Processor, coordinates the use of the physical processor by all Processes requiring service.
"
Class {
	#name : 'ProcessorScheduler',
	#superclass : 'Object',
	#instVars : [
		'quiescentProcessLists',
		'activeProcess'
	],
	#classVars : [
		'BackgroundProcess'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'priority names' }
ProcessorScheduler class >> highIOPriority [
	"Answer the priority at which the most time critical input/output 
	processes should run. An example is the process handling input from a 
	network."

	^70
]

{ #category : 'background process' }
ProcessorScheduler class >> idleProcess [
	"A default background process which is invisible."

	[true] whileTrue:
		[self relinquishProcessorForMicroseconds: 1000]
]

{ #category : 'priority names' }
ProcessorScheduler class >> lowIOPriority [
	"Answer the priority at which most input/output processes should run. 
	Examples are the process handling input from the user (keyboard, 
	pointing device, etc.) and the process distributing input from a network."

	^60
]

{ #category : 'priority names' }
ProcessorScheduler class >> lowestPriority [
	"Return the lowest priority that is allowed with the scheduler"

	^10
]

{ #category : 'instance creation' }
ProcessorScheduler class >> new [
	"New instances of ProcessorScheduler should not be created."

	self error:
'New ProcessSchedulers should not be created since
the integrity of the system depends on a unique scheduler'
]

{ #category : 'background process' }
ProcessorScheduler class >> relinquishProcessorForMicroseconds: anInteger [
	"Platform specific. This primitive is used to return processor cycles to the host operating system when Squeak's idle process is running (i.e., when no other Squeak process is runnable). On some platforms, this primitive causes the entire Squeak application to sleep for approximately the given number of microseconds. No Squeak process can run while the Squeak application is sleeping, even if some external event makes it runnable. On the Macintosh, this primitive simply calls GetNextEvent() to give other applications a chance to run. On platforms without a host operating system, it does nothing. This primitive should not be used to add pauses to a Squeak process; use a Delay instead."

	<primitive: 230>
	"don't fail if primitive is not implemented, just do nothing"

]

{ #category : 'background process' }
ProcessorScheduler class >> startUp [
	"Install a background process of the lowest possible priority that is always runnable."
	"Details: The virtual machine requires that there is aways some runnable process that can be scheduled; this background process ensures that this is the case."

	Smalltalk installLowSpaceWatcher.
	self stopBackgroundProcess.
	BackgroundProcess := [self idleProcess] newProcess.
	BackgroundProcess priority: self lowestPriority.
	BackgroundProcess name: 'Idle Process'.
	BackgroundProcess resume.

]

{ #category : 'background process' }
ProcessorScheduler class >> stopBackgroundProcess [
	"kill the background process. Only call if you know what you are doing, or are happy to try dangerous behavior"

	BackgroundProcess ifNotNil: [BackgroundProcess terminate].
	BackgroundProcess := nil
]

{ #category : 'priority names' }
ProcessorScheduler class >> systemBackgroundPriority [
	"Answer the priority at which system background processes should run. 
	Examples are an incremental garbage collector or status checker."

	^20
]

{ #category : 'priority names' }
ProcessorScheduler class >> timingPriority [
	"Answer the priority at which the system processes keeping track of real 
	time should run."

	^80
]

{ #category : 'priority names' }
ProcessorScheduler class >> userBackgroundPriority [
	"Answer the priority at which user background processes should run."

	^30
]

{ #category : 'priority names' }
ProcessorScheduler class >> userInterruptPriority [
	"Answer the priority at which user processes desiring immediate service 
	should run. Processes run at this level will preempt the window 
	scheduler and should, therefore, not consume the processor forever."

	^50
]

{ #category : 'priority names' }
ProcessorScheduler class >> userSchedulingPriority [
	"Answer the priority at which the window scheduler should run."

	^40
]

{ #category : 'accessing' }
ProcessorScheduler >> activePriority [
	"Answer the priority level of the currently running Process."

	^activeProcess priority
]

{ #category : 'accessing' }
ProcessorScheduler >> activeProcess [
	"Answer the currently running Process."

	^activeProcess
]

{ #category : 'private' }
ProcessorScheduler >> anyProcessesAbove: highestPriority  [
	"Do any instances of Process exist with higher priorities?"
	self processesDo: [ :p |
		p priority > highestPriority ifTrue: [ ^true ]].
	^false.
]

{ #category : 'services' }
ProcessorScheduler >> anyReceiverInStackIn: anArray [
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument"
	"
	Processor anyReceiverInStackIn: { self runningWorld }
	Processor anyReceiverInStackIn: { Object new }
	"
	self
		processesDo: [ :p | ]
		withStackFramestDo: [ :p :c |
			(anArray statePointsTo: c receiver)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: nil.
	^ false
]

{ #category : 'services' }
ProcessorScheduler >> anyReceiverInStackInstanceOfClassIn: anArray [
	"Iterate over all methods currently in execution. Answer true if in any of them 'self class' an element of argument"
	"
	Processor anyReceiverInStackInstanceOfClassIn: { WorldMorph }
	Processor anyReceiverInStackInstanceOfClassIn: { Text }
	"
	self
		processesDo: [ :p | ]
		withStackFramestDo: [ :p :c |
			(anArray statePointsTo: c receiver class)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: nil.
	^ false
]

{ #category : 'accessing' }
ProcessorScheduler >> backgroundProcess [
	"Answer the background process"
	^ BackgroundProcess
]

{ #category : 'priority names' }
ProcessorScheduler >> highIOPriority [
	"Answer the priority at which the most time critical input/output 
	processes should run. An example is the process handling input from a 
	network."

	^self class highIOPriority
]

{ #category : 'accessing' }
ProcessorScheduler >> highestPriority [
	"Answer the number of priority levels currently available for use."

	^quiescentProcessLists size
]

{ #category : 'accessing' }
ProcessorScheduler >> highestPriority: newHighestPriority [
	"Change the number of priority levels currently available for use."

	| continue newProcessLists |
	(quiescentProcessLists size > newHighestPriority
		and: [self anyProcessesAbove: newHighestPriority])
			ifTrue: [self error: 'There are processes with priority higher than '
													,newHighestPriority printString].
	newProcessLists := Array new: newHighestPriority.
	1 to: ((quiescentProcessLists size) min: (newProcessLists size)) do: 
		[:priority | newProcessLists at: priority put: (quiescentProcessLists at: priority)].
	quiescentProcessLists size to: newProcessLists size do: 
		[:priority | newProcessLists at: priority put: LinkedList new].
	quiescentProcessLists := newProcessLists
]

{ #category : 'priority names' }
ProcessorScheduler >> lowIOPriority [
	"Answer the priority at which most input/output processes should run. 
	Examples are the process handling input from the user (keyboard, 
	pointing device, etc.) and the process distributing input from a network."

	^self class lowIOPriority
]

{ #category : 'priority names' }
ProcessorScheduler >> lowestPriority [
	"Return the lowest priority that is allowed with the scheduler"
	^self class lowestPriority
]

{ #category : 'CPU usage tally' }
ProcessorScheduler >> nextReadyProcess [
	quiescentProcessLists reverseDo: [ :list |
		list isEmpty ifFalse: [ | proc |
			proc := list first.
			proc isReady ifTrue: [ ^proc ]]].
	^nil
]

{ #category : 'objects from disk' }
ProcessorScheduler >> objectForDataStream: refStrm [
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	dp := DiskProxy global: #Processor selector: #yourself args: #().
	refStrm replace: self with: dp.
	^ dp

]

{ #category : 'accessing' }
ProcessorScheduler >> preemptedProcess [
	"Return the process that the currently active process just preempted."
	
	self activeProcess priority to: 1 by: -1 do: [ :priority |
		(quiescentProcessLists at: priority) ifNotEmpty: [ :list |
			^ Smalltalk processPreemptionYields
				ifTrue: [ list last ]
				ifFalse: [ list first ]]].
	^ nil
	"
	Processor preemptedProcess
	"
]

{ #category : 'accessing' }
ProcessorScheduler >> processes [
	"Answer all non terminated Processes"
	^Process allSubInstances reject: [:each | each isTerminated ]
]

{ #category : 'accessing' }
ProcessorScheduler >> processesDo: aBlock [
	"Iterate over processes that can run"
	"
	Processor processesDo: [ :p | p print ].
	"
	Process allSubInstancesDo: [ :p |
		p isTerminated ifFalse: [
			aBlock value: p ]]
]

{ #category : 'accessing' }
ProcessorScheduler >> processesDo: aBlock withStackFramestDo: twoArgBlock runningProcessSearchStart: aContextOrNil [
	"Iterate over processes that can run.
	For each process, iterate over stack frames (i.e. contexts)."
	"
	Processor
		processesDo: [ :p |
			'--------------' print.
			p print. 
			'--------------' print ]
		withStackFramestDo: [ :p :c |
			('        ', c printString) print ].
	"
	self
		processesWithTopContextDo: [ :process :topContext | | context |
			aBlock value: process.
			context := topContext.
			[ context notNil ] whileTrue: [
				twoArgBlock value: process value: context.
				context := context sender ]]
		runningProcessSearchStart: aContextOrNil
]

{ #category : 'accessing' }
ProcessorScheduler >> processesWithTopContextDo: aBlock runningProcessSearchStart: aContextOrNil [
	"Iterate over processes that can run. Include top context in block arguments."
	"
	Processor processesWithTopContextDo: [ :p :c | p print. ('------->', c printString) print ].
	"
	self processesDo: [ :process |
		aBlock
			value: process
			value: (process isRunning ifTrue: [ aContextOrNil ifNil: [thisContext] ] ifFalse: [ process suspendedContext ]) ]
]

{ #category : 'removing' }
ProcessorScheduler >> remove: aProcess ifAbsent: aBlock  [
	"Remove aProcess from the list on which it is waiting for the processor. If 
	it is not waiting, evaluate aBlock."

	(quiescentProcessLists at: aProcess priority)
		remove: aProcess ifAbsent: aBlock.
	^aProcess
]

{ #category : 'printing' }
ProcessorScheduler >> storeOn: aStream [
	self == Processor ifTrue: [
		^ aStream nextPutAll: 'Processor'].
	^ super storeOn: aStream
]

{ #category : 'process state change' }
ProcessorScheduler >> suspendFirstAt: aPriority  [
	"Suspend the first Process that is waiting to run with priority aPriority."

	^self suspendFirstAt: aPriority
		  ifNone: [self error: 'No Process to suspend']
]

{ #category : 'process state change' }
ProcessorScheduler >> suspendFirstAt: aPriority ifNone: noneBlock  [
	"Suspend the first Process that is waiting to run with priority aPriority.  
	If no Process is waiting, evaluate noneBlock"

	| aList |
	aList := quiescentProcessLists at: aPriority.
	aList isEmpty
		ifTrue: [^noneBlock value]
		ifFalse: [^aList first suspend]
]

{ #category : 'priority names' }
ProcessorScheduler >> systemBackgroundPriority [
	"Answer the priority at which system background processes should run. 
	Examples are an incremental garbage collector or status checker."

	^self class systemBackgroundPriority
]

{ #category : 'CPU usage tally' }
ProcessorScheduler >> tallyCPUUsageFor: seconds every: msec [
	"Start a high-priority process that will tally the next ready process for the given
	number of seconds. Answer a Block that will return the tally (a Bag) after the task
	is complete" 
	| tally sem delay endDelay |
	tally := IdentityBag new.
	delay := Delay forMilliseconds: msec truncated.
	endDelay := Delay forSeconds: seconds.
	endDelay schedule.
	sem := Semaphore new.
	[
		[ endDelay isExpired ] whileFalse: [
			delay wait.
			tally add: Processor nextReadyProcess
		].
		sem signal.
	] forkAt: self highestPriority named: 'Processor CPU Usage Tallier'.

	^[ sem wait. tally ]
]

{ #category : 'process state change' }
ProcessorScheduler >> terminateActive [
	"Terminate the process that is currently running."

	activeProcess terminate
]

{ #category : 'priority names' }
ProcessorScheduler >> timingPriority [
	"Answer the priority at which the system processes keeping track of real 
	time should run."

	^self class timingPriority
]

{ #category : 'priority names' }
ProcessorScheduler >> userBackgroundPriority [
	"Answer the priority at which user background processes should run."

	^self class userBackgroundPriority
]

{ #category : 'priority names' }
ProcessorScheduler >> userInterruptPriority [
	"Answer the priority at which user processes desiring immediate service 
	should run. Processes run at this level will preempt the window 
	scheduler and should, therefore, not consume the processor forever."

	^self class userInterruptPriority
]

{ #category : 'priority names' }
ProcessorScheduler >> userSchedulingPriority [
	"Answer the priority at which the window scheduler should run."

	^self class userSchedulingPriority
]

{ #category : 'accessing' }
ProcessorScheduler >> waitingProcessesAt: aPriority [
	"Return the list of processes at the given priority level."
	^quiescentProcessLists at: aPriority
]

{ #category : 'process state change' }
ProcessorScheduler >> yield [
	"Give other Processes at the current priority a chance to run."

	| semaphore |

	<primitive: 167>
	semaphore := Semaphore new.
	[semaphore signal] fork.
	semaphore wait
]
