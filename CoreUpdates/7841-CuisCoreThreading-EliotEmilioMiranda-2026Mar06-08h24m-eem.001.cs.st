'From Cuis7.7 [latest update: #7840] on 6 March 2026 at 8:47:56 am'!
!classDefinition: #Process category: #'Kernel-Processes'!


!Process class methodsFor: 'TEMPORARY HACK' stamp: 'eem 6/Mar/2026 09:04:20'!
shouldNotBeRedefined "temporary; removed below" ^false! !

Link subclass: #Process
	instanceVariableNames: 'suspendedContext priority myList threadAffinity name env osError '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Processes'!

!Process commentStamp: '<historical>' prior: 0!
I represent an independent path of control in the system. This path of control may be stopped (by sending the message suspend) in such a way that it can later be restarted (by sending the message resume). When any one of several paths of control can be advanced, the single instance of ProcessorScheduler named Processor determines which one will actually be advanced, partly using the value of priority.

Instance Variables: N.B. The first four are used by the virtual machine. They must be defined, and defined in this order,
	suspendedContext:		<Context|nil>
	priority:					<Integer>
	myList:					<LinkedList|nil>
	threadAffinity:			<Integer|nil>
	osError:					<Integer|ByteArray|nil>
	name:					<String|nil>
	env:						<Dictionary|nil>

suspendedContext
	- if nil, then I am either the active process or I have been terminated.  If not nil it is the Context at the hot end of my stack.

priority
	- my scheduling priority, which corresponds to the index in ProcessorScheduler quiescentProcesses that holds the LinkedList of processes at this priority.  If I am running but quiescent, changing priority involves changing priority and myList.

myList
	- if nil, I am either suspended or the active process.  If not nil it is either some condition variable (Semaphore, Mutex) that I am blocked waiting on, or it is the LinkedList in ProcessorScheduler quiescentProcesses that holds the processes for my priority that are not the current active process but still runnable.

osError
	- used to record an error from an FFI call so that a) errors are per-process, rather than having a single error variable in the FFI plugin, and b) so that there is a place to collect library errors (e.g. errno and GetLastError) via the error reaper in a thread-safe manner immediately after making the main FFI call. If non-nil osError is either an integer (in the case of an error reaper) or a two-element ByteArray, with the last element being the FFILastError, in the case of an FFI callout failure during marshalling.
	
threadAffinity
	- if not nil then it is an integer value set by the VM that controls how I interact with underlying native threads. If positive, the threadAffinity is that of some native thread known to the VM, and the VM will ensure that when I am running, I will be running on that native thread.  If negative, the absolute value of threadAffinity is that of some thread known to the VM, and the VM will ensure that when I am running, I will *not* be running on that native thread.  Set by the setThreadAffinity: method only, and uonly on threaded VMs. See SmalltalkImage >> #processHasThreadAffinityInstVar: for further information.

name
	- if not nil this is my name, used for information, see e.g. the names of processes displayed in the ProcessBrowser

env
	- if not nil this is a Dictionary providing process-specific variables.  See e.g. environmentAt:*, DynamicVariable and ProcessSpecificVariable.!


!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 3/Mar/2026 18:40:00'!
processHasOSErrorInstVar
	"The threaded VM needs to know if the 5th inst var of Process
	 is osError which it uses to report FFI errors in a thread-safe manner."
	
	| flag |
	flag := self imageHeaderFlagAt: 0.
	self assert: [flag ==> [(Process instVarNames at: 5) = 'osError']].
	^ flag! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'mt 8/Dec/2023 11:01:00'!
processHasThreadAffinityInstVar
	"The threaded VM needs to know if the 4th inst var of Process
	 is threadAffinity which it uses to control process-to-thread binding."
	
	| flag |
	flag := self imageHeaderFlagAt: 0.
	self assert: [flag ==> [(Process instVarNames at: 4) = 'threadAffinity']].
	^ flag! !


!Process methodsFor: 'accessing' stamp: 'eem 3/Mar/2026 15:43:00'!
osError
	"Answer the receiver's osError from a recent FFI call, or nil.
	 N.B. these is no setter. It should be set only by the SqueakFFIPrims plugin."

	^osError! !

!Process methodsFor: 'threading' stamp: 'eem 6/Mar/2026 08:26:00'!
setThreadAffinity: anIntegerOrNil
	"Set the affinity of the receiver to a particular thread.
	 If anIntegerOrNil is nil, then make the receiver free to run on any thread.
	 If anIntegerOrNil is strictly positive, then affine the receiver to the thread with id anIntegerOrNil, and
	 arrange that the receiver will only run on that thread.  If anIntegerOrNil is negative then arrange
	 that the receiver will run on any thread other than the thread with id anIntegerOrNil.
	 Hence, if the result is an integer, when the receiver becomes active a thread switch may occur,
	 and will occur if necessary to meet these two constraints."
	<primitive: 229 error: ec>
	self primitiveFailed! !


!Process class methodsFor: 'class initialization' stamp: 'eem 3/Mar/2026 15:41:00'!
startUp: resuming
	"Ensure the VM's flags telling it about the presence of the threadAffinity and osError bits are up-to-date."

	resuming ifTrue:
		[| bits flags |
		"Map threadAffinity to 1, and osError to 8. These bits must be set for the VM to use the inst vars."
		 bits := ((instanceVariables indexOf: 'threadAffinity') / 4)
				+ ((instanceVariables indexOf: 'osError') * 8 / 5).
		 flags := Smalltalk vmParameterAt: 48.
		 (flags allMask: bits) ifFalse:
			[Smalltalk vmParameterAt: 48 put: ((flags bitClear: bits) bitOr: bits)]]! !

!methodRemoval: SystemDictionary #processHasThreadAffinityInstVar: stamp: 'eem 6/Mar/2026 08:39:27'!
SystemDictionary removeSelector: #processHasThreadAffinityInstVar:!
!classDefinition: #Process category: #'Kernel-Processes'!
Link subclass: #Process
	instanceVariableNames: 'suspendedContext priority myList threadAffinity osError name env'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Processes'!

Process startUp: true!

!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend suspendAndUnblock terminate)
('simulate execution' activateReturn:value: canSearchForSignalerContextOf: complete: completeStep: completeStepUpTo: completeTo: ifSuspendedContextIsUnhandledErrorDo: isSimulatingAnotherProcess isSuspendedContextSignalUnhandledError popTo: popTo:value: restartTop return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: osError priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString storeOn:)
('private' resolvePendingQuickReturns suspendAndReleaseCriticalSection suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debugContext:title: debugWithTitle: effectiveProcess evaluate:onBehalfOf:)
('signaling' signalException:)
('test support' suspendPrimitivelyOrFail)
('testing' isActiveProcess isBlocked isInExternalCode isReady isRunning isSuspended isTerminated isTheLastPreempted)
('process state' environmentAt:ifAbsent: environmentAt:ifAbsentPut: environmentAt:put: environmentRemoveKey:ifAbsent:)
('changing suspended state' install: restartTopWith:)
('threading' setThreadAffinity:)
!

Process class removeSelector: #shouldNotBeRedefined!

!Process class reorganize!
('instance creation' forContext:priority:)
('documentation' terminateExamples)
!
