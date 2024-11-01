'From Cuis7.1 [latest update: #6810] on 1 November 2024 at 12:55:18 pm'!
!classDefinition: #Process category: #'Kernel-Processes'!
Link subclass: #Process
	instanceVariableNames: 'suspendedContext priority myList threadId name env '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Processes'!

!Process commentStamp: 'eem 11/1/2024 12:47:28' prior: 0!
I represent an independent path of control in the system. This path of control may be stopped (by sending the message suspend) in such a way that it can later be restarted (by sending the message resume). When any one of several paths of control can be advanced, the single instance of ProcessorScheduler named Processor determines which one will actually be advanced, partly using the value of priority.

Instance Variables: N.B. The first four are used by the virtual machine. They must be defined, and defined in this order,
	suspendedContext:					<Context|nil>
	priority:					<Integer>
	myList:					<LinkedList|nil>
	threadId:					<Integer|nil>
	name:					<String|nil>
	env:					<Dictionary|nil>

suspendedContext
	- if nil, then I am either the active process or I have been terminated.  If not nil it is the Context at the hot end of my stack.

priority
	- my scheduling priority, which corresponds to the index in ProcessorScheduler quiescentProcesses that holds the LinkedList of processes at this priority.  If I am running but quiescent, changing priority involves changing priority and myList.

myList
	- if nil, I am either suspended or the active process.  If not nil it is either some condition variable (Semaphore, Mutex) that I am blocked waiting on, or it is the LinkedList in ProcessorScheduler quiescentProcesses that holds the processes for my priority that are not the current active process but still runnable.

threadId (threadAffinity in Squeak)
	- if not nil then it is an integer value set by the VM that controls how I interact with underlying native threads. If positive, the threadId is that of some native thread known to the VM, and the VM will ensure that when I am running, I will be running on that native thread.  If negative, the absolute value of threadId is that of some thread known to the VM, and the VM will ensure that when I am running, I will *not* be running on that native thread.  Set by the VM.  Not to be set manually.  Meaningful only on threaded VMs (which do not mean VMs with a threaded heartbeat).  It is required to be my fourth instance variable. See SmalltalkImage >> #processHasThreadAffinityInstVar: for further information.

name
	- if not nil this is my name, used for information, see e.g. the names of processes displayed in the ProcessBrowser

env
	- if not nil this is a Dictionary providing process-specific variables.  See e.g. environmentAt:*, DynamicVariable and ProcessSpecificVariable.!


!Process methodsFor: 'process specific' stamp: 'ul 8/16/2011 11:24'!
environmentAt: key  ifAbsent: aBlock
	
	^(env ifNil: [ ^aBlock value ]) at: key ifAbsent: aBlock.! !

!Process methodsFor: 'process specific' stamp: 'ul 9/2/2012 20:03'!
environmentAt: key ifAbsentPut: aBlock
	
	^(env ifNil: [ env := Dictionary new ]) at: key ifAbsentPut: aBlock.! !

!Process methodsFor: 'process specific' stamp: 'ul 8/16/2011 11:24'!
environmentAt: key put: value
	
	^(env ifNil: [ env := Dictionary new ]) at: key put: value.! !

!Process methodsFor: 'process specific' stamp: 'ul 8/16/2011 11:25'!
environmentRemoveKey: key ifAbsent: errorBlock
	
	^(env ifNil: [ ^errorBlock value ]) removeKey: key ifAbsent: errorBlock! !

!classDefinition: #Process category: #'Kernel-Processes'!
Link subclass: #Process
	instanceVariableNames: 'suspendedContext priority myList threadId name env'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Processes'!
