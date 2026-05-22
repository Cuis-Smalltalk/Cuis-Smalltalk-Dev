"
I provide synchronized communication of a single bit of information (a "signal") between Processes. A signal is sent by sending the message signal and received by sending the message wait. If no signal has been sent when a wait message is sent, the sending Process will be suspended until a signal is sent.

Beware that if a process calls 'aSemaphore critical: []' while already in a critical section for that semaphore, it will enter a deadlock. In some cases, a Mutex can be used instead. Refer to the Mutex class comment.

More detail on the implementation as provided by Eliot Miranda:

A semaphore is a queue (implemented as a linked list) and an excess signals count, which is a non-negative integer.  On instance creation a new semaphore is empty and has a zero excess signals count.  A semaphore created for mutual exclusion is empty and has an excess signals count of one.

When a process waits on a semaphore, if the semaphore's excess signals count is non-zero, then the excess signal count is decremented, and the process proceeds.  But if the semaphore has a zero excess signals count then the process is unscheduled and added to the end of the semaphore, after any other processes that are queued on the semaphore.

When a semaphore is signaled, if it is not empty, the first process is removed from it and added to the runnable processes in the scheduler. If the semaphore is empty its excess signals count is incremented.
"
Class {
	#name : 'Semaphore',
	#superclass : 'LinkedList',
	#instVars : [
		'excessSignals'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'instance creation' }
Semaphore class >> forMutualExclusion [
	"Answer a new instance of me that contains a single signal.  
	This new instance can now be used for mutual exclusion (see the 
	critical: message to Semaphore)."

	^self new signal
]

{ #category : 'instance creation' }
Semaphore class >> new [
	"Answer a new instance of Semaphore that contains no signals."

	^self basicNew initSignals
]

{ #category : 'comparing' }
Semaphore >> = anObject [
	^ self == anObject
]

{ #category : 'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock [
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in
	the process of running the critical: message. If the receiver is, evaluate
	mutuallyExcludedBlock after the other critical: message is finished."
	
	| caught |
	"We need to catch eventual interruptions very carefully. 
	The naive approach of just doing, e.g.,:
		self wait.
		aBlock ensure:[self signal].
	will fail if the active process gets terminated while in the wait.
	However, the equally naive:
		[self wait.
		aBlock value] ensure:[self signal].
	will fail too, since the active process may get interrupted while
	entering the ensured block and leave the semaphore signaled twice.
	To avoid both problems we make use of the fact that interrupts only
	occur on sends (or backward jumps) and use an assignment (bytecode)
	right before we go into the wait primitive (which is not a real send and
	therefore not interruptable either)."

	caught := false.
	^[
		caught := true.
		self wait.
		mutuallyExcludedBlock value
	] ensure: [ caught ifTrue: [self signal] ]

]

{ #category : 'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock ifCurtailed: terminationBlock [
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	^self critical: [ mutuallyExcludedBlock ifCurtailed: terminationBlock ]

]

{ #category : 'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock ifError: errorBlock [
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	| blockValue hasError errMsg errRcvr |
	hasError := false.
	blockValue := self critical:[
		mutuallyExcludedBlock ifError: [ :msg :rcvr |
			hasError := true.
			errMsg := msg.
			errRcvr := rcvr
		].
	].
	hasError ifTrue:[ ^errorBlock value: errMsg value: errRcvr].
	^blockValue
]

{ #category : 'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock ifLocked: alternativeBlock [
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, then evaluate 
	alternativeBlock and return."
	"See the comment of #critical: for the explanation how this pattern works
	before changing the code."

	| caught |
	caught := false.
	^[
		"Note: The following is tricky and depends on the fact that the VM will not switch between processes while executing byte codes (process switches happen only in real sends). The following test is written carefully so that it will result in bytecodes only.
	Do not change the following #== for #=, as #== is not a real message send, just a bytecode."
		excessSignals == 0
			ifTrue: [
				"If we come here, then the semaphore was locked when the test executed. 
				Evaluate the alternative block and answer its result."
				alternativeBlock value ]
			ifFalse: [
				excessSignals := excessSignals - 1.
				caught := true.
				mutuallyExcludedBlock value ] ]
		ensure: [ caught ifTrue: [ self signal ] ]
]

{ #category : 'comparing' }
Semaphore >> hash [
	^ self identityHash
]

{ #category : 'initialize-release' }
Semaphore >> initSignals [
	"Consume any excess signals the receiver may have accumulated."

	excessSignals := 0
]

{ #category : 'testing' }
Semaphore >> isSignaled [
	"Return true if this semaphore is currently signaled"
	^excessSignals > 0
]

{ #category : 'printing' }
Semaphore >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: ' with ';
		nextPutAll: excessSignals printString;
		space;
		nextPutAll: ('signal' pluralize: excessSignals ~= 1)
]

{ #category : 'communication' }
Semaphore >> signal [
	"Send a signal through the receiver. If one or more processes have been 
	suspended trying to receive a signal, allow the first one to proceed. If no 
	process is waiting, remember the excess signal. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals := excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"
]

{ #category : 'private' }
Semaphore >> species [
	"So we are never equal an Array"

	^ self class
]

{ #category : 'initialization' }
Semaphore >> terminateProcess [
	"Terminate the process waiting on this semaphore, if any."

	self isEmpty ifFalse: [ self removeFirst terminate ].
]

{ #category : 'communication' }
Semaphore >> wait [
	"The active Process must receive a signal through the receiver before 
	proceeding.  If no signal has been sent, the active Process will be suspended
	until one is sent.  Essential.  See  
	Object whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0  
		ifTrue: [excessSignals := excessSignals-1]  
		ifFalse: [self addLastLink: Processor activeProcess suspend]"
]

{ #category : 'communication' }
Semaphore >> waitTimeoutMSecs: anInteger [
	"Wait on this semaphore for up to the given number of milliseconds, then timeout. 
	Return true if the deadline expired, false otherwise."
	| d |
	d := DelayWaitTimeout new setDelay: (anInteger max: 0) forSemaphore: self.
	^d wait
]

{ #category : 'communication' }
Semaphore >> waitTimeoutSeconds: anInteger [
	"Wait on this semaphore for up to the given number of seconds, then timeout.
	Return true if the deadline expired, false otherwise."
	^self waitTimeoutMSecs: anInteger * 1000.

]
