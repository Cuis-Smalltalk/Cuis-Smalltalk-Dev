"
A Mutex is a light-weight MUTual EXclusion object being used when two or more processes need to access a shared resource concurrently. A Mutex grants ownership to a single process and will suspend any other process trying to aquire the mutex while in use. Waiting processes are granted access to the mutex in the order the access was requested.

Nested (or recursive) calls to #critical: from a single process are allowed, and the process is not blocked for this: access is granted immediately. For this reason instances of Mutex should be used for resources that support this form of multiple access (i.e. multiple simultaneous access from within a single Process). Resources that don't support this, like access to private state that changes for each call, should use a Semaphore. See Semaphore's class comment

Instance variables:
	semaphore		<Semaphore>		The (primitive) semaphore used for synchronization.
	owner			<Process>			The process owning the mutex.
"
Class {
	#name : 'Mutex',
	#superclass : 'Object',
	#instVars : [
		'semaphore',
		'owner'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'mutual exclusion' }
Mutex >> critical: aBlock [
	"Evaluate aBlock protected by the receiver.
	Provides reentrant mutex behavior: Allow reentry from the same process.
	See https://en.wikipedia.org/wiki/Reentrant_mutex

	| m |
	m := Mutex new.
	m critical: [ 1 print. m critical: [ 2 print ]. 3 print ]. 'Done'.
	"

	^self critical: aBlock onReentry: aBlock
]

{ #category : 'mutual exclusion' }
Mutex >> critical: aBlock ifWouldBlock: unprotectedBlock [

	self wouldBlock ifTrue: [ ^unprotectedBlock value ].
	^self critical: aBlock
]

{ #category : 'mutual exclusion' }
Mutex >> critical: aBlock onReentry: reentryBlock [
	"Evaluate aBlock protected by the receiver.
	This method allows using the receiver either as a reentrant or non-reentrant Mutex.
	See https://en.wikipedia.org/wiki/Reentrant_mutex

	| m |
	m := Mutex new.
	m critical: [ 1 print. m critical: [ 2 print ] onReentry: ['Reentry attempt!' print]. 3 print ]. 'Done'.
	"

	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess == owner ifTrue: [^reentryBlock value].
	^semaphore critical: [
		owner := activeProcess.
		aBlock ensure: [owner := nil]].
]

{ #category : 'mutual exclusion' }
Mutex >> criticalNonReentrant: aBlock [
	"Evaluate aBlock protected by the receiver.
	Provides non-reentrant mutex behavior: Raise an error if attempted to reenter from same process.
	See https://en.wikipedia.org/wiki/Reentrant_mutex

	| m |
	m := Mutex new.
	m criticalNonReentrant: [ 1 print. m criticalNonReentrant: [ 2 print ]. 3 print ]. 'Done'.
	"

	^self critical: aBlock onReentry: [ self error: 'Mutex reentry attempted' ]
]

{ #category : 'initialization' }
Mutex >> initialize [
	semaphore := Semaphore forMutualExclusion.
]

{ #category : 'system simulation' }
Mutex >> primitiveEnterCriticalSectionOnBehalfOf: effectiveProcess [
	"Primitive. Simulate primitiveEnterCriticalSection.  The receiver
	 must be unowned or owned by the effectiveProcess to proceed.
	 Answer if the process is already owned by the current process."
	<primitive: 186>
	^MethodContext primitiveFailTokenFor: nil
]

{ #category : 'system simulation' }
Mutex >> primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effectiveProcess [
	"Primitive. Simulate primitiveEnterCriticalSection.  Attempt to set the ownership
	 of the receiver.  If the receiver is unowned set its owningProcess to the
	 effectiveProcess and answer false.  If the receiver is owned by the effectiveProcess
	 answer true.  If the receiver is owned by some other process answer nil."
	<primitive: 187>
	^MethodContext primitiveFailTokenFor: nil
]

{ #category : 'mutual exclusion' }
Mutex >> releaseIfOwnerNotReadyToRun [
	"If process owning us is not ready to run, release.
	This means that the critical section might be already running, and suspended or blocked, for example, because of an open Debugger.
	If so, critical section will be reentered for next requester without any wait.
	Use with care!
	"
	(owner notNil and: [ owner isReady not ])
		ifTrue: [
			owner := nil.
			semaphore := Semaphore forMutualExclusion ].
]

{ #category : 'testing' }
Mutex >> wouldBlock [
	
	owner isNil ifTrue: [ ^false ].
	owner == Processor activeProcess ifTrue: [ ^false ].
	^true
]
