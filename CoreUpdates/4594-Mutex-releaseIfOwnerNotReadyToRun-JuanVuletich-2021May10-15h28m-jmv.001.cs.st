'From Cuis 5.0 [latest update: #4593] on 10 May 2021 at 3:29:06 pm'!

!Mutex methodsFor: 'mutual exclusion' stamp: 'jmv 5/10/2021 15:26:17'!
releaseIfOwnerNotReadyToRun
	"If process owning us is not ready to run, release.
	This means that the critical section might be already running, and suspended or blocked, for example, because of an open Debugger.
	If so, critical section will be reentered for next requester without any wait.
	Use with care!!
	"
	(owner notNil and: [ owner isReady not ])
		ifTrue: [
			owner _ nil.
			semaphore _ Semaphore forMutualExclusion ].! !

