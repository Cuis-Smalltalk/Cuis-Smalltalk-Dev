"
A SemaphoreTest is sunit test for simple and multiEx semaphores

Instance Variables none; does not have common test fixture accross all tests (because its testing differenct sorts of semaphores (could refactor into muliple testcases if there were more test conditions.

"
Class {
	#name : 'SemaphoreTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Processes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Processes'
}

{ #category : 'private' }
SemaphoreTest >> criticalError [
	Processor activeProcess terminate
]

{ #category : 'testing' }
SemaphoreTest >> testCritical [
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled
]

{ #category : 'testing' }
SemaphoreTest >> testCriticalIfError [
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError ifError:[]]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled
]

{ #category : 'testing' }
SemaphoreTest >> testSemaAfterCriticalWait [
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [s critical: []] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaAfterCriticalWait2 [
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p beenHere |
	s := Semaphore new. beenHere := false.
	p := [s critical: [beenHere := true]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.
	self deny: beenHere.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaAfterCriticalWaitInEnsure [
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [[] ensure: [s critical: []]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaAfterCriticalWaitInEnsure2 [
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p beenHere |
	s := Semaphore new. beenHere := false.
	p := [[] ensure: [s critical: [beenHere := true]]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[ p suspendingList == s ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.
	self assert: beenHere.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaAfterCriticalWaitSuspended [
	"Test whether a semaphore that has just left the wait in Semaphore>>critical: but 	has
	 been suspended before termination, leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [s critical:[]] newProcess.
	p priority: Processor activePriority-1.
	p resume.
	"wait until p entered the critical section"
	[p suspendingList == s] whileFalse: [(Delay forMilliseconds: 10) wait].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	self assert: p suspendingList class == LinkedList.
	p suspend.
	self assert: p suspendingList == nil.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaInCriticalEnsureArgument	 [
	"Test whether a process that is in an ensure argument block but has yet to evaluate the signal
	leaves it with signaling the associated semaphore."
	
	| terminatee sema |
	sema := Semaphore forMutualExclusion.
	terminatee := [sema critical: []] newProcess.
	self assert: terminatee isSuspended.
	[terminatee suspendedContext selectorToSendOrSelf = #signal] whileFalse: [terminatee step].
	self assert: terminatee isSuspended.
	terminatee terminate.
	self assert: terminatee isTerminated. 
	self assert: (sema instVarNamed: #excessSignals) equals: 1.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaInCriticalWait [
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p |
	sentinel := Array with: false.
	s := Semaphore new.
	p := [
		sentinel at: 1 put: true.
		s critical: []] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0
]

{ #category : 'testing' }
SemaphoreTest >> testSemaInCriticalWait2 [
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p beenHere |
	sentinel := Array with: false.
	s := Semaphore new. beenHere := false.
	p := [
		sentinel at: 1 put: true.
		s critical: [beenHere := true]] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0.
	self deny: beenHere.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaInCriticalWaitInEnsure [
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p |
	sentinel := Array with: false.
	s := Semaphore new.
	p := [[] ensure: [
			sentinel at: 1 put: true.
			s critical: []]] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0.
]

{ #category : 'testing' }
SemaphoreTest >> testSemaInCriticalWaitInEnsure2 [
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| sentinel s p beenHere |
	sentinel := Array with: false.
	s := Semaphore new. beenHere := false.
	p := [[] ensure: [
			sentinel at: 1 put: true.
			s critical: [beenHere := true]]] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it.
	Call #yield the number of times required to get where we need."
	[sentinel first] whileFalse: [
		Processor yield ].
	self assert: p suspendingList equals: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0.
	self deny: beenHere.
]

{ #category : 'testing' }
SemaphoreTest >> testSuspendAndResume [
	"Test the semantics of suspending and resuming a process blocked on a semaphore;
	Note the difference between #suspend and #suspendAndUnblock.
	Note: this test will fail when run with older VMs without primitive suspend 578."

	| p |
	p _ [Semaphore new wait] newProcess.
	p resume. Processor yield.
	p suspend; resume.
	"suspend removes p from the semaphore but backs up its pc;
	hence when resumed p reenters the wait state"
	Processor yield.

	"VMs older than 2022, where #processSuspensionUnblocks answers true,
	have this incorrect behavior (primitive 88 instead of the corrected 578)"
	Smalltalk processSuspensionUnblocks
		ifTrue: [self assert: p isTerminated]
		ifFalse: [	
			self assert: p isBlocked.
			p terminate].
	
	p _ [Semaphore new wait] newProcess.
	p resume. Processor yield.
	p suspendAndUnblock; resume.
	"suspendAndUnblock just removes p from the semaphore;
	hence when resumed p simply proceeds"
	Processor yield.
	self assert: p isTerminated
]

{ #category : 'testing' }
SemaphoreTest >> testWaitAndWaitTimeoutTogether [
	| semaphore value waitProcess waitTimeoutProcess |
	semaphore := Semaphore new.
	
	waitProcess := [semaphore wait. value := #wait] newProcess.
	waitProcess resume.

	waitTimeoutProcess := [semaphore waitTimeoutMSecs: 50. value := #waitTimeout] newProcess.
	waitTimeoutProcess resume.

	"Wait for the timeout to happen"
	(Delay forMilliseconds: 100) wait.

	"The waitTimeoutProcess should already have timed out.  This should release the waitProcess"
	semaphore signal.

	[waitProcess isTerminated and: [waitTimeoutProcess isTerminated]]
		whileFalse: [(Delay forMilliseconds: 100) wait].

	self assert: value = #wait.
	
]

{ #category : 'testing' }
SemaphoreTest >> testWaitTimeoutMSecs [
	"Ensure that waitTimeoutMSecs behaves properly"

	"Ensure that a timed out waitTimeoutMSecs: returns true from the wait"
	self assert: (Semaphore new waitTimeoutMSecs: 50) == true.

	"Ensure that a signaled waitTimeoutMSecs: returns false from the wait"
	self assert: (Semaphore new signal waitTimeoutMSecs: 50) == false.

]
