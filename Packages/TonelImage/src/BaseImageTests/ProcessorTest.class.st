Class {
	#name : 'ProcessorTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Processes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Processes'
}

{ #category : 'as yet unclassified' }
ProcessorTest >> testGrabProcessor [
	"
	ProcessorTest new testGrabProcessor
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait delay |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessor
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted at the end of #grabProcessor'
]

{ #category : 'as yet unclassified' }
ProcessorTest >> testGrabProcessorOnlyForNoTimeout [
	"
	ProcessorTest new testGrabProcessorOnlyForNoTimeout
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait delay consistentCount inconsistentCount |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	consistentCount _ 0.
	inconsistentCount _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 1000
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!'.
	].

	self
		assert: (updates = tries)
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: (observations = tries)
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted right after #grabProcessor'.
	self
		assert: consistentCount = tries
		description: 'Should have finished without timeout every time'.
	self
		assert: inconsistentCount = 0
		description: 'Should have never timeout-ed'
]

{ #category : 'as yet unclassified' }
ProcessorTest >> testGrabProcessorOnlyForTimeout [
	"
	1 to: 40 do: [ :i | i print. ProcessorTest new testGrabProcessorOnlyForTimeout ]
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait consistentCount inconsistentCount delay |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	consistentCount _ 0.
	inconsistentCount _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 25 							"About half the time should preempt it before it finishes"
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		"If it needs time to finish, at low priority, allow for it."
		[ consistentState ] whileFalse: [ delay wait ].
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40									"Mean value should be around 25"
		description: 'The low priority process was not preempted after the tiemout'.
	self
		assert: consistentCount > 10    									"Expected value is around 25"
		description: 'It seems the low priority process finished always before the timeout'.
	self
		assert: inconsistentCount > 10   									"Expected value is around 25"
		description: 'It seems the low priority process was not preempted at the timeout'
]

{ #category : 'as yet unclassified' }
ProcessorTest >> testValueUnpreemptively [
	"
	ProcessorTest new testValueUnpreemptively
	"
	| done consistentState tries updates observations semaphore waitStart waited totalWait delay |
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: 50 atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] valueUnpreemptively
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	delay _ Delay forMilliseconds: 5.
	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		delay wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted at the end of #grabProcessor'
]

{ #category : 'private' }
ProcessorTest >> wasteTime: milliseconds [
	| t0 |
	t0 _ Time localMillisecondClock.
	[ t0 + milliseconds > Time localMillisecondClock ] whileTrue: [
		1.23 asString asNumber ]
]
