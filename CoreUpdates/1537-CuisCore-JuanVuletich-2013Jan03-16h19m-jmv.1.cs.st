'From Cuis 4.1 of 12 December 2012 [latest update: #1535] on 3 January 2013 at 4:50:08 pm'!

!TestCase methodsFor: 'Running' stamp: 'jmv 1/3/2013 16:28'!
isLogging
	"Log to Transcript"
	^true! !

!TestCase methodsFor: 'Running' stamp: 'jmv 1/3/2013 16:33'!
logFailure: aString
	| log |
	self isLogging ifTrue: [
		log _ self failureLog.
		log newLine.
		self printOn: log.
		log nextPutAll: ' -- '.
		log nextPutAll: aString ]! !


!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 1/3/2013 16:50'!
testGrabProcessorOnlyForTimeout
	"
	1 to: 40 do: [ :i | i print. ProcessorTest new testGrabProcessorOnlyForTimeout ]
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait consistentCount inconsistentCount |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
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
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 25 							"About half the time should preempt it before it finishes"
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time millisecondClockValue.
		(Delay forMilliseconds: 15) wait.
		waited _ Time millisecondClockValue - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		"If it needs time to finish, at low priority, allow for it."
		[ consistentState ] whileFalse: [ (Delay forMilliseconds: 2) wait ].
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted after the tiemout'.
	self
		assert: consistentCount > 3
		description: 'It seems the low priority process finished always before the timeout'.
	self
		assert: inconsistentCount > 5
		description: 'It seems the low priority process was not preempted at the timeout'! !

