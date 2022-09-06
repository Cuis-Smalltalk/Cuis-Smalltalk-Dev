'From Cuis 6.0 [latest update: #5469] on 6 September 2022 at 9:41:55 am'!

!BlockClosure methodsFor: 'private' stamp: 'jmv 9/6/2022 09:41:25'!
grabProcessorFor: milliseconds onTimeout: timeoutBlock
	"Evaluate the receiver (block), without the possibility of preemption by regular priority processes.
	If not finished after milliseconds, evaluate timeoutBlock and restore original priority.
	Use with care!!"
	"Based on #valueUnpreemptively"
	
	| activeProcess oldPriority result done |
	activeProcess _ Processor activeProcess.
	oldPriority _ activeProcess priority.
	done _ false.
	
	activeProcess priority: Processor highIOPriority + Processor lowIOPriority // 2.
	milliseconds ifNotNil: [
		[
			(Delay forMilliseconds: milliseconds) wait.
			done ifFalse: [
				activeProcess
					suspend;
					priority: oldPriority.
				timeoutBlock value.
				"The timeoutBlock might terminate it, or even resume it!!"
				activeProcess isSuspended ifTrue: [
					activeProcess resume ]]
		] forkAt: Processor highIOPriority named: '#grabProcessorFor:onTimeout:' ].

	result _ self ensure: [
		done _ true.
		activeProcess priority: oldPriority].
	
	"Yield after restoring priority to give the preempted processes a chance to run"
	Processor yield.
	^result! !

