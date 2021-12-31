'From Cuis 5.0 [latest update: #5019] on 30 December 2021 at 6:40:13 pm'!

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 12/30/2021 18:38:55'!
findPigProcess
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process |
	promise _ Processor tallyCPUUsageFor: 0.1 every: 10.
	tally _ promise value.
	tally ifEmpty: [ ^Processor preemptedProcess ]. "We can not do better"
	process _ tally sortedCounts first value.
	"Avoid, if possible, the background process, and terminated processes."
	tally sortedCounts do: [ :entry |
		(process == Processor backgroundProcess or: [ (process name beginsWith: '[system]') or: [process isTerminated]])
			ifTrue: [ process _ entry value ]].
	^process! !


!InputSensor methodsFor: 'user interrupts' stamp: 'jmv 12/30/2021 18:32:52'!
installInterruptWatcher
	"Initialize the interrupt watcher process. Terminate the old process if any.
	This process simply waits for the VM to signal the 'user interrupt' semaphore, and opens a debugger."
	"
	Sensor installInterruptWatcher
	"

	InterruptWatcherProcess ifNotNil: [InterruptWatcherProcess terminate].
	InterruptSemaphore _ (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	InterruptWatcherProcess _ [self userInterruptWatcher] newProcess.
	InterruptWatcherProcess priority: Processor timingPriority-5.
	InterruptWatcherProcess name: '[system] User interrupt watcher'.
	InterruptWatcherProcess resume! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 12/30/2021 18:33:27'!
installLowSpaceWatcher
	"Start a process to watch for low-space conditions."
	"Smalltalk installLowSpaceWatcher"

	self stopLowSpaceWatcher.
	LowSpaceProcess _ [self lowSpaceWatcher] newProcess.
	LowSpaceProcess priority: Processor lowIOPriority + 5.
	LowSpaceProcess name: '[system] Low Space Watcher'.
	LowSpaceProcess resume.

! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor whenUIinSafeState: [
	EventSensor install.
	Smalltalk installLowSpaceWatcher ]!

