'From Cuis 5.0 [latest update: #3840] on 21 August 2019 at 12:30:57 pm'!

!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'pb 8/21/2019 12:23:45'!
monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	| delay |
	self stopMonitoring.
	watcher _ [
	delay _ Delay forMilliseconds: msecs truncated.
	[ | thisTally |
	thisTally _ IdentityBag new: 20.
	secs * 1000 // msecs timesRepeat: [
		delay wait.
		thisTally add: Processor nextReadyProcess ].
	tally _ thisTally.
	aBoolean ifTrue: [ self findThePig ]] repeat ] newProcess.
	watcher
		priority: Processor highestPriority;
		name: 'CPUWatcher monitor';
		resume.
	Processor yield.! !


!ProcessBrowser methodsFor: 'initialization' stamp: 'pb 8/21/2019 12:16:40'!
startCPUWatcher
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring ifFalse: [
		CPUWatcher startMonitoringPeriod: 1 rate: 25 threshold: 0.85 suspendPorcine: false.
		^true
	].
	^false
! !

