'From Cuis 4.0 of 21 April 2012 [latest update: #1465] on 8 October 2012 at 11:30:05 pm'!

!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 10/8/2012 23:24'!
findThePig
	"tally has been updated. Look at it to see if there is a bad process.
	This runs at a very high priority, so make it fast"
	| countAndProcess | 
	countAndProcess _ tally sortedCounts first.
	(countAndProcess key / tally size > threshold) ifTrue: [ | proc |
		proc _ countAndProcess value.
		proc == Processor backgroundProcess ifTrue: [ ^self ].	"idle process? OK"
		self catchThePig: proc
	].
! !

!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'jmv 10/8/2012 23:19'!
monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	| thisTally delay |
	self stopMonitoring.
	watcher _ [
		thisTally _ IdentityBag new: 200.
		delay _ Delay forMilliseconds: msecs truncated.
		[
			secs * 1000 // msecs timesRepeat: [
				delay wait.
				thisTally add: Processor nextReadyProcess
			].
			tally _ thisTally copy.
			aBoolean ifTrue: [ self findThePig ].
		] repeat
	] newProcess.
	watcher priority: Processor highestPriority.
	watcher name: 'CPUWatcher monitor'.
	watcher resume.
	Processor yield! !

!CPUWatcher methodsFor: 'accessing' stamp: 'jmv 10/8/2012 23:02'!
tally
	^tally! !


!CPUWatcher class methodsFor: 'as yet unclassified' stamp: 'jmv 10/8/2012 23:27'!
startMonitoringPeriod: pd rate: rt threshold: th suspendPorcine: aBoolean
	"CPUWatcher startMonitoring"

	CurrentCPUWatcher ifNil: [
		CurrentCPUWatcher _ self new.
		CurrentCPUWatcher
			threshold: th;
			monitorProcessPeriod: pd sampleRate: rt suspendPorcine: aBoolean ]! !


!ProcessBrowser methodsFor: 'initialize-release' stamp: 'jmv 10/8/2012 23:28'!
startCPUWatcher
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring ifFalse: [
		CPUWatcher startMonitoringPeriod: 1 rate: 100 threshold: 0.85 suspendPorcine: false.
		^true
	].
	^false
! !

!methodRemoval: ProcessBrowser class #tallyCPUUsageFor:every:!
ProcessBrowser class removeSelector: #tallyCPUUsageFor:every:!
!methodRemoval: CPUWatcher class #startMonitoring!
CPUWatcher class removeSelector: #startMonitoring!
!methodRemoval: CPUWatcher #startMonitoring!
CPUWatcher removeSelector: #startMonitoring!
!methodRemoval: CPUWatcher #threshold!
CPUWatcher removeSelector: #threshold!
