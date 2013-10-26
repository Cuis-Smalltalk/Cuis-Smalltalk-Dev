'From Cuis 4.0 of 21 April 2012 [latest update: #1464] on 7 October 2012 at 1:29:29 pm'!

!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 10/6/2012 17:04'!
nameAndRulesFor: aProcess 
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	^ [aProcess caseOf: {
		[] -> [{'no process'. false. false}].
		[Smalltalk lowSpaceWatcherProcess] -> [{'the low space watcher'. false. false}].
		[WeakArray runningFinalizationProcess] -> [{'the WeakArray finalization process'. false. false}].
		[Processor activeProcess] -> [{'the UI process'. false. true}].
		[Processor backgroundProcess] -> [{'the idle process'. false. false}].
		[Sensor interruptWatcherProcess] -> [{'the user interrupt watcher'. false. false}].
		[Sensor eventTicklerProcess] -> [{'the event tickler'. false. false}].
		[ProjectX uiProcessX] -> [{'the inactive Morphic UI process'. false. false}].
		[SoundPlayer playerProcess] -> [{'the Sound Player'. false. false}].
		[CPUWatcher currentWatcherProcess] -> [{'the CPUWatcher'. false. false}]}
		otherwise: [
			(aProcess priority = Processor timingPriority
					and: [aProcess suspendedContext receiver == Delay])
				ifTrue: [{'the timer interrupt watcher'. false. false}]
				ifFalse: [{aProcess suspendedContext asString. true. true}]]]
		ifError: [ :err :rcvr | {aProcess suspendedContext asString. true. true}]! !

!methodRemoval: ProcessorScheduler #tallyCPUUsageFor:!
ProcessorScheduler removeSelector: #tallyCPUUsageFor:!
!methodRemoval: ProcessBrowser class #tallyCPUUsageFor:!
ProcessBrowser class removeSelector: #tallyCPUUsageFor:!
