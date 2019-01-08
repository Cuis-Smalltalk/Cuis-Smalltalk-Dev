'From Cuis 5.0 of 7 November 2016 [latest update: #3569] on 8 January 2019 at 5:28:27 pm'!

!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 1/8/2019 17:27:55'!
rulesFor: aProcess
	"Answer two flags: allow-stop, and allow-debug"

	"Don't mess with the process running the ProcessBrowser.
	If we support several active UIs, we'd detect the UI process running us"
	self flag: #jmvVer2.
	aProcess == Processor activeProcess
		ifTrue: [^{false. false}].

	aProcess name = 'Sound Player'
		ifTrue: [ ^{false. false}].

	^ [aProcess caseOf: {
		[] -> [{false. false}].
		[Smalltalk lowSpaceWatcherProcess] -> [{false. false}].
		[WeakArray runningFinalizationProcess] -> [{false. false}].
		[Processor activeProcess] -> [{false. true}].
		[Processor backgroundProcess] -> [{false. false}].
		[Sensor interruptWatcherProcess] -> [{false. false}].
		[Sensor eventTicklerProcess] -> [{false. false}].
		[CPUWatcher currentWatcherProcess] -> [{false. false}].
		[Delay timerProcess] -> [{false. false}]}
		otherwise: [ {true. true}]]
		ifError: [ :err :rcvr | {true. true}]! !

