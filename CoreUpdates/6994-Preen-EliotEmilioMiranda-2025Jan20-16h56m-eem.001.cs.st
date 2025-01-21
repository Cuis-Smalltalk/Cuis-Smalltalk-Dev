'From Cuis7.3 [latest update: #6992] on 20 January 2025 at 5:28:36 pm'!

!ProcessBrowser class methodsFor: 'process control' stamp: 'eem 1/20/2025 16:58:24'!
rulesFor: aProcess
	"Answer two flags: allow-stop, and allow-debug"

	"Don't mess with the process running the ProcessBrowser.
	If we support several active UIs, we'd detect the UI process running us"
	aProcess == Processor activeProcess
		ifTrue: [^{false. false}].

	aProcess name = 'Sound Player'
		ifTrue: [ ^{false. false}].

	^ [aProcess caseOf: {
		[] -> [{false. false}].
		[Smalltalk lowSpaceWatcherProcess] -> [{false. false}].
		[EphemeronFinalizationProcess runningFinalizationProcess] -> [{false. false}].
		[Processor activeProcess] -> [{false. true}].
		[Processor backgroundProcess] -> [{false. false}].
		[Sensor interruptWatcherProcess] -> [{false. false}].
		[Sensor eventTicklerProcess] -> [{false. false}].
		[CPUWatcher currentWatcherProcess] -> [{false. false}].
		[Delay timerProcess] -> [{false. false}]}
		otherwise: [ {true. true}]]
		ifError: [ :err :rcvr | {true. true}]! !

