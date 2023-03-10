'From Cuis 6.0 [latest update: #5682] on 10 March 2023 at 4:22:43 pm'!

!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 3/10/2023 16:17:47'!
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
		[WeakArray runningFinalizationProcess] -> [{false. false}].
		[Processor activeProcess] -> [{false. true}].
		[Processor backgroundProcess] -> [{false. false}].
		[Sensor interruptWatcherProcess] -> [{false. false}].
		[Sensor eventTicklerProcess] -> [{false. false}].
		[CPUWatcher currentWatcherProcess] -> [{false. false}].
		[Delay timerProcess] -> [{false. false}]}
		otherwise: [ {true. true}]]
		ifError: [ :err :rcvr | {true. true}]! !


!Morph methodsFor: 'layout' stamp: 'jmv 3/10/2023 16:16:59'!
layoutSubmorphsIfNeeded
	"Return self. Recompute the layout if necessary."

	self isLayoutNeeded ifTrue: [
		self layoutSubmorphs ].! !


!DropEvent methodsFor: 'accessing' stamp: 'jmv 3/10/2023 16:20:24'!
wasHandled: aBool

	wasHandled := aBool! !


!DropFilesEvent methodsFor: 'testing' stamp: 'jmv 3/10/2023 16:20:54'!
wasHandled: aBool

	wasHandled := aBool! !


!UserInputEvent methodsFor: 'accessing' stamp: 'jmv 3/10/2023 16:21:14'!
wasHandled: aBool

	wasHandled := aBool! !

