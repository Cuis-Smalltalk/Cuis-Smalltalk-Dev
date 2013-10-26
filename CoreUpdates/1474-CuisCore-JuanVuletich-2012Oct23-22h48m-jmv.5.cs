'From Cuis 4.0 of 21 April 2012 [latest update: #1473] on 23 October 2012 at 11:08:06 pm'!

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 10/23/2012 23:02'!
spawnNewMorphicProcessFor: aWorld

	UIProcess ifNotNil: [ UIProcess animatedUI: nil ].
	UIProcess _ [
		aWorld clearWaitDelay.
		[ aWorld doOneCycle. Processor yield. true] whileTrue: [].
	] newProcess priority: Processor userSchedulingPriority.
	UIProcess
		name: 'Morphic UI';
		animatedUI: aWorld;
		resume! !


!Debugger methodsFor: 'private' stamp: 'jmv 10/21/2012 21:04'!
resumeProcess
	savedCursor
		ifNotNil: [Sensor currentCursor: savedCursor].
	interruptedProcess isTerminated ifFalse: [
		interruptedProcess resume ].
	"if old process was terminated, just terminate current one"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	self currentWorld ifNotNil: [ :w | w displayWorld ].
	Smalltalk installLowSpaceWatcher.
	"restart low space handler"
	errorWasInUIProcess == false
		ifFalse: [ Processor terminateActive ]! !


!Process methodsFor: 'UI support' stamp: 'jmv 10/23/2012 23:01'!
animatedUI: anUIRoot
	"Let us know that we are running a certain UI.
	In Morphic, anUIRoot should be the World being run.
	We use the event system to avoid the need to add an ivar to us."

	self removeActionsForEvent: #animatedUI.
	anUIRoot ifNotNil: [
		self when: #animatedUI send: #yourself to: anUIRoot ]! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 10/21/2012 20:10'!
isUIProcess: aProcess
	^aProcess animatedUI notNil! !

!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 10/23/2012 17:21'!
rulesFor: aProcess
	"Answer two flags: allow-stop, and allow-debug"

	"Don't mess with the process running the ProcessBrowser.
	If we support several active UIs, we'd detect the UI process running us"
	self flag: #jmvVer2.
	aProcess == Processor activeProcess
		ifTrue: [^{false. false}].

	^ [aProcess caseOf: {
		[] -> [{false. false}].
		[Smalltalk lowSpaceWatcherProcess] -> [{false. false}].
		[WeakArray runningFinalizationProcess] -> [{false. false}].
		[Processor activeProcess] -> [{false. true}].
		[Processor backgroundProcess] -> [{false. false}].
		[Sensor interruptWatcherProcess] -> [{false. false}].
		[Sensor eventTicklerProcess] -> [{false. false}].
		[SoundPlayer playerProcess] -> [{false. false}].
		[CPUWatcher currentWatcherProcess] -> [{false. false}].
		[Delay timerProcess] -> [{false. false}]}
		otherwise: [ {true. true}]]
		ifError: [ :err :rcvr | {true. true}]! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 10/21/2012 20:59'!
interruptNameX: labelString
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess  |
	preemptedProcess _ Processor preemptedProcess.
	"Don't stop the background process. This crashes the VM!!"
	preemptedProcess == Processor backgroundProcess
		ifTrue: [ ^self ].
	preemptedProcess suspend.
	Debugger
		openInterrupt: labelString, 
			' - Process: ', preemptedProcess name, 
			' - Priority: ', preemptedProcess priority printString 
		onProcess: preemptedProcess! !

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 10/21/2012 20:21'!
spawnNewProcessIfThisIsUIX: suspendedProcess

	suspendedProcess animatedUI ifNotNil: [ :aWorld |
		self spawnNewMorphicProcessFor: aWorld.
		^true
	].
	^false		"no new process was created"! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 10/21/2012 20:28'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| stream tally |
	
	"VM statistics (Memory use and GC, mainly)"
	stream _ FileStream forceNewFileNamed: 'MemoryStats.txt'.
	[ stream nextPutAll: Utilities vmStatisticsReportString ] 
		ensure: [ stream close ].
	
	"Process list"
	stream _ FileStream forceNewFileNamed: 'ProcessList.txt'.
	[
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]
	] ensure: [ stream close ].

"Fork all these, so they run in sequence, as the system is back running"
[
	
	"Process taking most CPU"
	stream _ FileStream forceNewFileNamed: 'ThePig.txt'.
	ProcessBrowser dumpPigStackOn: stream andClose: true.
	
	"Tally of all processes"
	stream _ FileStream forceNewFileNamed: 'FullTally.txt'.
	[
		tally _ MessageTally new.
		tally reportOtherProcesses: true.	"actually irrelevant"
		tally spyAllEvery: 1 on: [ (Delay forMilliseconds: 1000) wait ].
		tally report: stream ] ensure: [ stream close ].

	"Memory Analysis"
	stream _ FileStream forceNewFileNamed: 'MemoryAnalysis.txt'.
	[ SpaceTally new printSpaceAnalysis: 1 on: stream ]
		ensure: [ stream close ]

] forkNamed: 'CPU usage analysis'! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 10/21/2012 20:01'!
canvas: aFormCanvas
	canvas _ aFormCanvas.
	aFormCanvas ifNotNil: [
		aFormCanvas into: world ].
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new]
		ifNotNil: [ damageRecorder doFullRepaint]! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 10/21/2012 20:03'!
doDeferredUpdatingFor: aWorld
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, do nothing and answer false. One can set the class variable DisableDeferredUpdates to true to completely disable the deferred updating feature."
	| properDisplay |
	PasteUpMorph disableDeferredUpdates ifTrue: [^ false].
	(Display deferUpdates: true) ifNil: [^ false].  "deferred updates not supported"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	self flag: #jmvVer.
	true "aWorld == World" ifTrue: [  "this world fills the entire Display"
		properDisplay ifFalse: [
			aWorld viewBox: Display boundingBox.    "do first since it may clear canvas"
			self canvas: (Display getCanvas copyClipRect: Display boundingBox).
		]
	] ifFalse: [  "this world is inside an MVC window"
		self revisar.
		self halt.
	].
	^ true! !

!methodRemoval: ProjectX class #resumeProcessX:!
ProjectX class removeSelector: #resumeProcessX:!
!methodRemoval: ProjectX class #spawnNewProcessFor:!
ProjectX class removeSelector: #spawnNewProcessFor:!
!methodRemoval: ProjectX class #spawnNewProcessX!
ProjectX class removeSelector: #spawnNewProcessX!
!methodRemoval: ProjectX class #uiProcessX!
ProjectX class removeSelector: #uiProcessX!
!methodRemoval: HandMorph #interrupted!
HandMorph removeSelector: #interrupted!
