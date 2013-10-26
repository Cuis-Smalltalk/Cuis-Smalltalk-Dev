'From Cuis 4.0 of 21 April 2012 [latest update: #1463] on 3 October 2012 at 2:51:23 pm'!

!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'jmv 10/3/2012 11:21'!
monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	self stopMonitoring.

	watcher _ [ [ | promise |
		promise _ Processor tallyCPUUsageFor: secs every: msecs.
		tally _ promise value.
		promise _ nil.
		aBoolean ifTrue: [ self findThePig ].
	] repeat ] newProcess.
	watcher priority: Processor highestPriority.
	watcher resume.
	Processor yield ! !


!CPUWatcher class methodsFor: 'as yet unclassified' stamp: 'jmv 10/3/2012 11:21'!
startMonitoringPeriod: pd rate: rt threshold: th suspendPorcine: aBoolean
	"CPUWatcher startMonitoring"

	CurrentCPUWatcher ifNotNil: [ ^CurrentCPUWatcher startMonitoring. ].
	CurrentCPUWatcher _ (self new)
		monitorProcessPeriod: pd sampleRate: rt suspendPorcine: aBoolean;
		threshold: th;
		yourself.
	^CurrentCPUWatcher
! !


!ProcessBrowserWindow methodsFor: 'open/close' stamp: 'jmv 10/3/2012 11:24'!
openInWorld
	
	super openInWorld.
	self startAutoUpdate.
	self startCPUWatcher! !


!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'jmv 10/3/2012 11:21'!
startMonitoring
	self
		monitorProcessPeriod: 20 sampleRate: 100 suspendPorcine: true! !


!CPUWatcher class methodsFor: 'as yet unclassified' stamp: 'jmv 10/3/2012 11:22'!
startMonitoring
	"CPUWatcher startMonitoring"

	^self startMonitoringPeriod: 20 rate: 100 threshold: 0.8 suspendPorcine: true! !


!ProcessBrowser methodsFor: 'initialize-release' stamp: 'jmv 10/3/2012 11:22'!
startCPUWatcher
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring ifFalse: [
		CPUWatcher startMonitoringPeriod: 2 rate: 100 threshold: 0.85 suspendPorcine: false.
		^true
	].
	^false
! !

!ProcessBrowser methodsFor: 'initialize-release' stamp: 'jmv 10/3/2012 11:12'!
stopCPUWatcher

	CPUWatcher stopMonitoring.
	self updateProcessList! !

!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 10/3/2012 11:10'!
processNameList
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally _ CPUWatcher current ifNotNil: [ :pw | pw tally ].
	^ (processList asOrderedCollection
		copyWithout: nil)
		collect: [:each | | percent |
			percent _ tally
				ifNotNil: [ ((((tally occurrencesOf: each) * 100.0 / tally size) roundTo: 1)
						asString padded: #left to: 2 with: $ ), '% '  ]
				ifNil: [ '' ].
			percent, (self prettyNameForProcess: each)
		] ! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 10/3/2012 11:12'!
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
		[Smalltalk
			at: #SoundPlayer
			ifPresent: [:sp | sp playerProcess]] -> [{'the Sound Player'. false. false}].
		[CPUWatcher currentWatcherProcess] -> [{'the CPUWatcher'. false. false}]}
		 otherwise: 
			[(aProcess priority = Processor timingPriority
					and: [aProcess suspendedContext receiver == Delay])
				ifTrue: [{'the timer interrupt watcher'. false. false}]
				ifFalse: [{aProcess suspendedContext asString. true. true}]]]
		ifError: [:err :rcvr | {aProcess suspendedContext asString. true. true}]! !


!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 10/3/2012 11:13'!
processListMenu
	| menu |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			| nameAndRules | 
			nameAndRules _ model nameAndRulesForSelectedProcess.
			menu addList: #(
				('inspect (i)'					#inspectProcess)
				('explore (I)'				#exploreProcess)
				('inspect Pointers (P)'		#inspectPointers)).
			nameAndRules second
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess]].
			nameAndRules third
				ifTrue: [
					menu addList: #(
						('change priority (p)'		 	#changePriority)
						('debug (d)'						#debugProcess))].
			menu addList: #(('profile messages (m)'	#messageTally)).
			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [menu add: 'signal Semaphore (S)' target: model action: #signalSemaphore].
			menu add: 'full stack (k)' target: model action: #moreStack.
			menu addLine].

	menu addList: #(
		('find context... (f)'		#findContext)
		('find again (g)'			#nextContext		''		model)).
	menu addLine.

	menu
		add: (isStepping
				ifTrue: ['turn off auto-update (a)']
				ifFalse: ['turn on auto-update (a)'])
		action: #toggleAutoUpdate.
	menu add: 'update list (u)' target: model action: #updateProcessList.

	menu addLine.
	CPUWatcher isMonitoring
			ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher ]
			ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher  ].

	^ menu! !

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 10/3/2012 11:17'!
startCPUWatcher
	model startCPUWatcher ifTrue: [
		self setUpdateCallbackAfter: 3 ]! !

!methodRemoval: CPUWatcher class #startMonitoringPeriod:rate:threshold:!
CPUWatcher class removeSelector: #startMonitoringPeriod:rate:threshold:!
!methodRemoval: CPUWatcher #monitorProcessPeriod:sampleRate:!
CPUWatcher removeSelector: #monitorProcessPeriod:sampleRate:!
