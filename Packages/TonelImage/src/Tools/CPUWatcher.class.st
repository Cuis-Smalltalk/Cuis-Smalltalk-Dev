"
CPUWatcher implements a simple runaway process monitoring tool
that will suspend a process that is taking up too much of Squeak's
time and allow user interaction. By default it watches for a Process that
is taking more than 80% of the time; this threshold can be changed.

CPUWatcher can also be used to show cpu percentages for each process 
from within the ProcessBrowser.

	CPUWatcher startMonitoring.	"process period 20 seconds, sample rate 100 msec, threshold 80%"
	CPUWatcher startMonitoringPeriod: 10 rate: 20 threshold: 0.8 suspendPorcine: true
	CPUWatcher current threshold: 0.5.	"change from 80% to 50%"
	CPUWatcher stopMonitoring.

"
Class {
	#name : 'CPUWatcher',
	#superclass : 'ActiveModel',
	#instVars : [
		'tally',
		'watcher',
		'threshold'
	],
	#classVars : [
		'CurrentCPUWatcher'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'as yet unclassified' }
CPUWatcher class >> current [
	^CurrentCPUWatcher

]

{ #category : 'as yet unclassified' }
CPUWatcher class >> currentWatcherProcess [
	^CurrentCPUWatcher ifNotNil: [ CurrentCPUWatcher watcherProcess ]

]

{ #category : 'as yet unclassified' }
CPUWatcher class >> dumpTallyOnTranscript [
	self current ifNotNil: [
		ProcessBrowser dumpTallyOnTranscript: self current tally
	]
]

{ #category : 'as yet unclassified' }
CPUWatcher class >> isMonitoring [

	^CurrentCPUWatcher notNil and: [ CurrentCPUWatcher isMonitoring ]

]

{ #category : 'as yet unclassified' }
CPUWatcher class >> shutDown [
	self stopMonitoring.
]

{ #category : 'as yet unclassified' }
CPUWatcher class >> startMonitoring [
	"CPUWatcher startMonitoring"

	^self startMonitoringPeriod: 20 rate: 100 threshold: 0.8 suspendPorcine: true.
]

{ #category : 'as yet unclassified' }
CPUWatcher class >> startMonitoringPeriod: pd rate: rt threshold: th suspendPorcine: aBoolean [
	"CPUWatcher startMonitoring"

	CurrentCPUWatcher ifNil: [
		CurrentCPUWatcher := self new.
		CurrentCPUWatcher
			threshold: th;
			monitorProcessPeriod: pd sampleRate: rt suspendPorcine: aBoolean ]
]

{ #category : 'as yet unclassified' }
CPUWatcher class >> stopMonitoring [
	"CPUWatcher stopMonitoring"

	CurrentCPUWatcher ifNotNil: [ CurrentCPUWatcher stopMonitoring. ].
	CurrentCPUWatcher := nil.

]

{ #category : 'porcine capture' }
CPUWatcher >> catchThePig: aProcess [
	| rule |
	"nickname, allow-stop, allow-debug"
	rule := (ProcessBrowser rulesFor: aProcess) first.

	(UISupervisor isUIProcess: aProcess)
		ifTrue: [ "aProcess debugWithTitle: 'Interrupted from the CPUWatcher'." ]
		ifFalse: [
			rule ifFalse: [ ^self ].
			ProcessBrowser suspendProcess: aProcess.
			self openWindowForSuspendedProcess: aProcess ]
]

{ #category : 'process operations' }
CPUWatcher >> debugProcess: aProcess [
	| uiPriority oldPriority |
	uiPriority := Processor activeProcess priority.
	aProcess priority >= uiPriority ifTrue: [
		oldPriority := ProcessBrowser setProcess: aProcess toPriority: uiPriority - 1
	].
	ProcessBrowser debugProcess: aProcess.
]

{ #category : 'process operations' }
CPUWatcher >> debugProcess: aProcess fromMenu: aMenuMorph [
	aMenuMorph delete.
	self debugProcess: aProcess.
]

{ #category : 'porcine capture' }
CPUWatcher >> findThePig [
	"tally has been updated. Look at it to see if there is a bad process.
	This runs at a very high priority, so make it fast"
	| countAndProcess | 
	countAndProcess := tally sortedCounts first.
	(countAndProcess key / tally size > threshold) ifTrue: [ | proc |
		proc := countAndProcess value.
		proc == Processor backgroundProcess ifTrue: [ ^self ].	"idle process? OK"
		self catchThePig: proc
	].

]

{ #category : 'accessing' }
CPUWatcher >> isMonitoring [
	^watcher notNil
]

{ #category : 'startup-shutdown' }
CPUWatcher >> monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean [
	| delay |
	self stopMonitoring.
	watcher := [
		delay := Delay forMilliseconds: msecs truncated.
		[
			| thisTally |
			thisTally := IdentityBag new.
			secs * 1000 // msecs timesRepeat: [
				"Could happen if process is halted and resumed."
				delay beingWaitedOn ifTrue: [ delay := Delay forMilliseconds: delay delayDuration ].
				delay wait.
				thisTally add: Processor nextReadyProcess ].
			tally := thisTally.
			aBoolean ifTrue: [ self findThePig ]
		] repeat
	] newProcess.
	watcher
		priority: Processor highestPriority;
		name: 'CPUWatcher monitor';
		resume.
	Processor yield.
]

{ #category : 'porcine capture' }
CPUWatcher >> openMorphicWindowForSuspendedProcess: aProcess [
	| menu rule |
	menu := MenuMorph new.
	"nickname  allow-stop  allow-debug"
	rule := (ProcessBrowser rulesFor: aProcess) second.
	menu add: 'Dismiss this menu' target: menu action: #delete; addLine.
	menu add: 'Open Process Browser' target: ProcessBrowser action: #open.
	menu add: 'Resume'
		target: self
		action: #resumeProcess:fromMenu:
		argumentList: { aProcess . menu }.
	menu add: 'Terminate'
		target: self
		action: #terminateProcess:fromMenu:
		argumentList: { aProcess . menu }.
	rule ifTrue: [
		menu add: 'Debug at a lower priority'
			target: self
			action: #debugProcess:fromMenu:
			argumentList: { aProcess . menu }.
	].
	menu addTitle: aProcess identityHash printString,
		' ', aProcess name,
		' is taking too much time and has been suspended.
What do you want to do with it?'.
	menu stayUp.
	menu popUpInWorld
]

{ #category : 'porcine capture' }
CPUWatcher >> openWindowForSuspendedProcess: aProcess  [

	UISupervisor whenUIinSafeState: [self openMorphicWindowForSuspendedProcess: aProcess]
]

{ #category : 'process operations' }
CPUWatcher >> resumeProcess: aProcess fromMenu: aMenuMorph [
	aMenuMorph delete.
	ProcessBrowser resumeProcess: aProcess.
]

{ #category : 'startup-shutdown' }
CPUWatcher >> stopMonitoring [
	watcher ifNotNil: [
		ProcessBrowser terminateProcess: watcher.
		watcher := nil.
	]
]

{ #category : 'accessing' }
CPUWatcher >> tally [
	^tally
]

{ #category : 'process operations' }
CPUWatcher >> terminateProcess: aProcess fromMenu: aMenuMorph [
	aMenuMorph delete.
	ProcessBrowser terminateProcess: aProcess.
]

{ #category : 'accessing' }
CPUWatcher >> threshold: thresh [
	"What fraction of the time can a process be the active process before we stop it?"
	threshold := (thresh max: 0.02) min: 1.0
]

{ #category : 'accessing' }
CPUWatcher >> watcherProcess [
	^watcher
]
