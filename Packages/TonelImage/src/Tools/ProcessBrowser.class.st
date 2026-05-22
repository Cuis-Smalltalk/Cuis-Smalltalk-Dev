"
5/26/2021 14:46:46'
"
Class {
	#name : 'ProcessBrowser',
	#superclass : 'ActiveModel',
	#instVars : [
		'selectedProcess',
		'selectedContext',
		'methodText',
		'processList',
		'stackList',
		'selectedClass',
		'selectedSelector',
		'searchString'
	],
	#classVars : [
		'SuspendedProcesses'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'process control' }
ProcessBrowser class >> debugProcess: aProcess [
"	self resumeProcess: aProcess."
	aProcess debugWithTitle: 'Interrupted from the Process Browser'.

]

{ #category : 'CPU utilization' }
ProcessBrowser class >> dumpPigStackOn: aStream [
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	process := tally sortedCounts first value.
	"Avoid, if possible, the background process, and terminated processes."
	tally sortedCounts do: [ :entry |
		(process == Processor backgroundProcess or: [process isTerminated])
			ifTrue: [ process := entry value ]].
	"UISupervisor whenUIinSafeState: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; newLine.
	self dumpTally: tally on: aStream.
	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringFull: false);
		newLine.
	stack := process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: Debugger defaultDebugStackSize]
		ifFalse: [
			suspendedContext := process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: Debugger defaultDebugStackSize]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; newLine]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream newLine ]].
	^process
]

{ #category : 'CPU utilization' }
ProcessBrowser class >> dumpTally: tally on: aStream [
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	tally sortedCounts do: [ :assoc |
		((assoc key / tally size) * 100.0) rounded printOn: aStream.
		aStream
			nextPutAll: '%   ';
			print: assoc value identityHash; space;
			nextPutAll: assoc value name;
			newLine.
	]
]

{ #category : 'CPU utilization' }
ProcessBrowser class >> dumpTallyOnTranscript: tally [
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	self dumpTally: tally on: Transcript
]

{ #category : 'CPU utilization' }
ProcessBrowser class >> findPigProcess [
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process |
	promise := Processor tallyCPUUsageFor: 0.1 every: 10.
	tally := promise value.
	tally ifEmpty: [ ^nil ]. "We can not do better"
	"Avoid, system processes, the background process, and terminated processes."
	tally sortedCounts do: [ :entry |
		process := entry value.
		(process == Processor backgroundProcess or: [ (process name beginsWith: '[system]') or: [process isTerminated]])
			ifFalse: [ ^process ]].
	^nil
]

{ #category : 'class initialization' }
ProcessBrowser class >> initialize [
	"
	ProcessBrowser initialize.
	SystemDictionary initialize.
	"
	SuspendedProcesses ifNil: [
		SuspendedProcesses := IdentityDictionary new ]
]

{ #category : 'ui' }
ProcessBrowser class >> open [
	"
	ProcessBrowser open
	"
	"Create and schedule a ProcessBrowser."
	Smalltalk garbageCollect.
	^ ProcessBrowserWindow open: self new label: nil
]

{ #category : 'cached state access' }
ProcessBrowser class >> releaseClassCachedState [
	SuspendedProcesses ifNotNil: [
		SuspendedProcesses ifEmpty: [
			SuspendedProcesses := nil ]]
]

{ #category : 'process control' }
ProcessBrowser class >> resumeProcess: aProcess [
	| priority |
	priority := self suspendedProcesses
				removeKey: aProcess
				ifAbsent: [aProcess priority].
	aProcess priority: priority.
	aProcess isSuspended ifTrue: [ aProcess resume ]

]

{ #category : 'process control' }
ProcessBrowser class >> rulesFor: aProcess [
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
		ifError: [ :err :rcvr | {true. true}]
]

{ #category : 'process control' }
ProcessBrowser class >> setProcess: aProcess toPriority: priority [
	| oldPriority |
	oldPriority := self suspendedProcesses at: aProcess ifAbsent: nil.
	oldPriority ifNotNil: [ self suspendedProcesses at: aProcess put: priority ].
	aProcess priority: priority.
	^oldPriority
]

{ #category : 'as yet unclassified' }
ProcessBrowser class >> shutDown [

	"Remove terminated processes from my suspended list"
	SuspendedProcesses ifNotNil: [
		SuspendedProcesses keys do: [ :each |
			each isTerminated ifTrue: [ SuspendedProcesses removeKey: each ]]]
]

{ #category : 'process control' }
ProcessBrowser class >> suspendProcess: aProcess [
	| priority |
	priority := aProcess priority.
	self suspendedProcesses at: aProcess put: priority.
	"Need to take the priority down below the caller's
	so that it can keep control after signaling the Semaphore"
	(aProcess suspendingList isKindOf: Semaphore)
		ifTrue: [aProcess priority: Processor lowestPriority.
			aProcess suspendingList signal].
	[aProcess suspend]
		on: Error
		do: [:ex | self suspendedProcesses removeKey: aProcess].
	aProcess priority: priority.

]

{ #category : 'process control' }
ProcessBrowser class >> suspendedProcesses [
	"Answer a collection of processes that my instances have suspended.  
	This is so that they don't get garbage collected."
	^ SuspendedProcesses
		ifNil: [SuspendedProcesses := IdentityDictionary new]
]

{ #category : 'process control' }
ProcessBrowser class >> terminateProcess: aProcess [
	aProcess ifNotNil: [
		self suspendedProcesses
			removeKey: aProcess
			ifAbsent: nil.
		aProcess terminate
	]
]

{ #category : 'process control' }
ProcessBrowser class >> wasProcessSuspendedByProcessBrowser: aProcess [
	^self suspendedProcesses includesKey: aProcess
]

{ #category : 'stack list' }
ProcessBrowser >> changeStackListTo: aCollection  [

        stackList := aCollection.
        self changed: #stackNameList.
        self context: nil
]

{ #category : 'accessing' }
ProcessBrowser >> context: aContext  [
	selectedContext := aContext.
	selectedClass := nil.
	selectedSelector := nil.
	methodText := nil.
	self changed: #stackListIndex.
	self changed: #selectedMethod
]

{ #category : 'process list' }
ProcessBrowser >> findContext: aString [
	| initialProcess initialContext found |
	searchString := aString.
	searchString isEmpty
		ifTrue: [^ false].
	initialProcess := selectedProcess.
	initialContext := selectedContext.
	self processListIndex: 1.
	self stackListIndex: 1.
	found := self nextContext.
	found ifFalse: [
		self selectedProcess: initialProcess.
		self context: initialContext].
	^ found
]

{ #category : 'initialization' }
ProcessBrowser >> initialize [
	methodText := ''.
	selectedContext := nil.
	searchString := ''.
	self updateProcessList; processListIndex: 1
]

{ #category : 'stack list' }
ProcessBrowser >> moreStack [
	self updateStackList: 2000
]

{ #category : 'process list' }
ProcessBrowser >> nextContext [
	| initialProcessIndex initialStackIndex |

	searchString isEmpty ifTrue: [ ^false ].
	initialProcessIndex := self processListIndex.
	initialStackIndex := self stackListIndex.
	initialProcessIndex
		to: self processList size
		do: [ :pi | 
			self processListIndex: pi.
			self stackNameList withIndexDo: [ :name :si |
				(pi ~= initialProcessIndex or: [si > initialStackIndex])
					ifTrue: [
						(name includesSubString: searchString)
							ifTrue: [
								self stackListIndex: si.
								^true ]]]].
	self processListIndex: initialProcessIndex.
	self stackListIndex: initialStackIndex.
	^false
]

{ #category : 'process list' }
ProcessBrowser >> notify: errorString at: location in: aStream  [
	"A syntax error happened when I was trying to highlight my pc. 
	Raise a signal so that it can be ignored."
	Warning signal: 'syntax error'
]

{ #category : 'stack list' }
ProcessBrowser >> pcRange [
	"Answer the indices in the source code for the method corresponding to  
	the selected context's program counter value."
	(selectedContext isNil or: [methodText isEmptyOrNil])
		ifTrue: [^ 1 to: 0].
	^selectedContext debuggerMap
		rangeForPC: (selectedContext pc ifNotNil: [:pc| pc] ifNil: [selectedContext method endPC])
		contextIsActiveContext: stackList first == selectedContext
]

{ #category : 'accessing' }
ProcessBrowser >> processList [
	^ processList
]

{ #category : 'accessing' }
ProcessBrowser >> processListIndex [
	processList ifNil: [ ^0 ].
	selectedProcess ifNil: [ ^0 ].
	^processList indexOf: selectedProcess
]

{ #category : 'accessing' }
ProcessBrowser >> processListIndex: index  [
	self selectedProcess: 
		(processList at: index ifAbsent: nil)
]

{ #category : 'process list' }
ProcessBrowser >> processNameList [
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally := CPUWatcher current ifNotNil: [ :pw | pw tally ].
	^ (processList asOrderedCollection
		copyWithout: nil)
		collect: [:each | | percent |
			percent := tally
				ifNotNil: [
					(((tally occurrencesOf: each) * 100.0 / tally size) rounded
						printString padded: #left to: 2 with: $ ), '% '  ]
				ifNil: [ '' ].
			percent, (each browserPrintStringFull: false)
		]
]

{ #category : 'process actions' }
ProcessBrowser >> resumeProcess [
	selectedProcess
		ifNil: [^ self].
	self class resumeProcess: selectedProcess.
	self updateProcessList
]

{ #category : 'accessing' }
ProcessBrowser >> searchString [
	^searchString
]

{ #category : 'menus' }
ProcessBrowser >> selectedClass [
	"Answer the class in which the currently selected context's method was  
	found."
	^ selectedClass
		ifNil: [selectedClass := selectedContext receiver
				ifNil: [selectedSelector := selectedContext method selector.
					   selectedContext method methodClass]
				ifNotNil: [selectedContext methodClass]]
]

{ #category : 'stack list' }
ProcessBrowser >> selectedContext [
	^selectedContext
]

{ #category : 'accessing' }
ProcessBrowser >> selectedMethod [
	^ methodText ifNil: [methodText := selectedContext
						ifNil: ['']
						ifNotNil: [| pcRange | 
							methodText := [ selectedContext sourceCode ]
								ifError: [ :err :rcvr | 'error getting method text' ].
							pcRange := self pcRange.
							methodText asText
								addAttribute: TextColor red
								from: pcRange first
								to: pcRange last;
								
								addAttribute: TextEmphasis bold
								from: pcRange first
								to: pcRange last]]
]

{ #category : 'accessing' }
ProcessBrowser >> selectedProcess [
	^selectedProcess
]

{ #category : 'accessing' }
ProcessBrowser >> selectedProcess: aProcess [
	selectedProcess := aProcess.
	self updateStackList.
	self changed: #processListIndex
]

{ #category : 'accessing' }
ProcessBrowser >> selectedSelector [
	"Answer the class in which the currently selected context's method was  
	found."
	^ selectedSelector
		ifNil: [selectedSelector := selectedContext receiver
				ifNil: [selectedClass := selectedContext method methodClass
					   selectedContext method selector]
				ifNotNil: [selectedContext selector]]
]

{ #category : 'process actions' }
ProcessBrowser >> signalSemaphore [
	(selectedProcess suspendingList isKindOf: Semaphore)
		ifFalse: [^ self].
	[selectedProcess suspendingList signal] fork.
	(Delay forMilliseconds: 300) wait.
	"Hate to make the UI wait, but it's convenient..."
	self updateProcessList
]

{ #category : 'accessing' }
ProcessBrowser >> stackList [
	^ stackList
]

{ #category : 'accessing' }
ProcessBrowser >> stackListIndex [
	stackList ifNil: [ ^0 ].
	selectedContext ifNil: [ ^0 ].
	^stackList indexOf: selectedContext
]

{ #category : 'accessing' }
ProcessBrowser >> stackListIndex: index  [
	self context: (
		(stackList isNil or: [index = 0]) ifFalse: [
			stackList at: index ifAbsent: nil ])
]

{ #category : 'stack list' }
ProcessBrowser >> stackNameList [
	^ stackList
		ifNil: [#()]
		ifNotNil: [stackList
				collect: [:each | each asString]]
]

{ #category : 'initialization' }
ProcessBrowser >> startCPUWatcher [
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring ifFalse: [
		CPUWatcher startMonitoringPeriod: 1 rate: 25 threshold: 0.85 suspendPorcine: false.
		^true
	].
	^false

]

{ #category : 'initialization' }
ProcessBrowser >> startStackSizeWatcher [

	StackSizeWatcher isWatching ifFalse: [
		StackSizeWatcher startWatchingWithDefaults ]
]

{ #category : 'stepping' }
ProcessBrowser >> stepAt: millisecondSinceLast [
	self updateProcessList
]

{ #category : 'initialization' }
ProcessBrowser >> stopCPUWatcher [

	CPUWatcher stopMonitoring.
	self updateProcessList
]

{ #category : 'initialization' }
ProcessBrowser >> stopStackSizeWatcher [

	StackSizeWatcher stopWatching.
	self updateProcessList
]

{ #category : 'process actions' }
ProcessBrowser >> suspendProcess [
	| rule |
	(selectedProcess isSuspended or: [ selectedProcess isTerminated ])
		ifTrue: [^ self].
	rule := (self class rulesFor: selectedProcess) first.
	rule
		ifFalse: [
			PopUpMenu inform: 'Nope, won''t suspend ' , selectedProcess name.
			^ self].
	self class suspendProcess: selectedProcess.
	self updateProcessList
]

{ #category : 'process actions' }
ProcessBrowser >> terminateProcess [
	| rule |
	rule := (self class rulesFor: selectedProcess) first.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t kill ' , selectedProcess name.
			^ self].
	self class terminateProcess: selectedProcess.	
	self updateProcessList
]

{ #category : 'accessing' }
ProcessBrowser >> text [
	^methodText
]

{ #category : 'process list' }
ProcessBrowser >> updateProcessList [
	| oldSelectedProcess |
	oldSelectedProcess := selectedProcess.
	processList := selectedProcess := selectedSelector := nil.
	Smalltalk garbageCollectMost.
	"lose defunct processes"

	processList := Processor processes.
	processList := processList sort: [ :a :b | a priority >= b priority ].
	processList := WeakArray withAll: processList.
	self changed: #processNameList.
	self selectedProcess: ((processList includes: oldSelectedProcess)
		ifTrue: [oldSelectedProcess])
]

{ #category : 'stack list' }
ProcessBrowser >> updateStackList [
	self updateStackList: Debugger defaultDebugStackSize
]

{ #category : 'stack list' }
ProcessBrowser >> updateStackList: depth  [
	| suspendedContext oldHighlight |
	selectedProcess
		ifNil: [ ^ self changeStackListTo: nil ].
	oldHighlight := selectedContext.
	selectedProcess == Processor activeProcess
		ifTrue: [
			self changeStackListTo: (thisContext stackOfSize: depth)]
		ifFalse: [
			suspendedContext := selectedProcess suspendedContext.
			self changeStackListTo: (suspendedContext ifNotNil: [suspendedContext stackOfSize: depth])].
	self context: ((stackList notNil and:  [(stackList includes: oldHighlight)]) ifTrue: [oldHighlight])
]

{ #category : 'process actions' }
ProcessBrowser >> wasProcessSuspendedByProcessBrowser: aProcess [
	^self class suspendedProcesses includesKey: aProcess
]
