"
Morphic view for ProcessBrowser models. See category 'GUI building'.
"
Class {
	#name : 'ProcessBrowserWindow',
	#superclass : 'SystemWindow',
	#instVars : [
		'isStepping'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu-world' }
ProcessBrowserWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		10.
			#label 			-> 		'Process Browser'.
			#object 		-> 		ProcessBrowser.
			#selector 		-> 		#open.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> browseContext [
	model selectedContext
		ifNil: [^ self].
	Smalltalk browseFull: model selectedClass selector: model selectedSelector.
]

{ #category : 'GUI building' }
ProcessBrowserWindow >> buildMorphicWindow [
	"Create a pluggable version of me, answer a window"
	| aTextMorph list1 list2 upperRow |
	list1 := (PluggableListMorph
				withModel: model
				listGetter: #processNameList
				indexGetter: #processListIndex
				indexSetter: #processListIndex:)
					menuNshortcutsKey: #processListMenuSpec.
	list2 := (PluggableListMorph
				withModel: model
				listGetter: #stackNameList
				indexGetter: #stackListIndex
				indexSetter: #stackListIndex:)
					menuNshortcutsKey: #stackMenuSpec.
	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.
	aTextMorph := (TextModelMorph
				textProvider: model
				textGetter: #selectedMethod) emptyTextDisplayMessage: 'Method source (not selected?)'.
	aTextMorph askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: aTextMorph proportionalHeight: 0.5.
	self setLabel: 'Process Browser'
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> changePriority [
	| rule |
	rule := (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [self inform: 'Nope, won''t change priority of ' , model selectedProcess name.
			^ self].
	self request: 'New priority'
		initialAnswer: model selectedProcess priority printString
		verifying: [:aString| ([aString asNumber asInteger] on: Error do: []) isInteger]
		do: [ :aString |
			| newPriority |
			newPriority := aString asNumber asInteger.
			(newPriority between: 1 and: Processor highestPriority)
			ifTrue:
				[model class setProcess: model selectedProcess toPriority: newPriority.
				model updateProcessList]
			ifFalse: [self inform: 'Bad priority']]
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> debugProcess [
	| rule |
	rule := (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t debug ' , model selectedProcess name.
			^ self].
	model class debugProcess: model selectedProcess.
]

{ #category : 'open/close' }
ProcessBrowserWindow >> delete [
	model stopCPUWatcher.
	super delete
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> exploreContext [
	model selectedContext explore
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> exploreProcess [
	model selectedProcess explore
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> exploreReceiver [
	"Explore the receiver."

	model selectedContext
		ifNotNil: [ :context | context receiver explore ]
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> findContext [
	self request: 'Enter a string to search for in the process stack lists'
		initialAnswer: model searchString
		do: [:searchString| model findContext: searchString]
]

{ #category : 'initialization' }
ProcessBrowserWindow >> initialize [
	super initialize.
	isStepping := false.
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> inspectContext [
	model selectedContext inspect
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> inspectProcess [
	model selectedProcess inspect
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> inspectReceiver [
	model selectedContext
		ifNotNil: [ :context | context receiver inspect]
]

{ #category : 'open/close' }
ProcessBrowserWindow >> openInWorld [
	
	super openInWorld.
	self startAutoUpdate.
	model startCPUWatcher
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> openReferencesFinder [

	"Create and schedule a References Finder on the receiver's model's currently selected object."
	model selectedProcess ifNotNil: [ :pr | ReferenceFinder openExplorerOn: pr]
]

{ #category : 'menu & kbd shortcuts' }
ProcessBrowserWindow >> processListMenuSpec [
	"Spec for the menu and shortcut keys for the list of processes.
	See coment at Morph >> #getMenu
	"
	| rules answer |
	answer := #().
	model selectedProcess ifNotNil: [ :selectedProcess |
		rules := model class rulesFor: model selectedProcess.
		answer := answer, #(
				#(10		'Inspect (i)' 						$i 	window 	inspectProcess 			inspectIcon)
				#(20		'Explore (I)' 						$I 	window 	exploreProcess 			exploreIcon)
				#(30		'References Finder (P)' 			$P 	window 	openReferencesFinder 		exploreIcon)).
		rules first ifTrue: [
			answer := answer, 
				#((40		'Terminate (t)' 					$t 	model 		terminateProcess 			cancelIcon)).
			selectedProcess isSuspended ifTrue: [ answer := answer,
				#((50		'Resume (r)' 						$r 	model 		resumeProcess 			mediaPlaybackStartIcon))]
			ifFalse: [ answer := answer, 
				#((50 		'Suspend (s)' 					$s 	model 		suspendProcess 			chatIcon))]].
		rules second ifTrue: [ answer := answer, #(
				#(60 		'Change priority (p)' 				$p 	window 	changePriority 				systemMonitorIcon)
				#(70 		'Debug (d)' 						$d 	window 	debugProcess 				debugIcon))].
		(selectedProcess suspendingList isKindOf: Semaphore) ifTrue: [ answer := answer,
				#((80 		'Signal Semaphore (S)' 			$S 	model 		signalSemaphore 			haloHelpIcon))].
		answer := answer, #(
				#(90		'Full stack (k)' 					$k 	model 		moreStack 					systemIcon)
				#(100 		'-----'))].
	answer := answer, #(
				#(110		'Find context... (f)' 				$f 	window 	findContext 				findIcon)
				#(120		'Find again (g)' 					$g 	model 		nextContext 				systemIcon)
				#(130 		'-----')).
	answer := answer, (isStepping ifTrue: [
				#((140		'Turn off auto-update (a)' 		$a 	window 	toggleAutoUpdate 			cancelIcon)) ]
			ifFalse: [
				#((140		'Turn on auto-update (a)' 		$a 	window 	toggleAutoUpdate 			updateIcon)) ]).
	answer := answer, #(
				#(150		'Update list (u)' 					$u 	model 		updateProcessList 			updateIcon)
				#(160 		'-----')).
	answer := answer, (CPUWatcher isMonitoring ifTrue: [
				#((170		'Stop CPUWatcher' 				nil 	model 		stopCPUWatcher 			inspectIcon)) ]
			ifFalse: [
				#((170		'Start CPUWatcher' 				nil 	model 		startCPUWatcher 			inspectIcon)) ]).
	answer := answer, (StackSizeWatcher isWatching ifTrue: [
				#((180		'Stop StackSizeWatcher' 			nil 	model 		stopStackSizeWatcher 		inspectIcon)) ]
			ifFalse: [
				#((180		'Start StackSizeWatcher' 			nil 	model 		startStackSizeWatcher 	inspectIcon)) ]).
	^answer
]

{ #category : 'menu & kbd shortcuts' }
ProcessBrowserWindow >> stackMenuSpec [
	"Spec for the menu and shortcut keys for the list of stack frames.
	See coment at Morph >> #getMenu
	"
	^ #(
		#(10	'Inspect Context (c)' 		$c 	window 	inspectContext 	inspectIcon)
		#(20	'Explore Context (C)' 		$C 	window 	exploreContext 	exploreIcon)
		#(30	'Inspect Receiver (i)' 		$i 	window 	inspectReceiver 	inspectIcon)
		#(40	'Explore Receiver (I)' 		$I 	window 	exploreReceiver 	exploreIcon)
		#(50	'Browse Context (b)' 		$b 	window 	browseContext 	editFindReplaceIcon)
	).
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> startAutoUpdate [
	self startStepping.
	isStepping := true
]

{ #category : 'stepping' }
ProcessBrowserWindow >> stepTime [
	"Update 5 times per second."
	^ 200
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> stopAutoUpdate [
	self stopStepping.
	isStepping := false
]

{ #category : 'menu commands' }
ProcessBrowserWindow >> toggleAutoUpdate [
	isStepping
		ifTrue: [ self stopAutoUpdate ]
		ifFalse: [ self startAutoUpdate ].

]
