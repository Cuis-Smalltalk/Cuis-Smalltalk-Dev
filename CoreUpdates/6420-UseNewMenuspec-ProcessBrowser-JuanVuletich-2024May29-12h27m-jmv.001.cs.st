'From Cuis7.1 [latest update: #6418] on 29 May 2024 at 3:44:40 pm'!

!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/29/2024 15:08:43'!
processListMenuSpec
	"Spec for the menu and shortcut keys for the list of processes.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	| rules answer |
	answer := #().
	model selectedProcess ifNotNil: [ :selectedProcess |
		rules := model class rulesFor: model selectedProcess.
		answer := answer, #(
				#(10		'inspect (i)' 						$i 	window 			inspectProcess 						nil 	inspectIcon)
				#(20		'explore (I)' 						$I 	window 			exploreProcess 						nil 	exploreIcon)
				#(30		'references finder (P)' 						$P 	window 			openReferencesFinder 					nil 		exploreIcon)).
		rules first ifTrue: [
			answer := answer, 
				#((40		'terminate (t)' 						$t 	model 			terminateProcess 						nil 	cancelIcon)).
			selectedProcess isSuspended ifTrue: [ answer := answer,
				#((50		'resume (r)' 						$r 	model 			resumeProcess 						nil 	mediaPlaybackStartIcon))]
			ifFalse: [ answer := answer, 
				#((50 		'suspend (s)' 						$s 	model 			suspendProcess 						nil 	chatIcon))]].
		rules second ifTrue: [ answer := answer, #(
				#(60 		'change priority (p)' 						$p 	window 			changePriority 						nil 	systemMonitorIcon)
				#(70 		'debug (d)' 						$d 	window 			debugProcess 						nil 	debugIcon))].
		(selectedProcess suspendingList isKindOf: Semaphore) ifTrue: [ answer := answer,
				#((80 		'signal Semaphore (S)' 						$S 	model 			signalSemaphore 						nil 	haloHelpIcon))].
		answer := answer, #(
				#(90		'full stack (k)' 						$k 	model 			moreStack 						nil 	systemIcon)
				#(100 		'-----'))].
	answer := answer, #(
				#(110		'find context... (f)' 						$f 	window 			findContext 						nil 	findIcon)
				#(120		'find again (g)' 						$g 	model 			nextContext 						nil 	systemIcon)
				#(130 		'-----')).
	answer := answer, (isStepping ifTrue: [
				#((140		'turn off auto-update (a)' 						$a 	window 			toggleAutoUpdate 						nil 	cancelIcon)) ]
			ifFalse: [
				#((140		'turn on auto-update (a)' 						$a 	window 			toggleAutoUpdate 						nil 	updateIcon)) ]).
	answer := answer, #(
				#(150		'update list (u)' 						$u 	model 			updateProcessList 						nil 	updateIcon)
				#(160 		'-----')).
	answer := answer, (CPUWatcher isMonitoring ifTrue: [
				#((170		'stop CPUWatcher' 						nil 	model 			stopCPUWatcher 						nil 	inspectIcon)) ]
			ifFalse: [
				#((170		'start CPUWatcher' 						nil 	model 			startCPUWatcher 						nil 	inspectIcon)) ]).
	answer := answer, (StackSizeWatcher isWatching ifTrue: [
				#((180		'stop StackSizeWatcher' 						nil 	model 			stopStackSizeWatcher 						nil 	inspectIcon)) ]
			ifFalse: [
				#((180		'start StackSizeWatcher' 						nil 	model 			startStackSizeWatcher 						nil 	inspectIcon)) ]).
	^answer! !

!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/29/2024 12:55:56'!
stackMenuSpec
	"Spec for the menu and shortcut keys for the list of stack frames.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	^ #(
		#(10		'inspect context (c)' 					$c 	window 			inspectContext 					nil 	inspectIcon)
		#(20		'explore context (C)' 					$C 	window 			exploreContext 					nil 	exploreIcon)
		#(30		'inspect receiver (i)' 					$i 	window 			inspectReceiver 					nil 	inspectIcon)
		#(40		'explore receiver (I)' 					$I 	window 			exploreReceiver 					nil 	exploreIcon)
		#(50		'browse context (b)' 					$b 	window 			browseContext 					nil 	editFindReplaceIcon)
	).! !


!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/29/2024 12:54:27'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	^#(
		#(10		'fullStack (f)' 						$f 	window 			fullStack 						nil 	nil 	'show the complete call stack')
		#(20		'proceed (p)' 						$p 	window 			proceed 						nil 	nil 	'close the debugger and proceed')
		#(30		'restart (r)' 						$r 	window 			restart 						nil 	nil 	'reset this context to its start')
		#(40		'step into send (e)' 						$e 	window 			send 						nil 	nil 	'step into message sends')
		#(50		'step over send (t)' 						$t 	window 			doStep 						nil 	nil 	'step over message sends')
		#(60		'step into block (T)' 						$T 	window 			stepIntoBlock 						nil 	nil 	'step into a code block')
		#(70		'run to cursor' 						nil 	window 			runToCursor 						nil 	nil 	'run to cursor')
		#(80		'where (w)' 						$w 	window 			where 						nil 	nil 	'select current pc range')
		#(90		'peel to first like this' 						nil 	window 			peelToFirst 						nil 	nil 	'peel stack back to the second occurance of selected message')
		#(100 		'-----')
		#(110		'return entered value' 						nil 	window 			returnValue 						nil 	nil 	'lets you enter an expression and return method answering result')
		#(120		'toggle break on entry' 						nil 	model 			toggleBreakOnEntry 						nil 	nil 	'install or uninstall a breakpoint on method entry')
	)! !


!ProcessBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/29/2024 15:41:00'!
buildMorphicWindow
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
	upperRow := LayoutMorph newRow.
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
	self setLabel: 'Process Browser'! !

!ProcessBrowserWindow methodsFor: 'open/close' stamp: 'jmv 5/29/2024 15:06:07'!
openInWorld
	
	super openInWorld.
	self startAutoUpdate.
	model startCPUWatcher! !

!methodRemoval: ProcessBrowserWindow #processListMenu stamp: 'jmv 5/29/2024 15:43:21'!
ProcessBrowserWindow removeSelector: #processListMenu!
!methodRemoval: ProcessBrowserWindow #stackListKey:from: stamp: 'jmv 5/29/2024 12:42:01'!
ProcessBrowserWindow removeSelector: #stackListKey:from:!
!methodRemoval: ProcessBrowserWindow #processListKey:from: stamp: 'jmv 5/29/2024 15:43:13'!
ProcessBrowserWindow removeSelector: #processListKey:from:!
!methodRemoval: ProcessBrowserWindow #stopCPUWatcher stamp: 'jmv 5/29/2024 15:05:43'!
ProcessBrowserWindow removeSelector: #stopCPUWatcher!
!methodRemoval: ProcessBrowserWindow #stackListMenu stamp: 'jmv 5/29/2024 12:42:04'!
ProcessBrowserWindow removeSelector: #stackListMenu!
!methodRemoval: ProcessBrowserWindow #startCPUWatcher stamp: 'jmv 5/29/2024 15:06:20'!
ProcessBrowserWindow removeSelector: #startCPUWatcher!
!methodRemoval: ProcessBrowserWindow #stopStackSizeWatcher stamp: 'jmv 5/29/2024 15:05:16'!
ProcessBrowserWindow removeSelector: #stopStackSizeWatcher!
!methodRemoval: ProcessBrowserWindow #startStackSizeWatcher stamp: 'jmv 5/29/2024 15:05:56'!
ProcessBrowserWindow removeSelector: #startStackSizeWatcher!

!ProcessBrowserWindow reorganize!
('initialization' initialize)
('GUI building' buildMorphicWindow)
('menu building')
('menu commands' browseContext changePriority debugProcess exploreContext exploreProcess exploreReceiver findContext inspectContext inspectProcess inspectReceiver openReferencesFinder startAutoUpdate stopAutoUpdate toggleAutoUpdate)
('keyboard shortcuts')
('stepping' stepTime)
('open/close' delete openInWorld)
('menu & kbd shortcuts' processListMenuSpec stackMenuSpec)
!

