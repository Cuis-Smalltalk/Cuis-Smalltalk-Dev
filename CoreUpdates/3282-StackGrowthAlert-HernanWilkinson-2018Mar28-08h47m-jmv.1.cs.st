'From Cuis 5.0 of 7 November 2016 [latest update: #3281] on 28 March 2018 at 9:05:04 am'!
!classDefinition: #StackSizeWatcher category: #'Tools-Profiling'!
Object subclass: #StackSizeWatcher
	instanceVariableNames: 'watcher timeBetweenChecks stackSizeThreashold'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: 'StackSizeWatcher class' category: #'Tools-Profiling'!
StackSizeWatcher class
	instanceVariableNames: 'current'!

!ProcessBrowser methodsFor: 'initialization' stamp: 'jmv 3/28/2018 08:57:18'!
startStackSizeWatcher

	StackSizeWatcher isWatching ifFalse: [
		StackSizeWatcher startWatchingWithDefaults ]! !

!ProcessBrowser methodsFor: 'initialization' stamp: 'jmv 3/28/2018 08:56:50'!
stopStackSizeWatcher

	StackSizeWatcher stopWatching.
	self updateProcessList! !


!ContextPart methodsFor: 'accessing' stamp: 'HAW 3/27/2018 14:02:09'!
depthBelow

	^self depthBelow: nil! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 3/28/2018 08:55:53'!
startStackSizeWatcher

	model startStackSizeWatcher! !

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 3/28/2018 08:56:08'!
stopStackSizeWatcher
	model stopStackSizeWatcher! !


!StackSizeWatcher methodsFor: 'assertions' stamp: 'HAW 3/27/2018 14:16:52'!
assertIsNotWatching
	
	self isNotWatching ifFalse: [ self error: 'Already watching' ].! !

!StackSizeWatcher methodsFor: 'assertions' stamp: 'HAW 3/27/2018 14:16:43'!
assertIsWatching
	
	self isWatching ifFalse: [ self error: 'It is not watching' ]! !

!StackSizeWatcher methodsFor: 'configuration changing' stamp: 'HAW 3/27/2018 14:21:02'!
changeStackSizeThresholdTo: aThreshold
	
	stackSizeThreashold _ aThreshold ! !

!StackSizeWatcher methodsFor: 'configuration changing' stamp: 'HAW 3/27/2018 14:21:17'!
changeTimeBetweenChecksTo: aTimeBetweenChecks
	
	"time in milliseconds - Hernan"
	
	timeBetweenChecks _ aTimeBetweenChecks ! !

!StackSizeWatcher methodsFor: 'testing' stamp: 'HAW 3/27/2018 14:49:41'!
canDebug: aProcess

	^(ProcessBrowser rulesFor: aProcess) second
	
! !

!StackSizeWatcher methodsFor: 'testing' stamp: 'HAW 3/27/2018 13:51:35'!
isNotWatching

	^self isWatching not! !

!StackSizeWatcher methodsFor: 'testing' stamp: 'HAW 3/27/2018 14:55:20'!
isStackTooDeepAt: aProcess

	"aProcess suspendedContext should never be nil under this circunstances but checking that just in case - Hernan"
	^aProcess suspendedContext 
		ifNil: [ false ] 
		ifNotNil: [ :topContext | topContext depthBelow > stackSizeThreashold ]
		! !

!StackSizeWatcher methodsFor: 'testing' stamp: 'HAW 3/27/2018 13:51:28'!
isWatching

	^watcher notNil ! !

!StackSizeWatcher methodsFor: 'testing' stamp: 'HAW 3/27/2018 14:49:21'!
shouldStopAndDebug: aProcess

	^(self isStackTooDeepAt: aProcess) and: [self canDebug: aProcess]
	
! !

!StackSizeWatcher methodsFor: 'start/stop' stamp: 'jmv 3/28/2018 08:50:08'!
startWatching

	self assertIsNotWatching.
	
	watcher _ [ [self watch] repeat ] newProcess.
	watcher priority: Processor lowIOPriority.
	watcher name: 'StackSizeWatcher monitor'.
	watcher resume.
	Processor yield! !

!StackSizeWatcher methodsFor: 'start/stop' stamp: 'HAW 3/27/2018 14:19:22'!
startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold

	self assertIsNotWatching.
	
	self changeTimeBetweenChecksTo: aTimeBetweenChecks.
	self changeStackSizeThresholdTo: aThreshold.
	self startWatching ! !

!StackSizeWatcher methodsFor: 'start/stop' stamp: 'HAW 3/27/2018 13:50:21'!
stopWatching

	self assertIsWatching.
	
	watcher terminate.
	watcher _ nil! !

!StackSizeWatcher methodsFor: 'private' stamp: 'HAW 3/27/2018 14:53:56'!
debug: aProcess

	aProcess debugFullWithTitle: 'Interrupted - Stack too deep'.
! !

!StackSizeWatcher methodsFor: 'private' stamp: 'HAW 3/27/2018 14:47:26'!
watch

	| processToWatch |
	
	(Delay forMilliseconds: timeBetweenChecks) wait.
	processToWatch := Processor nextReadyProcess.
	(self shouldStopAndDebug: processToWatch) ifTrue: [ self debug: processToWatch ]
! !


!StackSizeWatcher class methodsFor: 'current' stamp: 'HAW 3/27/2018 14:12:49'!
current

	current isNil ifTrue: [ current _ self new ].
	^current! !

!StackSizeWatcher class methodsFor: 'current' stamp: 'HAW 3/27/2018 14:15:30'!
resetCurrent

	current _ nil! !

!StackSizeWatcher class methodsFor: 'defaults' stamp: 'HAW 3/27/2018 14:20:12'!
defaultStackSizeThreshold

	^2000! !

!StackSizeWatcher class methodsFor: 'defaults' stamp: 'HAW 3/27/2018 14:07:34'!
defaultTimeBetweenChecks 

	"Time in milliseconds - Hernan"
	^10! !

!StackSizeWatcher class methodsFor: 'start/stop' stamp: 'jmv 3/28/2018 08:54:09'!
isWatching
	^ current notNil and: [ current isWatching ]! !

!StackSizeWatcher class methodsFor: 'start/stop' stamp: 'HAW 3/27/2018 14:19:58'!
startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold

	^self current startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold
! !

!StackSizeWatcher class methodsFor: 'start/stop' stamp: 'HAW 3/27/2018 14:20:22'!
startWatchingWithDefaults

	^self startWatchingAt: self defaultTimeBetweenChecks informingWhenStackSizeBiggerThan: self defaultStackSizeThreshold! !

!StackSizeWatcher class methodsFor: 'start/stop' stamp: 'HAW 3/27/2018 14:15:17'!
stopWatching

	self current stopWatching.
	self resetCurrent	! !


!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/28/2018 08:58:40'!
processListMenu
	| menu rules |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			rules _ model class rulesFor: model selectedProcess.
			menu addList: #(
				('inspect (i)'					#inspectProcess)
				('explore (I)'				#exploreProcess)
				('references finder'			#openReferencesFinder)).
			rules first
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess]].
			rules second
				ifTrue: [
					menu addList: #(
						('change priority (p)'		 	#changePriority)
						('debug (d)'						#debugProcess))].
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
	StackSizeWatcher isWatching
			ifTrue: [ menu add: 'stop StackSizeWatcher' action: #stopStackSizeWatcher ]
			ifFalse: [ menu add: 'start StackSizeWatcher' action: #startStackSizeWatcher  ].

	^ menu! !


!Theme methodsFor: 'menus' stamp: 'jmv 3/28/2018 09:03:29'!
basicIcons

	"Minimal menu scheme.
	Theme current class beCurrent
	"

	^ { 
		#('open...') -> #openIcon.
		#('windows...' 'find window' 'Focus follows mouse' 'Click to focus') -> #windowIcon.
		#('help...' 'explain' 'about this system...' 'Terse Guide to Cuis' 'Class Comment Browser' 'Code management in Cuis' 'Using GitHub to host Cuis packages' ) -> #helpIcon.
		#('themes...') -> #appearanceIcon.
		#('do it (d)') -> #doItIcon.
		#('new morph...' 'objects (o)' 'save world as morph file') -> #morphsIcon.
		#('save' ) -> #saveIcon.
		#('Save options...' 'save as...' 'change category...' 'rename change set (r)' 'rename') -> #saveAsIcon.
		#('save as new version') -> #saveAsNewVersionIcon.
		#('quit') -> #quitIcon.
		#('save and quit' ) -> #saveAndQuitIcon.
		#('inspect it (i)' 'inspect world'  'inspect model' 'inspect morph'
		 'inspect owner chain' 'inspect' 'inspect (i)' 'basic inspect' 'message names' 'find message names' 'inspect instances' 'inspect subinstances' 'inspect change set' 'inspect context (c)' 'inspect receiver (i)' 'start CPUWatcher' 'stop CPUWatcher')
			-> #inspectIcon.
		#('explore' 'explore it (I)' 'explore world' 'explore morph' 'explore (I)' 'explore context (C)' 'explore receiver (I)' 'references finder' 'weight explorer' 'explore CompiledMethod') -> #exploreIcon.
		#('find...(f)' 'find class... (f)' 'find method...' 'find recent submissions' 'show hierarchy' 'show definition' 'show comment' 'filter' 'filter message list...' 'find context... (f)') -> #findIcon.
		#('add item...' 'new category...' 'create new change set...' 'new change set... (n)' 'add new file') -> #newIcon.
		#('remove method (x)' 'remove' 'remove class (x)' 'remove method from system (x)' 'remove class from system (x)' 'remove postscript''stop StackSizeWatcher' ) -> #deleteIcon.
		#('delete method from changeset (d)' 'delete class from change set (d)' 'destroy change set (X)' 'revert & remove from changes' 'delete unchanged windows' 'delete non windows' 'delete both of the above' 'reset variables' 'remove contained in class categories...' 'clear this change set' 'uninstall this change set' 'delete directory...' 'delete' 'start StackSizeWatcher') -> #warningIcon.
		#('do again (j)' 'Redo - multiple (Z)') -> #redoIcon.
		#('undo (z)' 'revert to previous version' 'Undo - multiple (z)') -> #undoIcon.
		#('copy to clipboard (c)' 'copy (c)' 'copy class...' 'copy class chgs to other side' 'copy method to other side' 'copy all to other side (c)' 'copy name to clipboard' 'copy selector to clipboard') -> #copyIcon.
		#('paste (v)' 'Paste without Format') -> #pasteIcon.
		#('cut (x)' 'move class chgs to other side' 'move method to other side' 'submerge into other side') -> #cutIcon.
		#('paste...' 'icons...') -> #worldIcon.
}! !

!Theme methodsFor: 'menus' stamp: 'jmv 3/28/2018 09:03:59'!
miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history' 'code file contents' 'package file contents') -> #changesIcon.
		#('Install New Updates' 'update' 'turn on auto-update (a)' 'update list (u)' 'install code' 'fileIn entire file' 'install package') -> #updateIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' 'turn off auto-update (a)' 'stop StackSizeWatcher') -> #cancelIcon.
		#('debug...' 'Debug it (D)' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'restore all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor'  'basic text editor'  'unicode editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList' 'compress file') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner' 'Accept & Run Test in Class (t)' 'Accept & Run Test in Category (y)' 'Run tests (t)' 'Run tests' 'Run test (t)' 'Accept & Debug Test (r)' 'Debug test (r)') -> #weatherFewCloudsIcon.
		#('Font Sizes...' 'system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'All preferences...' 'what to show...' 'view as hex') -> #preferencesIcon.
		#('Editor keyboard shortcuts') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.		"currently unused, but a neat icon"
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set code author...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'Profile it') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance') -> #instanceIcon.
		#('Use Selection for Find (j)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('smalltalk options' 'method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)' 'Toggle WordWrap') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)' 'More...') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('find changed windows...') -> #newWindowIcon.
		#('make undraggable') -> #pushPinIcon.
		#('Utilities saveScreenshot.') -> #stillCameraIcon.
		#('add new directory') -> #newFolderIcon.
		#('select all' 'deselect all') -> #selectAllIcon.
		#('sort by date') -> #dateIcon.
		#('justified') -> #formatJustifyFillIcon.
		#('centered') -> #formatJustifyCenterIcon.
		#('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon.
		#('rightFlush') -> #formatJustifyRightIcon.
		#('signal Semaphore (S)') -> #haloHelpIcon.
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...' 'Set Default Font...') -> #fontXGenericIcon.
		#('Clear Font') -> #newIcon.
		#('code file browser' 'package file browser') -> #findIcon.
	}! !


!StackSizeWatcher class reorganize!
('current' current resetCurrent)
('defaults' defaultStackSizeThreshold defaultTimeBetweenChecks)
('start/stop' isWatching startWatchingAt:informingWhenStackSizeBiggerThan: startWatchingWithDefaults stopWatching)
!

