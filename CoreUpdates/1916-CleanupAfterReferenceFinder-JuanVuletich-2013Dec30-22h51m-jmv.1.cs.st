'From Cuis 4.2 of 25 July 2013 [latest update: #1915] on 30 December 2013 at 10:57:07.632913 pm'!

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/30/2013 22:19'!
openReferencesFinder

	"Create and schedule a References Finder on the receiver's model's currently selected object."
	model selectedProcess ifNotNil: [ :pr | ReferenceFinder openOn: pr]! !


!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 12/30/2013 22:20'!
processListMenu
	| menu |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			| rules | 
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

!ProcessBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 12/30/2013 22:20'!
processListKey: aKey from: aView 
	^ aKey caseOf: {
		[$i] -> [self inspectProcess].
		[$I] -> [self exploreProcess].
		[$P] -> [self openReferencesFinder].
		[$t] -> [model terminateProcess].
		[$r] -> [model resumeProcess].
		[$s] -> [model suspendProcess].
		[$d] -> [self debugProcess].
		[$p] -> [self changePriority].
		[$m] -> [self messageTally].
		[$f] -> [self findContext].
		[$g] -> [model nextContext].
		[$a] -> [self toggleAutoUpdate].
		[$u] -> [model updateProcessList].
		[$S] -> [model signalSemaphore].
		[$k] -> [model moreStack]}
		 otherwise: []! !


!Theme methodsFor: 'menus' stamp: 'jmv 12/30/2013 22:21'!
miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history') -> #changesIcon.
		#('Install New Updates') -> #updateIcon.
		#('print PS to file...' ) -> #printIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' ) -> #cancelIcon.
		#('debug...' 'debug it' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'expand all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner') -> #weatherFewCloudsIcon.
		#('Font Sizes...' 'system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'All preferences...' 'what to show...') -> #preferencesIcon.
		#('Editor keyboard shortcuts') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.		"currently unused, but a neat icon"
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set code author...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'profile messages (m)') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance') -> #instanceIcon.
		#('Use Selection for Find (j)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('smalltalk options' 'method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('update' 'turn on auto-update (a)' 'update list (u)') -> #updateIcon.
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
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...') -> #fontXGenericIcon.
	}! !

!methodRemoval: ProcessBrowserWindow #inspectPointers!
ProcessBrowserWindow removeSelector: #inspectPointers!
!methodRemoval: SystemDictionary #browseAllObjectReferencesTo:except:ifNone:!
SystemDictionary removeSelector: #browseAllObjectReferencesTo:except:ifNone:!
!methodRemoval: SystemDictionary #pointersToEachIn:!
SystemDictionary removeSelector: #pointersToEachIn:!
!methodRemoval: SystemDictionary #pointersToItem:of:!
SystemDictionary removeSelector: #pointersToItem:of:!
!methodRemoval: ProcessBrowser #pointers!
ProcessBrowser removeSelector: #pointers!
