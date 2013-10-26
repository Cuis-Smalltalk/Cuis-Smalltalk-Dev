'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 12:22:59 pm'!

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/29/2012 12:21'!
                        fileOutAndKeep
	"File out the current change set."

	myChangeSet fileOut! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/29/2012 12:21'!
          fileOutAndRemove
	"File out the current change set."

	myChangeSet fileOut.
	self removePrompting: false.
	self update! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/29/2012 12:04'!
                     changeSetForBaseSystem

	| csName thisNumber |
	Installing

		ifNil: [
			csName _ self baseSystemNameFor: self lastUsedNumber.
			ChangeSorter allChangeSets
				detect: [ :any | any name beginsWith: csName ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					LastUsedNumber _ self lastUsedNumber + 1.
					csName _ (self baseSystemNameFor: self lastUsedNumber),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSorter existingOrNewChangeSetNamed: csName ]]

		ifNotNil: [
			thisNumber _ Installing asNumber.
			thisNumber = (self lastUsedNumber + 1) ifTrue: [
				LastUsedNumber _ thisNumber ].
			csName _ 'Affects-BaseSystem--Install-', Installing.
			^ChangeSorter existingOrNewChangeSetNamed: csName ]! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 3/29/2012 12:19'!
                   changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu title: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble.
				aMenu add: 'Remove preamble' 	action: #removePreamble]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 		action: #addPreamble].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'Edit postscript...' 		action: #editPostscript.
				aMenu add: 'Remove postscript' 	action: #removePostscript]
			ifFalse: [
				aMenu add: 'Add postscript...' 		action: #editPostscript].
	].
	aMenu addLine.
	aMenu add: 'File out and keep (k)' 					action: #fileOutAndKeep.
	aMenu add: 'Rename change set (r)' 			action: #rename.
	aMenu add: 'Destroy change set (x)' 			action: #remove.
	aMenu addLine.
	aMenu add: 'more...' 							target: self				action: #offerShiftedChangeSetMenu.
	^ aMenu! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 3/29/2012 12:19'!
     changeSetListKey: aChar from: view
	"Respond to a Command key.  I am a model with a listView that has a list of changeSets."

	aChar == $D ifTrue: [^ model toggleDiffing]. 
	aChar == $o ifTrue: [^ model fileOutAndRemove].
	aChar == $k ifTrue: [^ model fileOutAndKeep].
	aChar == $r ifTrue: [^ model rename].
	aChar == $x ifTrue: [^ model remove].

	^ self messageListKey: aChar from: view! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 3/29/2012 12:08'!
                      save
	| nameToUse |
	fullFileName ifNil: [
		fullFileName _
			(ChangeSet defaultChangeSetDirectory pathName, FileDirectory slash, 
			self packageName, FileDirectory dot, 'pck')
				asFileName ].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck') asFileName ]."
	Cursor write
		showWhile: [
			| file |
			file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]].
	self hasUnsavedChanges: false.
	ChangeSorter removeChangeSet: (ChangeSet changeSetForPackage: self)! !


!Theme methodsFor: 'menus' stamp: 'jmv 3/29/2012 12:22'!
                      miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history') -> #changesIcon.
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
		#('system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'what to show...') -> #preferencesIcon.
		#('command-key help') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set author initials...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'profile messages (m)') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance' 'inspect Pointers (P)') -> #instanceIcon.
		#('Use Selection for Find (h)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
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

!methodRemoval: ChangeSorter #fileOut!
ChangeSorter removeSelector: #fileOut!
