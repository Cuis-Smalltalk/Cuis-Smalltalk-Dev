'From Cuis 4.2 of 25 July 2013 [latest update: #2296] on 3 May 2015 at 11:36:27.815611 am'!

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 5/3/2015 11:08'!
restoreAll
	"Expand all windows to their previous state"
	Taskbar restoreAll! !


!Taskbar methodsFor: 'initialization' stamp: 'jmv 5/3/2015 11:33'!
intoWorld: aWorld
	Display
		when: #screenSizeChanged
		send: #screenSizeChanged
		to: self.
	self screenSizeChanged! !


!Taskbar class methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:26'!
clearSoleInstance
	SoleInstance _ nil! !

!Taskbar class methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:27'!
restoreAll

	SoleInstance ifNotNil: [ :tb | tb restoreAll ]! !


!TaskbarTask methodsFor: 'commands' stamp: 'jmv 11/10/2013 23:29'!
minimize: aMorph

	| button imageForm |

	morph _ aMorph.

	self turnOn.

	button _ PluggableButtonMorph
			model: self
			stateGetter: nil
			action: #switch.
	imageForm _ aMorph imageForm: 32.
	button
		color: Taskbar singleton color;
		icon: imageForm;
		setBalloonText: morph label.
	morph visible: false.
			
	^button! !


!Taskbar methodsFor: 'change reporting' stamp: 'jmv 5/3/2015 11:26'!
delete

	self restoreAll.
	super delete.
	Display removeActionsWithReceiver: self.
	Taskbar clearSoleInstance! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:10'!
restoreAll
	
	viewBox ifNotNil: [ viewBox submorphs reverseDo: [ :button |  button model restore ]]! !

!Taskbar methodsFor: 'stepping' stamp: 'jmv 5/3/2015 11:14'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	WorldState addDeferredUIMessage: [
		self world ifNotNil: [ :w |
			self
				morphPosition: 0@ (w morphExtent y - (self externalizeDistance: extent ) y)
				extent: (self internalize: self world morphExtent) x @ self defaultHeight ]]! !


!Taskbar class methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:27'!
hideTaskbar

	SoleInstance ifNotNil: [ :tb | tb delete ]! !

!Taskbar class methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:28'!
minimize: aMorph

	self showTaskbar.
	SoleInstance minimize: aMorph! !

!Taskbar class methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:27'!
restore: aMorph

	SoleInstance
		ifNotNil: [ :tb | tb restore: aMorph ]! !

!Taskbar class methodsFor: 'commands' stamp: 'jmv 5/3/2015 11:28'!
showTaskbar

	SoleInstance ifNil: [
		SoleInstance _ self newRow.
		SoleInstance openInWorld ]! !

!Taskbar class methodsFor: 'testing' stamp: 'jmv 5/3/2015 11:27'!
isCollapsed: aWindow

	^ SoleInstance
		ifNil: [ false ]
		ifNotNil: [ :tb | tb isCollapsed: aWindow ]! !

!Taskbar class methodsFor: 'instance creation' stamp: 'jmv 5/3/2015 11:30'!
singleton

	^ SoleInstance! !

!Taskbar class methodsFor: 'class state access' stamp: 'jmv 5/3/2015 11:36'!
releaseClassState
	"Not just cached state but actual state. User preferences will be forgotten, and defaults will be assumed.
	Taskbar releaseClassState
	"
	Taskbar hideTaskbar! !


!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'jmv 5/3/2015 11:09'!
windowsMenu
        "Build the windows menu for the world."

	^ self fillIn: (self menu: 'Windows') from: {  
		{ 'Find Window' . { #myWorld . #findWindow: }. 'Presents a list of all windows; if you choose one from the list, it becomes the active window.'}.
		{ 'Find changed Browsers...' . { #myWorld . #findDirtyBrowsers: }. 'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.
		{ 'Find changed Windows...' . { #myWorld . #findDirtyWindows: }. 'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.
		nil.
		{ 'Find a Transcript' . { #myWorld . #findATranscript: }. 'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'}.
		{ 'Find a FileList' . { #myWorld . #findAFileList: }. 'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'}.
		{ 'Find a Change Sorter' . { #myWorld . #findAChangeSorter: }. 'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'}.
		{ 'Find Message Names' . { #myWorld . #findAMessageNamesWindow: }. 'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'}.
		nil.
		{ #staggerPolicyString . { self . #toggleWindowPolicy }. 'stagger: new windows positioned so you can see a portion of each one.
			tile: new windows positioned so that they do not overlap others, if possible.'}.
		nil.
		{ 'Collapse all Windows' . { #myWorld . #collapseAll }. 'Reduce all open windows to collapsed forms that only show titles.'}.
		{ 'Restore all Windows' . { #myWorld . #restoreAll }. 'Restore all collapsed windows back to their previous forms.'}.
		{ 'Close top Window' . { SystemWindow . #closeTopWindow }. 'Close the topmost window if possible.'}.
		{ 'Send top Window to back' . { SystemWindow . #sendTopWindowToBack  }. 'Make the topmost window become the backmost one, and activate the window just beneath it.'}.
		{ 'Move Windows onscreen' . { #myWorld . #bringWindowsFullOnscreen }. 'Make all windows fully visible on the screen'}.
		nil.
		{ 'Delete unchanged Windows' . { #myWorld . #closeUnchangedWindows }. 'Deletes all windows that do not have unsaved text edits.'}.
		{ 'Delete non Windows' . { #myWorld . #deleteNonWindows }. 'Deletes all non-window morphs lying on the world.'}.
		{ 'Delete Both of the Above' . { self . #cleanUpWorld }. 'deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'}.

	}! !


!Theme methodsFor: 'menus' stamp: 'jmv 5/3/2015 11:09'!
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
		#('expand / contract' 'show taskbar' 'restore all windows') -> #expandIcon.
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


!Theme class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 11:31'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	Taskbar hideTaskbar; showTaskbar.
	self runningWorld ifNotNil: [ :w |
		w restoreMorphicDisplay ].
	
	^ CurrentTheme! !

!methodRemoval: Taskbar class #initialize!
Taskbar class removeSelector: #initialize!
!methodRemoval: Taskbar class #reset!
Taskbar class removeSelector: #reset!
!methodRemoval: Taskbar class #screenSizeChanged!
Taskbar class removeSelector: #screenSizeChanged!
!methodRemoval: Taskbar class #startUp!
Taskbar class removeSelector: #startUp!
!methodRemoval: Taskbar class #visible!
Taskbar class removeSelector: #visible!
Taskbar initialize!

!Taskbar class reorganize!
('commands' clearSoleInstance hideTaskbar minimize: restore: restoreAll showTaskbar)
('testing' isCollapsed:)
('instance creation' singleton)
('class state access' releaseClassState)
!


!Taskbar reorganize!
('change reporting' delete)
('commands' minimize: restore: restoreAll)
('initialization' defaultColor defaultHeight defaultScale initialize intoWorld:)
('testing' is: isCollapsed: isSticky)
('private' buttonFor:)
('stepping' screenSizeChanged)
('resize' addCustomMenuItems:hand: normalScale resize scale scale: scaleNormal scaleX2 scaleX4)
!

!methodRemoval: PasteUpMorph #expandAll!
PasteUpMorph removeSelector: #expandAll!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Display removeActionsWithReceiver: Taskbar!
