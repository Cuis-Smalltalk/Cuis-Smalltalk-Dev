'From Cuis 5.0 of 7 November 2016 [latest update: #3285] on 14 April 2018 at 7:50:08 pm'!

!Theme methodsFor: 'menus - private' stamp: 'HAW 4/12/2018 15:53:55'!
basicIconsTypeSelector

	^#addBasicIconsTo:! !

!Theme methodsFor: 'menus - private' stamp: 'HAW 4/14/2018 19:47:32'!
iconDefinersFor: anIconTypeSelector

	^(Smalltalk allClassesImplementing: anIconTypeSelector) 
		select: [ :aClass | aClass isMeta ] 
		thenCollect: [ :aClass | aClass soleInstance ].
	! !

!Theme methodsFor: 'menus - private' stamp: 'HAW 4/14/2018 19:47:39'!
iconsDefinitionFor: anIconTypeSelector

	^(self iconDefinersFor: anIconTypeSelector) 
		inject: OrderedCollection new 
		into: [ :definition :iconDefiner | 
			iconDefiner perform: anIconTypeSelector with: definition.
			definition ]! !

!Theme methodsFor: 'menus - private' stamp: 'HAW 4/12/2018 16:00:19'!
miscellaneousIconsTypeSelector

	^#addMiscellaneousIconsTo:
! !


!Theme class methodsFor: 'icons by menu' stamp: 'HAW 4/14/2018 19:44:24'!
addBasicIconsTo: aCollectorCollection

	aCollectorCollection 		
		add: #('open...') -> #openIcon;
		add: #('windows...' 'find window' 'Focus follows mouse' 'Click to focus') -> #windowIcon;
		add: #('help...' 'explain' 'about this system...' 'Terse Guide to Cuis' 'Class Comment Browser' 'Code management in Cuis' 'Using GitHub to host Cuis packages' ) -> #helpIcon;
		add: #('themes...') -> #appearanceIcon;
		add: #('do it (d)') -> #doItIcon;
		add: #('new morph...' 'objects (o)' 'save world as morph file') -> #morphsIcon;
		add: #('save' ) -> #saveIcon;
		add: #('Save options...' 'save as...' 'change category...' 'rename change set (r)' 'rename') -> #saveAsIcon;
		add: #('save as new version') -> #saveAsNewVersionIcon;
		add: #('quit') -> #quitIcon;
		add: #('save and quit' ) -> #saveAndQuitIcon;
		add: #('inspect it (i)' 'inspect world'  'inspect model' 'inspect morph'
		 'inspect owner chain' 'inspect' 'inspect (i)' 'basic inspect' 'message names' 'find message names' 'inspect instances' 'inspect subinstances' 'inspect change set' 'inspect context (c)' 'inspect receiver (i)' 'start CPUWatcher' 'stop CPUWatcher')
			-> #inspectIcon;
		add: #('explore' 'explore it (I)' 'explore world' 'explore morph' 'explore (I)' 'explore context (C)' 'explore receiver (I)' 'references finder' 'weight explorer' 'explore CompiledMethod') -> #exploreIcon;
		add: #('find...(f)' 'find class... (f)' 'find method...' 'find recent submissions' 'show hierarchy' 'show definition' 'show comment' 'filter' 'filter message list...' 'find context... (f)') -> #findIcon;
		add: #('add item...' 'new category...' 'create new change set...' 'new change set... (n)' 'add new file') -> #newIcon;
		add: #('remove method (x)' 'remove' 'remove class (x)' 'remove method from system (x)' 'remove class from system (x)' 'remove postscript''stop StackSizeWatcher' ) -> #deleteIcon;
		add: #('delete method from changeset (d)' 'delete class from change set (d)' 'destroy change set (X)' 'revert & remove from changes' 'delete unchanged windows' 'delete non windows' 'delete both of the above' 'reset variables' 'remove contained in class categories...' 'clear this change set' 'uninstall this change set' 'delete directory...' 'delete' 'start StackSizeWatcher') -> #warningIcon;
		add: #('do again (j)' 'Redo - multiple (Z)') -> #redoIcon;
		add: #('undo (z)' 'revert to previous version' 'Undo - multiple (z)') -> #undoIcon;
		add: #('copy to clipboard (c)' 'copy (c)' 'copy class...' 'copy class chgs to other side' 'copy method to other side' 'copy all to other side (c)' 'copy name to clipboard' 'copy selector to clipboard') -> #copyIcon;
		add: #('paste (v)' 'Paste without Format') -> #pasteIcon;
		add: #('cut (x)' 'move class chgs to other side' 'move method to other side' 'submerge into other side') -> #cutIcon;
		add: #('paste...' 'icons...') -> #worldIcon
! !

!Theme class methodsFor: 'icons by menu' stamp: 'HAW 4/14/2018 19:42:09'!
addMiscellaneousIconsTo: aCollectorCollection

	aCollectorCollection 
		add: #('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon;
		add: #('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history' 'code file contents' 'package file contents') -> #changesIcon;
		add: #('Install New Updates' 'update' 'turn on auto-update (a)' 'update list (u)' 'install code' 'fileIn entire file' 'install package') -> #updateIcon;
		add: #('find again (g)' 'full stack (k)') -> #systemIcon;
		add: #('print it (p)' 'check change set for slips') -> #printIcon;
		add: #('accept (s)' 'make changes go to me (m)') -> #acceptIcon;
		add: #('cancel (l)' 'turn off auto-update (a)' 'stop StackSizeWatcher') -> #cancelIcon;
		add: #('debug...' 'Debug it (D)' 'toggle break on entry') -> #debugIcon;
		add: #('close' 'close all debuggers' 'close top window') -> #closeIcon;
		add: #('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon;
		add: #('expand / contract' 'show taskbar' 'restore all windows') -> #expandIcon;
		add: #('menu') -> #windowMenuIcon;
		add: #('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon;
		add: #('workspace' 'workspace with contents') -> #terminalIcon;
		add: #('styled text editor' 'text editor'  'basic text editor'  'unicode editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon;
		add: #('file list' 'find a fileList' 'compress file') -> #systemFileManagerIcon;
		add: #('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon;
		add: #('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon;
		add: #('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon;
		add: #('change sorter') -> #halfRefreshIcon;
		add: #('SUnit Test Runner' 'Accept & Run Test in Class (t)' 'Accept & Run Test in Category (y)' 'Run tests (t)' 'Run tests' 'Run test (t)' 'Accept & Debug Test (r)' 'Debug test (r)') -> #weatherFewCloudsIcon;
		add: #('Font Sizes...' 'system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon;
		add: #('full screen on') -> #viewFullscreenIcon;
		add: #('full screen off') -> #exitFullscreenIcon;
		add: #('set desktop color...') -> #wallpaperIcon;
		add: #('preferences...' 'All preferences...' 'what to show...' 'view as hex') -> #preferencesIcon;
		add: #('Editor keyboard shortcuts') -> #keyboardShortcutsIcon;
		add: #('world menu help') -> #globeIcon;		"currently unused, but a neat icon"
		add: #('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon;
		add: #('set code author...' 'check for other authors' 'check for any other authors') -> #usersIcon;
		add: #('space left') -> #removableMediaIcon;
		add: #('start drawing all again' 'window color...') -> #graphicsIcon;
		add: #('start stepping again') -> #mediaPlaybackStartIcon;
		add: #('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon;
		add: #('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'Profile it') -> #clockIcon;
		add: #('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon;
		add: #('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon;
		add: #('references to it (N)') -> #addressBookIcon;
		add: #('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon;
		add: #('inst var refs...' 'inst var defs...' 'sample instance') -> #instanceIcon;
		add: #('Use Selection for Find (j)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon;
		add: #('smalltalk options' 'method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon;
		add: #('method strings with it (E)' 'Toggle WordWrap') -> #genericTextIcon;
		add: #('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon;
		add: #('move up' 'make next-to-topmost') -> #goUpIcon;
		add: #('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon;
		add: #('inheritance (i)' 'move down') -> #goDownIcon;
		add: #('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon;
		add: #('spawn full protocol') -> #speadsheetTemplateIcon;
		add: #('alphabetize') -> #fontXGenericIcon;
		add: #('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon;
		add: #('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon;
		add: #('add to current change set' 'add all meths to current chgs' 'add preamble (p)' 'More...') -> #listAddIcon;
		add: #('toggle diffing (D)' 'toggle selections') -> #switchIcon;
		add: #('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon;
		add: #('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon;
		add: #('find changed windows...') -> #newWindowIcon;
		add: #('make undraggable') -> #pushPinIcon;
		add: #('Utilities saveScreenshot.') -> #stillCameraIcon;
		add: #('add new directory') -> #newFolderIcon;
		add: #('select all' 'deselect all') -> #selectAllIcon;
		add: #('sort by date') -> #dateIcon;
		add: #('justified') -> #formatJustifyFillIcon;
		add: #('centered') -> #formatJustifyCenterIcon;
		add: #('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon;
		add: #('rightFlush') -> #formatJustifyRightIcon;
		add: #('signal Semaphore (S)') -> #haloHelpIcon;
		add: #('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...' 'Set Default Font...') -> #fontXGenericIcon;
		add: #('Clear Font') -> #newIcon;
		add: #('code file browser' 'package file browser') -> #findIcon.
	! !


!Theme methodsFor: 'menus' stamp: 'HAW 4/12/2018 15:56:21'!
basicIcons

	"Minimal menu scheme.
	Theme current class beCurrent
	"

	^self iconsDefinitionFor: self basicIconsTypeSelector 
! !

!Theme methodsFor: 'menus' stamp: 'HAW 4/12/2018 16:00:06'!
miscellaneousIcons
	
	^self iconsDefinitionFor: self miscellaneousIconsTypeSelector
! !


!Theme reorganize!
('colors' background buttonColorFrom: buttonLabel errorColor failureColor focusIndicator line missingCommentTextColor paneBackgroundFrom: scrollbarButtonColor scrollbarColor scrollbarSliderShadowColor successColor text textCursor textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('menu colors' menu menuHighlight menuText menuTitleBar)
('tool colors' browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet object packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton listHighlightFocused: listMultiHighlightFocused: listSelectedRowText listUnselectedRowText textPane)
('other options' buttonGradientBottomFactor buttonGradientHeight buttonGradientTopFactor buttonPaneHeight embossedButtonLabels embossedTitles fullScreenDeskMargin layoutAdjusterThickness minimalWindows roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius scrollbarThickness steButtons titleGradientBottomFactor titleGradientExtraLightness titleGradientTopFactor useButtonGradient useTaskbar useUniformColors useWindowTitleGradient)
('private - shout mappings' ansiAssignment argumentTypes assignment blockLevelFive blockLevelFour blockLevelSeven blockLevelSix blockTemps defaults firstBlockLevel generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags pattern pseudoVariables return symbols tempBar tempVars undefined)
('menus - private' basicIconsTypeSelector iconDefinersFor: iconsDefinitionFor: miscellaneousIconsTypeSelector)
('menus' allIcons basicIcons menuDecorations miscellaneousIcons noIcons)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon helpIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon removableMediaIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('accessing' decorateMenu:)
('shout' shout)
('initialization' initialize)
('keyboard shortcuts' keyStroke:morph:)
('as yet unclassified' windowClosed: windowOpen:)
('*Refactoring')
!


!Theme class reorganize!
('instance creation' beCurrent)
('accessing' content content: current)
('importing/exporting' bootstrap)
('class initialization' currentTheme:)
('user interface' changeFontSizes changeIcons changeTheme)
('icons by menu' addBasicIconsTo: addMiscellaneousIconsTo:)
!

