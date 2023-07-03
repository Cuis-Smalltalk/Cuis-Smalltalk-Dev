'From Cuis 6.0 [latest update: #5889] on 3 July 2023 at 3:40:46 pm'!

!FileList methodsFor: 'own services' stamp: 'jmv 7/3/2023 15:26:28'!
serviceAddNewDirectory
	"Answer a service entry characterizing the 'add new directory' command"

	self flag: #assumesKeyboardShortcuts.
	^ SimpleServiceEntry
		provider: self
		label: 'add new directory (N)'
		selector: #addNewDirectory
		description: 'adds a new, empty directory (folder)'
		icon: #listAddIcon! !

!FileList methodsFor: 'own services' stamp: 'jmv 7/3/2023 15:26:31'!
serviceAddNewFile
	"Answer a service entry characterizing the 'add new file' command"

	self flag: #assumesKeyboardShortcuts.
	^ SimpleServiceEntry 
		provider: self 
		label: 'add new file (n)' 
		selector: #addNewFile 
		description: 'create a new,. empty file, and add it to the current directory.'
		icon: #newIcon! !

!FileList methodsFor: 'own services' stamp: 'jmv 7/3/2023 15:26:34'!
serviceDeleteFile

	self flag: #assumesKeyboardShortcuts.
	^ SimpleServiceEntry
		provider: self
		label: 'delete (x)'
		selector: #deleteFile
		description: 'delete the seleted item'
		icon: #deleteIcon! !

!FileList methodsFor: 'own services' stamp: 'jmv 7/3/2023 15:26:37'!
serviceRenameFile

	self flag: #assumesKeyboardShortcuts.
	^ SimpleServiceEntry
		provider: self
		label: 'rename (R)'
		selector: #renameFile
		description: 'rename file'
		icon: #saveAsIcon! !


!TextEditor methodsFor: 'menu' stamp: 'jmv 7/3/2023 12:05:41'!
addCutAndPasteMenuSectionTo: aMenu
	"Adds  typical cut and paste operations section to a menu"

	self flag: #assumesKeyboardShortcuts.
	self hasUnacceptedEdits ifTrue: [
		aMenu
			add: 'Accept (s)'
			action: #acceptContents
			icon: #acceptIcon
	].
	
	aMenu
		add: 'Copy (c)'
		action: #copySelection
		icon: #copyIcon.
	
	aMenu
		add: 'Cut (x)'
		action: #cut
		icon: #cutIcon.

	aMenu
		add: 'Paste (v)'
		action: #paste
		icon: #pasteIcon.
		
	aMenu
		add: 'Paste without Format'
		action: #pasteString
		icon: #pasteIcon.
	
	aMenu
		add: 'Paste...'
		action: #pasteRecent
		icon: #worldIcon.
	
	^aMenu! !

!TextEditor methodsFor: 'menu' stamp: 'jmv 7/3/2023 12:05:36'!
addFinderMenuSectionTo: aMenu
	"Build a submenu with finding related operations"

	self flag: #assumesKeyboardShortcuts.
	aMenu addItemsFromDictionaries:
	 `{
			{
				#label 			-> 		'Find...(f)'.
				#selector 			-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'Find Again (g)'.
				#selector 			-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 			-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		}`.
		^aMenu! !

!TextEditor methodsFor: 'menu' stamp: 'jmv 7/3/2023 12:05:56'!
addStyleMenuSectionTo: aMenu
	"Adds to the given menu text styiling related operations"

	self flag: #assumesKeyboardShortcuts.
	aMenu
		addItemsFromDictionaries: 
	`{
		{
				#label 			-> 		'Toggle WordWrap'.
				#selector 			-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 			-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Clear Font'.
				#selector 			-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font Style'.
				#selector 			-> 		#offerFontStyleMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Set Text Color'.
				#selector 			-> 		#offerColorMenu.
				#icon 			-> 		#graphicsIcon
			} asDictionary.
			{
				#label 			-> 		'Set Default Font...'.
				#selector 			-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'Set Alignment...'.
				#selector 			-> 		#chooseAlignment.
				#icon 			-> 		#formatJustifyLeftIcon
			} asDictionary.
		}`.
		^aMenu! !

!TextEditor methodsFor: 'menu' stamp: 'jmv 7/3/2023 12:06:24'!
addUndoMenuSectionTo: aMenu
	"Adds undo operations to the given menu"

	self flag: #assumesKeyboardShortcuts.
	aMenu
		addItemsFromDictionaries: 
		`{	
			nil.
			{
				#label 			-> 		'Undo - multiple (z)'.
				#selector 			-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 			-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#label 			-> 		'Undo / Redo history'.
				#selector 			-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
		}`.
		
	^aMenu.! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 7/3/2023 12:02:44'!
basicCmdShortcutsSpec
	"
	Editor initialize
	"

	"arranged in QWERTY keyboard order"
	self flag: #definesKeyboardShortcuts.
	^#(
		#(		$( 	#enclose:					'Enclose within ( and ), or remove enclosing ( and )')
		#(		$[ 	#enclose:					'Enclose within [ and ], or remove enclosing [ and ]')
		#(		${ 	#enclose:					'Enclose within { and }, or remove enclosing { and }')
		#(		$' 	#enclose:					'Enclose within single quotes, or remove enclosing single quotes')
		#(		$" 	#enclose:					'Enclose within double quotes, or remove enclosing double quotes')
		#(		$` 	#enclose:					'Enclose within backticks, or remove enclosing backticks')
		#(		$< 	#enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:					'Select all')
		#(		$s 	#save:					'Save')
		#(		$f 	#find:					'Find')
		#(		$g 	#findAgain:					'Find again')
		#(		$h 	#help:					'Open this help')
		#(		$j 	#setSearchString:					'Set selection as search string for find again')

		#(		$z 	#undo:					'Undo (multiple levels)')
		#(		$x 	#cut:					'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:					'Copy selection to the Clipboard')
		#(		$v 	#paste:					'Paste Clipboard contents')

		#(		$Y	#makeUppercase:					'Force selection to uppercase')
		#(		$U	#changeLineEndsToLf:		'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H	#cursorTopHome:					'Move cursor to start of text')

		#(		$Z	#redo:					'Redo (multiple levels)')
		#(		$X	#makeLowercase:					'Force selection to lowercase')
		#(		$C	#compareToClipboard:		'Compare argument to clipboard')
	)! !

!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 7/3/2023 12:03:25'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	self flag: #definesKeyboardShortcuts.
	^#(
		#(		$0 	changeEmphasis:			'Normal')
		#(		$1 	changeEmphasis:			'Bold')
		#(		$2 	changeEmphasis:			'Italic')
		#(		$3 	changeEmphasis:			'Underline')
		#(		$4 	changeEmphasis:			'Strikeout')
		#(		$5 	changeEmphasis:			'Superscript')
		#(		$6 	changeEmphasis:			'Subscript')
		#(		$8	#offerColorMenu:		'Change color')

		#(		$u	#align:				'Toggle alignment')

		#(		$k	#offerFontMenu:			'Set font')
	)! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 7/3/2023 12:03:06'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	self flag: #definesKeyboardShortcuts.
	^#(
		#(		$0 	changeEmphasis:							'Normal')
		#(		$1 	changeEmphasis:							'Bold')
		#(		$2 	changeEmphasis:							'Italic')
		#(		$3 	changeEmphasis:							'Underline')
		#(		$4 	changeEmphasis:							'Strikeout')
		#(		$5 	changeEmphasis:							'Superscript')
		#(		$6 	changeEmphasis:							'Subscript')
		#(		$8	#offerColorMenu:							'Change color')
	
		#(		$e	#methodSourceContainingIt:							'Method source containing it')
		#(		$r	#acceptAndDebugTest:							'Saves method and debugs it')
		#(		$t	#acceptAndTest:							'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:							'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$u	#align:							'Toggle alignment')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:							'Print it (selection is a valid expression)')

		#(		$d	#doIt:							'Do it (selection is a valid expression)')
		#(		$k	#offerFontMenu:							'Set font')
		#(		$l	#cancel:							'Cancel all edits')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$W	#argNext:							'Next argument')
		#(		$E	#methodStringsContainingIt:							'Method strings containing it')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$D	#debugIt:							'Debug it')

		#(		$N	#referencesToIt:							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 7/3/2023 12:24:04'!
smalltalkEditorEditingMenuOptions
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Find...(f)'.
				#selector 			-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Find Again (g)'.
				#selector 			-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 			-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Undo - multiple (z)'.
				#selector 			-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 			-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'Undo / Redo history'.
				#selector 			-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Copy (c)'.
				#selector 			-> 		#copySelection.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Cut (x)'.
				#selector 			-> 		#cut.
				#icon 			-> 		#cutIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'Paste (v)'.
				#selector 			-> 		#paste.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		40.
				#label 			-> 		'Paste without Format'.
				#selector 			-> 		#pasteString.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		50.
				#label 			-> 		'Paste...'.
				#selector 			-> 		#pasteRecent.
				#icon 			-> 		#worldIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Toggle WordWrap'.
				#selector 			-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Set Font... (k)'.
				#selector 			-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'Clear Font'.
				#selector 			-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'Set Default Font...'.
				#selector 			-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
		}`
! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 7/3/2023 12:25:48'!
smalltalkEditorMenuOptions
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Help...'.
				#selector 			-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Do it (d)'.
				#selector 			-> 		#doIt.
				#icon 			-> 		#doItIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Print it (p)'.
				#selector 			-> 		#printIt.
				#icon 			-> 		#printIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'Inspect it (i)'.
				#selector 			-> 		#inspectIt.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'Explore it (I)'.
				#selector 			-> 		#exploreIt.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		50.
				#label 			-> 		'Debug it (D)'.
				#selector 			-> 		#debugIt.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		60.
				#label 			-> 		'Profile it'.
				#selector 			-> 		#profileIt.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		5.
				#label 			-> 	SmalltalkEditor editMenuLabel.
				#selector 			-> 		#yourself.
				#icon 			-> 		#textEditorIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Explain'.
				#selector 			-> 		#explain.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Browse it (b)'.
				#selector 			-> 		#browseIt.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'Senders of it (n)'.
				#selector 			-> 		#sendersOfIt.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		40.
				#label 			-> 		'Implementors of it (m)'.
				#selector 			-> 		#implementorsOfIt.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		50.
				#label 			-> 		'References to it (N)'.
				#selector 			-> 		#referencesToIt.
				#icon 			-> 		#addressBookIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Method Strings with it (E)'.
				#selector 			-> 		#methodStringsContainingIt.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Method Source with it (e)'.
				#selector 			-> 		#methodSourceContainingIt.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'Class Comments with it'.
				#selector 			-> 		#classCommentsContainingIt.
				#icon 			-> 		#chatIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'Accept (s)'.
				#selector 			-> 		#acceptContents.
				#icon 			-> 		#acceptIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'Accept & Run Test in Class (t)'.
				#selector 			-> 		#acceptAndTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		40.
				#label 			-> 		'Accept & Run Test in Category (y)'.
				#selector 			-> 		#acceptAndTestAll.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		50.
				#label 			-> 		'Accept & Debug Test (r)'.
				#selector 			-> 		#acceptAndDebugTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}, SmalltalkEditor smalltalkEditorEditingMenuOptions `! !


!Morph methodsFor: 'debug and other' stamp: 'jmv 7/3/2023 15:22:19'!
buildDebugMenu: aHand 
	"Answer a debugging menu for the receiver.  The hand argument is seemingly historical and plays no role presently"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addStayUpIcons.
	self isKnownFailing
		ifTrue: [
			aMenu add: 'start drawing again' action: #resumeAfterDrawError.
			aMenu addLine].
	(self hasProperty: #errorOnStep) 
		ifTrue: [
			aMenu add: 'start stepping again' action: #resumeAfterStepError.
			aMenu addLine].
	aMenu add: 'inspect morph' action: #inspect.
	aMenu add: 'inspect owner chain' action: #inspectOwnerChain.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'inspect model'
				target: self model
				action: #inspect].
	aMenu 
		add: 'explore morph'
		target: self
		action: #explore.
	aMenu 
		add: 'copy to clipboard (c)'
		target: self
		action: #copyToClipboard.
	aMenu addLine.
	aMenu 
		add: 'browse morph class'
		target: self
		action: #browseClassHierarchy.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'browse model class'
				target: self model
				action: #browseClassHierarchy].
	aMenu addLine.
	aMenu
		add: 'edit balloon help' action: #editBalloonHelpText.
	^aMenu! !

!Morph methodsFor: 'menus' stamp: 'jmv 7/3/2023 15:22:24'!
addCopyItemsTo: aMenu 
	"Add copy-like items to the halo menu"

	self flag: #assumesKeyboardShortcuts.
	aMenu add: 'copy to clipboard (c)' action: #copyToClipboard:! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 7/3/2023 12:12:57'!
keyStroke: aKeyboardEvent
	self flag: #definesKeyboardShortcuts.
	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ BrowserWindow openBrowser ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MessageSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
	^ super keyStroke: aKeyboardEvent! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 7/3/2023 12:26:09'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 			-> 		[  scrollBar scrollUp: 1 ].
			[ #down ] 			-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 			-> 		[  hScrollBar scrollUp: 1 ].
			[ #right ] 			-> 		[  hScrollBar scrollDown: 1 ] }! !


!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:13:31'!
classListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of classes and a 
	code pane, and I also have a listView that has a list of methods.  The 
	view knows how to get the list and selection."

	self flag: #definesKeyboardShortcuts.
	aChar = $r ifTrue: [^ model recent].
	aChar = $h ifTrue: [^ self browseHierarchy].
	aChar = $x ifTrue: [^ model removeClass].
	aChar = $t ifTrue: [^ model runClassTests ].
	
	^ self messageListKey: aChar from: view! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:14:10'!
messageCatListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $o ifTrue: [ ^ model fileOutMessageCategories ].
	aChar = $t ifTrue: [ ^ model runMessageCategoryTests ].
	aChar = $x ifTrue: [ ^ model removeMessageCategory ].
	aChar = $R ifTrue: [ ^ model renameCategory ].
	aChar = $n ifTrue: [ ^ model addCategory ].
	aChar = $N ifTrue: [ ^ model addExtensionCategory ].
	aChar = $e ifTrue: [ ^ model removeEmptyCategories ].
	aChar = $c ifTrue: [ ^ model categorizeAllUncategorizedMethods ].
	aChar = $a ifTrue: [ ^ model alphabetizeMessageCategories ].
	aChar = $r ifTrue: [ ^ model editMessageCategories ]! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:11:20'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	self flag: #definesKeyboardShortcuts.
	aChar = $D ifTrue: [^ model toggleDiffing].

	sel := model selectedMessageName.
	aChar = $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar = $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class := model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar = $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar = $N ifTrue: [^ self browseClassRefs].
	aChar = $i ifTrue: [^ self methodInheritance].
	aChar = $h ifTrue: [^ self browseHierarchy].
	aChar = $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $o ifTrue: [^ model fileOutMessage].
			aChar = $c ifTrue: [^ model copySelector].
			aChar = $v ifTrue: [^ self browseVersions].
			aChar = $C ifTrue: [^ model showHomeCategory].
			aChar = $O ifTrue: [^ self openSingleMessageBrowser].
			aChar = $x ifTrue: [^ model removeMessage].
			aChar = $t ifTrue: [^ model runMethodTest].
			aChar = $r ifTrue: [^ model debugMethodTest]]
		ifNil: [
			aChar = $R ifTrue: [^ model renameClass]]! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:20:30'!
systemCatListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also have a listView that has a list of methods.  The view knows how to get the list and selection."

	self flag: #definesKeyboardShortcuts.
	aChar = $f ifTrue: [ ^ self findClass ].
	aChar = $x ifTrue: [ ^ model removeSystemCategory ].
	aChar = $t ifTrue: [ ^ model runSystemCategoryTests ].
	aChar = $a ifTrue: [ ^ model addSystemCategory ].
	aChar = $A ifTrue: [ ^ model alphabetizeSystemCategories ].
	aChar = $b ifTrue: [ ^ self openSystemCategoryBrowser ].
	aChar = $B ifTrue: [ ^ self browseAllClasses ].
	aChar = $o ifTrue: [ ^ model fileOutSystemCategory ].
	aChar = $u ifTrue: [ ^ model updateSystemCategories ].
	aChar = $R ifTrue: [ ^ model renameSystemCategory ].
	
	^ self classListKey: aChar from: view.! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:03:30'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	self flag: #assumesKeyboardShortcuts.
	^DynamicMenuBuilder 
		buildTitled: 'Message List' 
		targeting: self 
		collectingMenuOptionsWith: #messageListMenu2Options 
		changingThemWith: [ :options |
			self addExtraMenu2ItemsTo: options.
			model canShowMultipleMessageCategories ifTrue: [	
				options add: `{ 
					#itemGroup 			-> 		40.
					#itemOrder 			-> 		45.		
					#label 			-> 		'show category (C)'.
					#object 			-> 		#model.
					#selector 			-> 		#showHomeCategory.
					#icon 			-> 		#packageIcon
				} asDictionary` ]].
		
	! !

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:11:25'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel |
	self flag: #definesKeyboardShortcuts.
	sel := model selectedMessageName.
	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $c ifTrue: [ ^ self browseCategoryOfCurrentMethod ].
			aChar = $R ifTrue: [^ self renameSelector].
			aChar = $U ifTrue: [^ self addParameter ].
			aChar = $I ifTrue: [^ self removeParameter ].
			aChar = $3 ifTrue: [^ self inlineMethod ]].
	super messageListKey: aChar from: view! !

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:20:26'!
systemCatListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $r ifTrue: [^ model recent ].
	
	^super systemCatListKey: aChar from: view.! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 12:29:20'!
classListMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^ `{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		5.
				#label 			-> 		'show classes hierarchically'.
				#object 			-> 		#model.
				#selector 			-> 		#enableListClassesHierarchically.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		6.
				#label 			-> 		'show classes alphabetically'.
				#object 			-> 		#model.
				#selector 			-> 		#enableListClassesAlphabetically.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'show definition'.
				#object 			-> 		#model.
				#selector 			-> 		#editClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'show comment'.
				#object 			-> 		#model.
				#selector 			-> 		#editComment.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 			-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 			-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'class var refs...'.
				#selector 			-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'class vars'.
				#selector 			-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'class refs (N)'.
				#selector 			-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'rename class... (R)'.
				#object 			-> 		#model.
				#selector 			-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'copy class...'.
				#object 			-> 		#model.
				#selector 			-> 		#copyClass.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove class (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runClassTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		20.
				#label 			-> 		BrowserWindow moreMenuLabel.
				#selector 			-> 		#yourself.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}, BrowserWindow classListMoreMenuOptions`.! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 14:43:07'!
classListMoreMenuOptions

	^ `{			
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		10.
				#label 				-> 		'unsent methods'.
				#selector 				-> 		#browseUnusedMethods.
				#icon 				->		#junkIcon.
				#balloonText 				-> 		'browse all methods defined by this class that have no senders'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		20.
				#label 				-> 		'unreferenced inst vars'.
				#selector 				-> 		#showUnreferencedInstVars.
				#icon 				->		#junkIcon.
				#balloonText 				-> 		'show a list of all instance variables that are not referenced in methods'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		30.
				#label 				-> 		'unreferenced class vars'.
				#selector 				-> 		#showUnreferencedClassVars.
				#icon 				->		#junkIcon.
				#balloonText 				-> 		'show a list of all class variables that are not referenced in methods'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		40.
				#label 				-> 		'subclass template'.
				#object 				-> 		#model.
				#selector 				-> 		#makeNewSubclass.
				#icon 				->		#classIcon.
				#balloonText 				-> 		'put a template into the code pane for defining of a subclass of this class'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		10.
				#label 				-> 		'sample instance'.
				#selector 				-> 		#makeSampleInstance.
				#icon 				->		#instanceIcon.
				#balloonText 				-> 		'give me a sample instance of this class, if possible'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		20.
				#label 				-> 		'inspect instances'.
				#selector 				-> 		#inspectInstances.
				#icon 				->		#inspectIcon.
				#balloonText 				-> 		'open an inspector on all the extant instances of this class'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		30.
				#label 				-> 		'inspect subinstances'.
				#selector 				-> 		#inspectSubInstances.
				#icon 				->		#inspectIcon.
				#balloonText 				-> 		'open an inspector on all the extant instances of this class and of all of its subclasses'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		30.
				#itemOrder 				-> 		10.
				#label 				-> 		'create inst var accessors'.
				#object 				-> 		#model.
				#selector 				-> 		#createInstVarAccessors.
				#icon 				->		#sendReceiveIcon.
				#balloonText 				-> 		'compile instance-variable access methods for any instance variables that do not yet have them'
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 14:49:55'!
messageCategoryMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut (o)'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutMessageCategories.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'reorganize (r)'.
				#object 			-> 		#model.
				#selector 			-> 		#editMessageCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'alphabetize (a)'.
				#object 			-> 		#model.
				#selector 			-> 		#alphabetizeMessageCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove empty categories (e)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeEmptyCategories.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'categorize all uncategorized (c)'.
				#object 			-> 		#model.
				#selector 			-> 		#categorizeAllUncategorizedMethods.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		50.
				#label 			-> 		'new category... (n)'.
				#object 			-> 		#model.
				#selector 			-> 		#addCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		60.
				#label 			-> 		'new extension category... (N)'.
				#object 			-> 		#model.
				#selector 			-> 		#addExtensionCategory.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'rename... (R)'.
				#object 			-> 		#model.
				#selector 			-> 		#renameCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'remove (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessageCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runMessageCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 15:03:40'!
messageListMenu2Options
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'implementors of sent messages'.
				#selector 		-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'local senders of...'.
				#selector 		-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'local implementors of...'.
				#selector 		-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'spawn sub-protocol'.
				#selector 		-> 		#browseProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'spawn full protocol'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#speadsheetTemplateIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'change category...'.
				#object 			-> 		#model.
				#selector 		-> 		#changeCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		50.
				#label 			-> 		'change sets with this method'.
				#selector 		-> 		#findMethodInChangeSets.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		60.
				#label 			-> 		'revert to previous version'.
				#object 			-> 		#model.
				#selector 		-> 		#revertToPreviousVersion.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'more...'.
				#selector 		-> 		#openMessageListMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.			
		}`! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 14:51:18'!
messageListMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'what to show...'.
				#selector 			-> 		#offerWhatToShowMenu.
				#icon 			-> 		#preferencesIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleBreakOnEntry.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'browse method (O)'.
				#selector 			-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{	#itemGroup  			-> 		20.
				#itemOrder  			-> 		50.
				#label            			-> 		'browse method category (c)'.
				#selector       		-> 			#browseCategoryOfCurrentMethod.
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'inspect CompiledMethod'.
				#object 			-> 		#model.
				#selector 			-> 		#inspectCompiledMethod.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'senders of... (n)'.
				#selector 			-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'implementors of... (m)'.
				#selector 			-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'inheritance (i)'.
				#selector 			-> 		#methodInheritance.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'versions (v)'.
				#selector 			-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 			-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 			-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'class var refs...'.
				#selector 			-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		40.
				#label 			-> 		'class variables'.
				#selector 			-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		50.
				#label 			-> 		'class refs (N)'.
				#selector 			-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'debug test (r)'.
				#object 			-> 		#model.
				#selector 			-> 		#debugMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		10.
				#label 			-> 		'more...'.
				#selector 			-> 		#openMessageListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 14:58:56'!
systemCatSingletonMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'find class... (f)'.
				#selector 			-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse all (B)'.
				#selector 			-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse (b)'.
				#selector 			-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut (o)'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'update (u)'.
				#object 			-> 		#model.
				#selector 			-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'rename... (R)'.
				#object 			-> 		#model.
				#selector 			-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeSystemCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 14:58:21'!
systemCategoryMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'find class... (f)'.
				#selector 			-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'recent classes... (r)'.
				#object 			-> 		#model.
				#selector 			-> 		#recent.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse all (B)'.
				#selector 			-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse (b)'.
				#selector 			-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut (o)'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'reorganize'.
				#object 			-> 		#model.
				#selector 			-> 		#editSystemCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'alphabetize (A)'.
				#object 			-> 		#model.
				#selector 			-> 		#alphabetizeSystemCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'update (u)'.
				#object 			-> 		#model.
				#selector 			-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'add item... (a)'.
				#object 			-> 		#model.
				#selector 			-> 		#addSystemCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'rename... (R)'.
				#object 			-> 		#model.
				#selector 			-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		35.
				#label 			-> 		'move to... (m)'.
				#object 			-> 		#model.
				#selector 			-> 		#moveAllToOtherSystemCategory.
				#icon 			-> 		#saveAsIcon.
				#balloonText	-> 				'Move all classes in this category to another category'       
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		40.
				#label 			-> 		'remove (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeSystemCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'move to top'.
				#object 			-> 		#model.
				#selector 			-> 		#moveSystemCategoryTop.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'move up'.
				#object 			-> 		#model.
				#selector 			-> 		#moveSystemCategoryUp.
				#icon 			-> 		#goUpIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'move down'.
				#object 			-> 		#model.
				#selector 			-> 		#moveSystemCategoryDown.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		40.
				#label 			-> 		'move to bottom'.
				#object 			-> 		#model.
				#selector 			-> 		#moveSystemCategoryBottom.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runSystemCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	! !


!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:04:50'!
classListMenu

	| aMenu itemColl |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	itemColl := OrderedCollection new.
	itemColl addAll:
		{
			{#label -> 'definition'. #object -> #model. #selector -> #editClass. #icon -> #editFindReplaceIcon} asDictionary. 
			{#label -> 'comment'. #object -> #model. #selector -> #editComment. #icon -> #editFindReplaceIcon} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl addAll:
			{
				{#label -> 'browse full (b)'. #selector -> #browseMethodFull. #icon -> #editFindReplaceIcon} asDictionary. 
				{#label -> 'class refs (N)'. #selector -> #browseClassRefs. #icon -> #classIcon} asDictionary. 
				nil. 
				{#label -> 'fileIn'. #object -> #model. #selector -> #fileInClass. #icon -> #updateIcon} asDictionary
			} ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #object -> #model. #selector -> #fileOutClass. #icon -> #fileOutIcon} asDictionary. 
			nil. 
			{#label -> 'rename...'. #object -> #model. #selector -> #renameClass. #icon -> #saveAsIcon} asDictionary. 
			{#label -> 'remove'. #object -> #model. #selector -> #removeClass. #icon -> #listRemoveIcon} asDictionary. 
			nil. 
			{#label -> 'remove existing'. #object -> #model. #selector -> #removeUnmodifiedCategories. #icon -> #deleteIcon} asDictionary
		}.
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:05:05'!
codeFileListMenu
	| aMenu itemColl |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Code File'.
	itemColl := OrderedCollection new.
	itemColl addAll:
		{
			{#label -> 'find class... (f)'. #selector -> #findClass} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #object -> #model. #selector -> #fileIn} asDictionary ].
	itemColl add:
		{#label -> 'fileOut'. #object -> #model. #selector -> #fileOut} asDictionary.
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		itemColl add:
			{#label -> 'remove existing'. #object -> #model. #selector -> #removeUnmodifiedClasses} asDictionary ].
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:05:23'!
messageListMenu
	| aMenu itemColl |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message List'.
	itemColl := OrderedCollection new.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #object -> #model. #selector -> #fileInMessage. #icon -> #updateIcon} asDictionary ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #object -> #model. #selector -> #fileOutMessage. #icon -> #fileOutIcon} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl addAll:
			{
				{#label -> 'senders (n)'. #selector -> #browseSenders. #icon -> #mailForwardIcon} asDictionary. 
				{#label -> 'implementors (m)'. #selector -> #browseImplementors. #icon -> #developmentIcon} asDictionary. 
				{#label -> 'method inheritance (h)'. #selector -> #methodInheritance. #icon -> #goDownIcon} asDictionary. 
				{#label -> 'versions (v)'. #selector -> #browseVersions. #icon -> #clockIcon} asDictionary
			} ].
	itemColl addAll:
		{
			nil. 
			{#label -> 'remove method (x)'. #object -> #model. #selector -> #removeMessage. #icon -> #deleteIcon} asDictionary
		}.
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!CodeFileBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:13:56'!
classListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $b ifTrue: [^ self browseMethodFull].
	aChar = $N ifTrue: [^ self browseClassRefs].
	self codeFileListKey: aChar from: view! !

!CodeFileBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:14:39'!
codeFileListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $f ifTrue: [^ self findClass]! !

!CodeFileBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:14:45'!
messageListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $b ifTrue: [^ self browseMethodFull].
	super messageListKey: aChar from: view! !


!MessageNamesWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:21:54'!
selectorListMenu
	"Answer the menu associated with the selectorList"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	(aMenu add: 'senders (n)' action: #browseSenders icon: #mailForwardIcon)
		setBalloonText: 'browse senders of the chosen selector'.
	^ aMenu! !

!MessageNamesWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:15:08'!
selectorListKey: aChar from: view
	"Respond to a Command key in the message-list pane."

	self flag: #definesKeyboardShortcuts.
	aChar = $n ifTrue: [ ^ self browseSenders ].
	aChar = $b ifTrue: [ ^ self browseMethodFull ].
	aChar = $m ifTrue: [ ^ self browseImplementors ].! !


!ProtocolBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:05:38'!
protocolClassListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.

		}`.
	^ aMenu! !

!ProtocolBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:15:14'!
protocolClassListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of 
	classes and a list of methods.  The 
	view knows how to get the list and selection."

	self flag: #definesKeyboardShortcuts.
	aChar = $b ifTrue: [^ self browseMethodFull ].
	aChar = $h ifTrue: [^ self browseHierarchy].
	
	^ nil! !


!ChangeListWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:12:53'!
listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Change List'.
	aMenu addStayUpIcons.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileIn selections'.
				#object 			-> 		#model.
				#selector 		-> 		#fileInSelections.
				#balloonText 	-> 		'import the selected items into the image'.
			} asDictionary.
			{
				#label 			-> 		'fileOut selections...	'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSelections.
				#balloonText 	-> 		'create a new file containing the selected items'.
			} asDictionary.
			{
				#label 			-> 		'fileOut current version of selections...'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutCurrentVersionsOfSelections.
				#balloonText 	-> 		'create a new file containing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'compare to current'.
				#object 			-> 		#model.
				#selector 		-> 		#compareToCurrentVersion.
				#balloonText 	-> 		'open a separate window which shows the text differences between the on-file version and the in-image version.'.
			} asDictionary.
			{
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#balloonText 	-> 		'start or stop showing diffs in the code pane.'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select new methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectNewMethods.
				#balloonText 	-> 		'select methods in the file that do not currently exist in the image'.
			} asDictionary.
			{
				#label 			-> 		'select changes for absent classes'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForAbsentClasses.
				#balloonText 	-> 		'select methods in the file for classes that are not defined in the image'.
			} asDictionary.
			{
				#label 			-> 		'select all changes for this class'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForThisClass.
				#balloonText 	-> 		'select all methods in the file that belong to the currently-selected class'.
			} asDictionary.
			{
				#label 			-> 		'select unchanged methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectUnchangedMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions are the same as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods equivalent to current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectEquivalentMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods older than current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectMethodsOlderThanCurrent.
				#balloonText 	-> 		'select methods in the file that are older than the one currently in the image'.
			} asDictionary.
			{
				#label 			-> 		'select removals of sent methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectRemovalsOfSent.
				#balloonText 	-> 		'select all method removals of methods that have some sender in the image'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select all (a)'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAll.
				#balloonText 	-> 		'select all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'deselect all (A)'.
				#object 			-> 		#model.
				#selector 		-> 		#deselectAll.
				#balloonText 	-> 		'deselect all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'invert selections'.
				#object 			-> 		#model.
				#selector 		-> 		#invertSelections.
				#balloonText 	-> 		'select every item that is not currently selected, and deselect every item that *is* currently selected'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse class and method'.
				#selector 		-> 		#browseMethodFull.
				#balloonText 	-> 		'open a full browser showing the selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse all versions of single selection'.
				#selector 		-> 		#browseVersions.
				#balloonText 	-> 		'open a version browser showing the versions of the currently selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse current versions of selections'.
				#selector 		-> 		#browseCurrentVersionsOfSelections.
				#balloonText 	-> 		'open a message-list browser showing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			{
				#label 			-> 		'destroy current methods of selections'.
				#object 			-> 		#model.
				#selector 		-> 		#destroyCurrentCodeOfSelections.
				#balloonText 	-> 		'remove (*destroy*) the in-image counterparts of all selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove doIts'.
				#object 			-> 		#model.
				#selector 		-> 		#removeDoIts.
				#balloonText 	-> 		'remove all items that are doIts rather than definitions'.
			} asDictionary.
			{
				#label 			-> 		'remove older versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeOlderMethodVersions.
				#balloonText 	-> 		'remove all but the most recent versions of methods in the list'.
			} asDictionary.
			{
				#label 			-> 		'remove up-to-date versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeUpToDate.
				#balloonText 	-> 		'remove all items whose code is the same as the counterpart in-image code'.
			} asDictionary.
			{
				#label 			-> 		'remove empty class comments'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyClassComments.
				#balloonText 	-> 		'remove all empty class comments'.
			} asDictionary.
			{
				#label 			-> 		'remove selected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSelections.
				#balloonText 	-> 		'remove the selected items from the change-list'.
			} asDictionary.
			{
				#label 			-> 		'remove unselected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeNonSelections.
				#balloonText 	-> 		'remove all the items not currently selected from the change-list'.
			} asDictionary.
		}`.
	^ aMenu! !

!ChangeListWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:15:21'!
changeListKey: aChar from: view
	"Respond to a Command key in the list pane."

	self flag: #definesKeyboardShortcuts.
	aChar = $D ifTrue: [^ model toggleDiffing].
	aChar = $a ifTrue: [^ model selectAll].
	aChar = $A ifTrue: [^ model deselectAll]! !


!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:05:50'!
classCommentVersionsMenu
	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: model.				"all commands are implemented by the model, not the view"
	aMenu addTitle: 'versions'.
	aMenu addStayUpIcons.
	aMenu addItemsFromDictionaries: `{
		{
			#label 			-> 			'compare to current'.
			#selector 		-> 			#compareToCurrentVersion.
			#balloonText 	-> 			'compare selected version to the current version'
		} asDictionary.
		{
			#label 			-> 			'revert to selected version'.
			#selector 		-> 			#fileInSelections.
			#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#selector 		-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#selector 		-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#selector 		-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu! !

!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:05:55'!
methodVersionsMenu
	"Fill aMenu with menu items appropriate to the receiver"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Versions'.
	aMenu addStayUpIcons.
	model listIndex > 0 ifTrue:[
		(model list size > 1 ) ifTrue: [
			aMenu addItemsFromDictionaries: `{
				{
					#label 			-> 			'compare to current'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToCurrentVersion.
					#balloonText 	-> 			'compare selected version to the current version'
				} asDictionary.
				{
					#label 			-> 			'compare to version...'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToOtherVersion.
					#balloonText 	-> 			'compare selected version to another selected version'
				} asDictionary.
			}` ].
		"Note: Revert to selected should be visible for lists of length one for having the ability to revert to an accidentally deleted method"
		aMenu addItemsFromDictionaries: `{
			{
				#label 			-> 			'revert to selected version (z)'.
				#object 			-> 			#model.
				#selector 		-> 			#fileInSelections.
				#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
			} asDictionary.
		}` ].

	aMenu addItemsFromDictionaries: `{
		{
			#label 			-> 			'edit current method (O)'.
			#selector 		-> 			#openSingleMessageBrowser.
			#balloonText 	-> 			'open a single-message browser on the current version of this method'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#object 			-> 			#model.
			#selector 		-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#object 			-> 			#model.
			#selector 		-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'senders (n)'.
			#selector 		-> 			#browseSenders.
			#balloonText 	-> 			'browse all senders of this selector'
		} asDictionary.
		{
			#label 			-> 			'implementors (m)'.
			#selector 		-> 			#browseImplementors.
			#balloonText 	-> 			'browse all implementors of this selector'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#object 			-> 			#model.
			#selector 		-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu! !

!VersionsBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:15:26'!
changeListKey: aChar from: view
	"Respond to a Command key in the list pane. of the versions browser"

	self flag: #definesKeyboardShortcuts.
	aChar = $z ifTrue: [^ model fileInSelections].
	^ self messageListKey: aChar from: view! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:21:37'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	self flag: #assumesKeyboardShortcuts.
	isForBaseSystem := model changeSet isForBaseSystem.
	aMenu := MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove 	icon: #fileOutIcon 	enabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep 		icon: #fileOutIcon.
	aMenu addLine.

	aMenu add: 'Rename change set (r)' 			action: #rename 					icon: #saveAsIcon 	enabled: isForBaseSystem.
	aMenu add: 'Destroy change set (x)' 			action: #remove 					icon: #warningIcon 	enabled: isForBaseSystem.
	aMenu addLine.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble 			icon: #textEditorIcon.
				aMenu add: 'Remove preamble' 	action: #removePreamble 		icon: #listRemoveIcon ]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 	action: #addPreamble 			icon: #listAddIcon ].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'Edit postscript...' 		action: #editPostscript 			icon: #textEditorIcon .
				aMenu add: 'Remove postscript' 	action: #removePostscript 		icon: #listRemoveIcon ]
			ifFalse: [
				aMenu add: 'Add postscript...' 		action: #editPostscript 			icon: #listAddIcon ].
	].
	aMenu addLine.

	"CONFLICTS SECTION"
	(aMenu add: 'conflicts with other change sets' 	target: self 	action: #browseMethodConflicts 	icon: #emblemImportantIcon)
		setBalloonText: 'Browse all methods that occur both in this change set and in at least one other change set.'.
	aMenu addLine.

	"CHECKS SECTION"
	(aMenu add: 'trim history' 						action: #trimHistory 				icon: #clockIcon 		enabled: isForBaseSystem)
		setBalloonText: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.'.

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories icon: #packageIcon)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:04:58'!
classListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'delete class from change set (d)'.
				#object 			-> 		#model.
				#selector 		-> 		#forgetClass.
				#icon 			-> 		#warningIcon
			} asDictionary.
			{
				#label 			-> 		'remove class from system (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class vars'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
		}`.
	^ aMenu! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:06:04'!
messageMenu
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'message list'.
	aMenu addStayUpIcons.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'delete method from changeSet (d)'.
				#object 			-> 		#model.
				#selector 			-> 		#forget.
				#icon 			-> 		#warningIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove method from system (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse method (O)'.
				#selector 			-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'senders of... (n)'.
				#selector 			-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'implementors of... (m)'.
				#selector 			-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#label 			-> 		'inheritance (i)'.
				#selector 			-> 		#methodInheritance.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#label 			-> 		'versions (v)'.
				#selector 			-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
		}`.
	^ aMenu! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:15:35'!
changeSetListKey: aChar from: view
	"Respond to a Command key.  I am a model with a listView that has a list of changeSets."

	self flag: #definesKeyboardShortcuts.
	aChar = $D ifTrue: [^ model toggleDiffing]. 
	aChar = $o ifTrue: [^ model fileOutAndRemove].
	aChar = $k ifTrue: [^ model fileOutAndKeep].
	aChar = $r ifTrue: [^ model rename].
	aChar = $x ifTrue: [^ model remove].

	^ self messageListKey: aChar from: view! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:14:02'!
classListKey: aChar from: view
	"Respond to a Command key in the class-list pane."

	self flag: #definesKeyboardShortcuts.
	aChar = $x ifTrue: [^ model removeClass].
	aChar = $d ifTrue: [^ model forgetClass]. 

	^ self messageListKey: aChar from: view "picks up b,h,p"! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:11:32'!
messageListKey: aChar from: view
	"Respond to a Command key in the message-list pane."

	self flag: #definesKeyboardShortcuts.
	aChar = $d ifTrue: [^ model forget].
	super messageListKey: aChar from: view! !


!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:06:22'!
contextFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 		-> 		#exploreContextSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#contextClassHierarchy
			} asDictionary.
		}`.
	^ aMenu! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:06:27'!
contextStackMenu
	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fullStack (f)'.
				#selector 		-> 		#fullStack
			} asDictionary.
			{
				#label 			-> 		'restart (r)'.
				#selector 		-> 		#restart
			} asDictionary.
			{
				#label 			-> 		'proceed (p)'.
				#selector 		-> 		#proceed
			} asDictionary.
			{
				#label 			-> 		'step (t)'.
				#selector 		-> 		#doStep
			} asDictionary.
			{
				#label 			-> 		'step through (T)'.
				#selector 		-> 		#stepIntoBlock
			} asDictionary.
			{
				#label 			-> 		'send (e)'.
				#selector 		-> 		#send
			} asDictionary.
			{
				#label 			-> 		'where (w)'.
				#selector 		-> 		#where
			} asDictionary.
			{
				#label 			-> 		'peel to first like this'.
				#selector 		-> 		#peelToFirst
			} asDictionary.
			nil.
			{
				#label 			-> 		'return entered value'.
				#selector 		-> 		#returnValue
			} asDictionary.
			{
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry
			} asDictionary.
		}`.
	^aMenu! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:06:35'!
receiverFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 		-> 		#exploreReceiverSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#receiverClassHierarchy
			} asDictionary.
		}`.
	^ aMenu! !

!DebuggerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:15:45'!
contextStackKey: aChar from: view
	"Respond to a keystroke in the context list"

	self flag: #definesKeyboardShortcuts.
	aChar = $e ifTrue: [^ self send].
	aChar = $t ifTrue: [^ self doStep].
	aChar = $T ifTrue: [^ self stepIntoBlock].
	aChar = $p ifTrue: [^ self proceed].
	aChar = $r ifTrue: [^ self restart].
	aChar = $f ifTrue: [^ self fullStack].
	aChar = $w ifTrue: [^ self where].

	^ self messageListKey: aChar from: view! !

!DebuggerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:16:53'!
inspectorKey: aChar from: view
	"Respond to a Command key issued while the cursor is over my field list"

	self flag: #definesKeyboardShortcuts.
	aChar = $i ifTrue: [^ self inspectSelectionIn: view model ].
	aChar = $I ifTrue: [^ self exploreSelectionIn: view model ].
	aChar = $b ifTrue:[^ self browseMethodFullIn: view model ].
	aChar = $h ifTrue:[^ self browseHierarchyIn: view model].
	aChar = $p ifTrue: [^ self browseFullProtocolIn: view model].
	aChar = $N ifTrue: [^ self browseClassRefsIn: view model].! !


!FileListWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:16:11'!
fileListKey: aChar from: aView

	self flag: #definesKeyboardShortcuts.
	aChar = $x ifTrue: [ ^ aView model deleteFile ].
	aChar = $R ifTrue: [ ^ aView model renameFile ].
	aChar = $n ifTrue: [ ^ aView model addNewFile ].
	aChar = $N ifTrue: [ ^ aView model addNewDirectory ].! !


!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 12:27:42'!
basicMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^ `{
		{
			#label 			-> 		'inspect (i)'.
			#selector 			-> 		#inspectSelection.
			#icon 			-> 		#inspectIcon
		} asDictionary.
		{
			#label 			-> 		'explore (I)'.
			#selector 			-> 		#exploreSelection.
			#icon 			-> 		#exploreIcon
		} asDictionary.
		{
			#label 			-> 		'copy to clipboard (c)'.
			#selector 			-> 		#copySelectionToClipboard.
			#icon 			-> 		#copyIcon
		} asDictionary.
		{
			#label 			-> 		'basic inspect'.
			#selector 			-> 		#inspectBasic.
			#icon 			-> 		#inspectIcon
		} asDictionary.
		{
			#label 			-> 		'references finder'.
			#selector 			-> 		#openReferencesFinder.
			#icon 			-> 		#exploreIcon
		} asDictionary.
		{
			#label 			-> 		'weight explorer'.
			#selector 			-> 		#openWeightExplorer.
			#icon 			-> 		#exploreIcon
		} asDictionary.
	}`.! !

!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 14:47:34'!
menuOptionsForBrowsing

	self flag: #assumesKeyboardShortcuts.
	^ `{
		nil.
		{
			#label 			-> 		'browse full (b)'.
			#selector 			-> 		#browseMethodFull.
			#icon 			-> 		#editFindReplaceIcon
		} asDictionary.
		{
			#label 			-> 		'browse hierarchy (h)'.
			#selector 			-> 		#browseHierarchy.
			#icon 			-> 		#goTopIcon
		} asDictionary.
		{
			#label 			-> 		'browse protocol (p)'.
			#selector 			-> 		#browseFullProtocol.
			#icon 			-> 		#spreadsheetIcon
		} asDictionary.
	}`! !

!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:07:09'!
menuOptionsForDictionary

	^ `{
		nil.
		{
			#label 			-> 		'inspect key'.
			#selector 			-> 		#inspectSelectedKey.
			#icon 			-> 		#findIcon
		} asDictionary.
		{
			#label 			-> 		'senders of this key'.
			#selector 			-> 		#sendersOfSelectedKey.
			#icon 			-> 		#mailForwardIcon
		} asDictionary.
		{
			#label 			-> 		'add key'.
			#selector 			-> 		#addEntry.
			#icon 			-> 		#listAddIcon
		} asDictionary.
		{
			#label 			-> 		'rename key'.
			#selector 			-> 		#renameEntry.
			#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
			#label 			-> 		'remove'.
			#object 			-> 		#model.
			#selector 			-> 		#removeSelection.
			#icon 			-> 		#listRemoveIcon
		} asDictionary.
	}`! !

!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:07:15'!
menuOptionsForSet

	^ `{
		nil.
		{
			#label 			-> 		'remove'.
			#object 			-> 		#model.
			#selector 			-> 		#removeSelection.
			#icon 			-> 		#listRemoveIcon
		} asDictionary.
	}`! !

!InspectorWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:16:47'!
inspectorKey: aChar from: view
	"Respond to a Command key issued while the cursor is over my field list"

	self flag: #definesKeyboardShortcuts.
	aChar = $i ifTrue: [^ self inspectSelection].
	aChar = $I ifTrue: [^ self exploreSelection].
	aChar = $b ifTrue:[^ self browseMethodFull].
	aChar = $h ifTrue:[^ self browseHierarchy].
	aChar = $p ifTrue: [^ self browseFullProtocol].
	aChar = $N ifTrue: [^ self browseClassRefs].! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'jmv 7/3/2023 15:07:22'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				action: #yourself]
		ifNotNil: [
			aMenu
				addItemsFromDictionaries: `{
					{
						#label 			-> 		'inspect (i)'.
						#selector 			-> 		#inspectSelection.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'explore (I)'.
						#selector 			-> 		#exploreSelection.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'copy to clipboard (c)'.
						#selector 			-> 		#copySelectionToClipboard.
						#icon 			-> 		#copyIcon
					} asDictionary.
					{
						#label 			-> 		'basic inspect'.
						#selector 			-> 		#inspectBasic.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'references finder'.
						#selector 			-> 		#openReferencesFinder.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'weight explorer'.
						#selector 			-> 		#openWeightExplorer.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					nil.
					{
						#label 			-> 		'browse full (b)'.
						#selector 			-> 		#browseMethodFull.
						#icon 			-> 		#editFindReplaceIcon
					} asDictionary.
					{
						#label 			-> 		'browse hierarchy (h)'.
						#selector 			-> 		#browseHierarchy.
						#icon 			-> 		#goTopIcon
					} asDictionary.
					{
						#label 			-> 		'browse protocol (p)'.
						#selector 			-> 		#browseFullProtocol.
						#icon 			-> 		#spreadsheetIcon
					} asDictionary.
				}`.
			model getCurrentSelection ifNotNil: [ :currSel |
				(currSel item is: #Morph) ifTrue: [
					aMenu addLine.
					aMenu add: 'show morph halo' target: currSel item action: #addHalo]].
			aMenu addLine;
				add: 'monitor changes'
				target: self
				action: #monitor:
				argument: model getCurrentSelection.
			model class == ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					action: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				action: #stopMonitoring ].
	^ aMenu! !

!ObjectExplorerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:16:42'!
explorerKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	model getCurrentSelection ifNotNil: [

		aChar = $i ifTrue: [^ self inspectSelection].
		aChar = $I ifTrue: [^ self exploreSelection].

		aChar = $b ifTrue: [^ self browseMethodFull].
		aChar = $h ifTrue: [^ self browseHierarchy].
		aChar = $p ifTrue: [^ self browseFullProtocol].
		aChar = $N ifTrue: [^ self browseClassRefs]].! !


!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:21:19'!
processListMenu

	| menu rules |
	self flag: #assumesKeyboardShortcuts.
	menu := MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			rules := model class rulesFor: model selectedProcess.
			menu
				add: 'inspect (i)'				action: #inspectProcess 				icon: #inspectIcon;
				add: 'explore (I)'				action: #exploreProcess 				icon: #exploreIcon;
				add: 'references finder'	action: #openReferencesFinder 		icon: #exploreIcon.
			rules first
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess icon: #cancelIcon.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess icon: #mediaPlaybackStartIcon]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess icon: #chatIcon]].
			rules second
				ifTrue: [
					menu
						add: 'change priority (p)'	action: #changePriority 	icon: #systemMonitorIcon;
						add: 'debug (d)'				action: #debugProcess 	icon: #debugIcon ].
			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [menu add: 'signal Semaphore (S)' target: model action: #signalSemaphore icon: #haloHelpIcon ].
			menu add: 'full stack (k)' target: model action: #moreStack icon: #systemIcon.
			menu addLine].

	menu
		add: 'find context... (f)'					action: #findContext 	icon: #findIcon;
		add: 'find again (g)'	target: model 	action: #nextContext 	icon: #systemIcon.
	menu addLine.

	isStepping
		ifTrue: [ 	menu add: 'turn off auto-update (a)' 	action: #toggleAutoUpdate icon: #cancelIcon ]
		ifFalse: [ 	menu add: 'turn on auto-update (a)' 	action: #toggleAutoUpdate icon: #updateIcon ].
		
	menu add: 'update list (u)' target: model action: #updateProcessList icon: #updateIcon.

	menu addLine.
	CPUWatcher isMonitoring
			ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher icon: #inspectIcon ]
			ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher icon: #inspectIcon ].
	StackSizeWatcher isWatching
			ifTrue: [ menu add: 'stop StackSizeWatcher' action: #stopStackSizeWatcher icon: #inspectIcon ]
			ifFalse: [ menu add: 'start StackSizeWatcher' action: #startStackSizeWatcher  icon: #inspectIcon ].

	^ menu! !

!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:21:11'!
stackListMenu
	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	model selectedContext
		ifNil: [^ aMenu].
	aMenu
		add: 'inspect context (c)' 		action: #inspectContext icon: #inspectIcon;
		add: 'explore context (C)' 		action: #exploreContext icon: #exploreIcon;
		add: 'inspect receiver (i)' 		action: #inspectReceiver icon: #inspectIcon;
		add: 'explore receiver (I)' 		action: #exploreReceiver icon: #exploreIcon;
		add: 'browse (b)' 				action: #browseContext icon: #editFindReplaceIcon.
	^aMenu! !

!ProcessBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:19:45'!
processListKey: aKey from: aView

	self flag: #definesKeyboardShortcuts.
	^ aKey caseOf: {
		[$i] -> [self inspectProcess].
		[$I] -> [self exploreProcess].
		[$P] -> [self openReferencesFinder].
		[$t] -> [model terminateProcess].
		[$r] -> [model resumeProcess].
		[$s] -> [model suspendProcess].
		[$d] -> [self debugProcess].
		[$p] -> [self changePriority].
		[$f] -> [self findContext].
		[$g] -> [model nextContext].
		[$a] -> [self toggleAutoUpdate].
		[$u] -> [model updateProcessList].
		[$S] -> [model signalSemaphore].
		[$k] -> [model moreStack]}
		 otherwise: []! !

!ProcessBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:19:59'!
stackListKey: aKey from: aView 

	self flag: #definesKeyboardShortcuts.
	^ aKey caseOf: {
		[$c] -> [self inspectContext].
		[$C] -> [self exploreContext].
		[$i] -> [self inspectReceiver].
		[$I] -> [self exploreReceiver].
		[$b] -> [self browseContext]}
		 otherwise: [].! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/3/2023 15:19:53'!
buildFilterButton
	| filterButton |
	self flag: #assumesKeyboardShortcuts.
	filterButton := PluggableButtonMorph 
				model: model
				action: #setFilter
				label: 'Filter'.
	filterButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'select tests matching a string pattern (f)'.
	^filterButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/3/2023 15:20:02'!
buildRefreshButton
	| refreshButton |
	self flag: #assumesKeyboardShortcuts.
	refreshButton := PluggableButtonMorph 
				model: model
				action: #refreshTests
				label: 'Refresh'.
	refreshButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'clear and update the tests list (u)'.
	^refreshButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/3/2023 15:20:13'!
buildRunButton
	| runButton |
	self flag: #assumesKeyboardShortcuts.
	runButton := PluggableButtonMorph 
				model: model
				action: #runTests
				label: 'Run'.
	runButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run all tests selected (r)'.
	^runButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/3/2023 15:20:17'!
buildRunOneButton
	| runOneButton |
	self flag: #assumesKeyboardShortcuts.
	runOneButton := PluggableButtonMorph 
				model: model
				action: #runOneTest
				label: 'Run One'.
	runOneButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run one selected test (o)'.
	^runOneButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/3/2023 15:20:20'!
buildRunProfiledButton
	| runProfiledButton |
	self flag: #assumesKeyboardShortcuts.
	runProfiledButton := PluggableButtonMorph 
				model: model
				action: #runProfiledTests
				label: 'Run Profiled'.
	runProfiledButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run and profile all tests selected (p)'.
	^runProfiledButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/3/2023 15:20:25'!
buildStopButton
	| stopButton |
	self flag: #assumesKeyboardShortcuts.
	stopButton := PluggableButtonMorph 
				model: model
				action: #interruptRun
				label: 'Stop'.
	stopButton
		enableSelector: #isRunning;
		color: self runButtonColor;
		setBalloonText: 'interrupt and debug the currently running test (l)'.
	^stopButton! !

!TestRunnerWindow methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:20:53'!
listMenu
	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Test Cases';
		add: 'select all (a)' target: model action: #selectAll icon: #selectAllIcon;
		add: 'deselect all (A)' target: model action: #deselectAll icon: #selectAllIcon;
		add: 'invert selections' target: model action: #invertSelections icon: #switchIcon;
		add: 'filter (f)' target: model action: #setFilter icon: #findIcon;
		add: 'refresh (u)' target: model action: #refreshTests icon: #refreshIcon;
		addLine.
	model isRunning
		ifTrue:
			[aMenu add: 'stop (l)' target: model action: #interruptRun icon: #stopIcon]
		ifFalse:
			[aMenu add: 'run (r)' target: model action: #runTests icon: #playIcon;
				add: 'run one (o)' target: model action: #runOneTest icon: #playIcon;
				add: 'run profiled (p)' target: model action: #runProfiledTests icon: #playIcon].
	model selectedSuite > 0 ifTrue: [ | cls |
		cls := (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
		cls := cls asSymbol.
		cls := (Smalltalk at: cls ifAbsent: nil).
		cls ifNotNil: [
			aMenu addLine.
			(aMenu add: 'browse' target: self action: #browse: argument: cls)
				setIcon: #editFindReplaceIcon.
		].
	].
	aMenu addLine.
	aMenu add: 'log to Transcript' target: model action: #showResult icon: #printerIcon.
	^aMenu! !

!TestRunnerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:16:35'!
listKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $a ifTrue: [^ model selectAll].
	aChar = $A ifTrue: [^ model deselectAll].
	aChar = $u ifTrue: [^ self refreshTests].
	aChar = $f ifTrue: [^ model setFilter].
	aChar = $r ifTrue: [^ model runTests].
	aChar = $o ifTrue: [^ model runOneTest].
	aChar = $p ifTrue: [^ model runProfiledTests].
	aChar = $l ifTrue: [^ model interruptRun].! !


!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 7/3/2023 12:18:01'!
isCloseWindowShortcut

	self flag: #definesKeyboardShortcuts.
	^ (self commandAltKeyPressed or: [ self controlKeyPressed ])
		and: [self keyCharacter = $w].! !

!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 7/3/2023 12:17:33'!
isFindClassShortcut

	self flag: #definesKeyboardShortcuts.
	^ self shiftPressed and: [ self isReturnKey ].! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 7/3/2023 15:08:53'!
helpMenuOptions

	^ `{
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 			-> 		#aboutThisSystem.
				#icon 			->		#helpIcon.
				#balloonText 	-> 				'current version information.'
			} asDictionary.
			{
				#submenuOf 		-> TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Terse Guide to Cuis'.
				#selector 			-> 		#openTerseGuide.
				#icon 			->		#helpIcon.
				#balloonText 	-> 				'explore Cuis Smalltalk'
			} asDictionary.
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Class Comment Browser'.
				#selector 			-> 		#openCommentGuide.
				#icon 			->		#helpIcon.
				#balloonText 	-> 				'search & explore Cuis Class Comments'
			} asDictionary.
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCodeManagementInCuis.
				#icon 			->		#helpIcon.
				#balloonText 	-> 				'Features are kept in Packages.'
			} asDictionary.
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		50.
				#label 			-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCuisAndGitHub.
				#icon 			->		#helpIcon.
				#balloonText 	-> 				'GitHub usage pattern.'
			} asDictionary.
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 			-> 		#openHelp.
				#icon 			-> 		#keyboardShortcutsIcon.
				#balloonText 	-> 				'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openUsefulExpressions.
				#icon 			-> 		#chatIcon.
				#balloonText 	-> 				'a window full of useful expressions.'
			} asDictionary.
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'VM Statistics'.
				#selector 			-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 	-> 				'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#submenuOf -> 					TheWorldMenu helpLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Space Left'.
				#selector 			-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 	-> 				'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'jmv 7/3/2023 14:56:35'!
smalltalkEditorMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^`{
	{
			#itemGroup 			-> 		35.
			#itemOrder 			-> 		10.
			#label 			-> 		RefactoringMenues refactoringsLabel.
			#selector 			-> 		#yourself.
			#icon 			-> 		#listAddIcon
	} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 			-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Extract as Parameter... (1)'.
				#selector 			-> 		#contextualExtractAsParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Add Parameter... (A)'.
				#selector 			-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Remove Parameter... (S)'.
				#selector 			-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Extract Temporary... (J)'.
				#selector 			-> 		#extractToTemporary.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Extract Method... (K)'.
				#selector 			-> 		#extractMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Inline Temporary... (2)'.
				#selector 			-> 		#inlineTemporaryVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Inline Method... (3)'.
				#selector 			-> 		#contextualInlineMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.		
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 			-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Push Up Instance Variable'.
				#selector 			-> 		#contextualPushUpInClassDefinition.
				#icon 			-> 		#goTopIcon
		} asDictionary.
		{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'Push Down Instance Variable'.
				#selector 			-> 		#contextualPushDownInClassDefinition.
				#icon 			-> 		#goBottomIcon
		} asDictionary.
	
	}`! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 15:11:44'!
classListMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^ `{
			{
				#itemGroup 				-> 		5.
				#itemOrder 				-> 		10.
				#label 				-> 		RefactoringMenues refactoringsLabel.
				#selector 				-> 		#yourself.
				#icon 				-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf  				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		10.
				#label 				-> 		'rename class... (R)'.
				#object 				-> 		#model.
				#selector 				-> 		#renameClass.
				#icon 				-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		20.
				#label 				-> 		'insert superclass...'.
				#object 				-> 		#model.
				#selector 				-> 		#insertSuperclass.
				#icon 				-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		30.
				#label 				-> 		'safely remove class (x)'.
				#object 				-> 		#model.
				#selector 				-> 		#safelyRemoveClass.
				#icon 				-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		10.
				#label 				-> 		'add inst var...'.
				#selector 				-> 		#addInstVar.
				#icon 				-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		20.
				#label 				-> 		'rename inst var...'.
				#selector 				-> 		#renameInstVar.
				#icon 				-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		30.
				#label 				-> 		'remove inst var...'.
				#selector 				-> 		#removeInstVar.
				#icon 				-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		40.
				#label 				-> 		'remove all unreferenced inst var...'.
				#selector 				-> 		#removeAllUnreferencedInstVar.
				#icon 				-> 		#deleteIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		50.
				#label 				-> 		'push up inst var...'.
				#selector 				-> 		#pushUpInstanceVariable.
				#icon 				-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		60.
				#label 				-> 		'push down inst var...'.
				#selector 				-> 		#pushDownInstanceVariable.
				#icon 				-> 		#goBottomIcon
			} asDictionary.				
			{
				#submenuOf 				-> 		RefactoringMenues refactoringsLabel.
				#itemGroup 				-> 		30.
				#itemOrder 				-> 		10.
				#label 				-> 		'rename global...'.
				#object 				-> 		#model.
				#selector 				-> 		#renameGlobal.
				#icon 				-> 		#saveAsIcon
			} asDictionary.
	}`.! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'jmv 7/3/2023 15:11:37'!
messageListMenuOptions

	self flag: #assumesKeyboardShortcuts.
	^ `{
		{
			#itemGroup 			-> 		5.
			#itemOrder 			-> 		10.
			#label 			-> 		RefactoringMenues refactoringsLabel.
			#selector 			-> 		#yourself.
			#icon 			-> 		#listAddIcon
		} asDictionary.
			{
				#submenuOf 	-> 				RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 			-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 			-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 			-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 			-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'inline method... (3)'.
				#selector 			-> 		#inlineMethod.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'move to instance/class method'.
				#selector 			-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'push up'.
				#selector 			-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'add in superclass as subclassResponsability'.
				#selector 			-> 		#addAsSubclassResponsabilitySelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		50.
				#label 			-> 		'push down to subclasses'.
				#selector 			-> 		#pushDownSelectorToSubclasses.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#submenuOf -> 					RefactoringMenues refactoringsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		60.
				#label 			-> 		'push down to one subclass'.
				#selector 			-> 		#pushDownSelectorToOneSubclass.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			
		}`.
	! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'jmv 7/3/2023 12:03:44'!
smalltalkEditorCmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	self flag: #definesKeyboardShortcuts.
	^#(
		#(	$R	#contextualRename:								'Renames what is under cursor')
		#(	$T	#temporaryToInstanceVariable:								'Extracts temporary variable to instance variable')
		#(	$O	#inlineTemporaryVariable:								'Inlines the selected temporary variable into each usage')
		#(	$P	#contextualInlineMethod:								'Inlines the selected message send into desired colaborations')

		#(	$A	#contextualAddParameter:								'Adds parameter to message that is under cursor')
		#(	$S	#contextualRemoveParameter:								'Removes parameter to message that is under cursor')
		#(	$J	#extractToTemporary:								'Extracts the selected code into a temporary variable')
		#(	$K	#extractMethod:								'Extracts the selected code into a separate method')		
		#(	$L 	#contextualExtractAsParameter: 								'Extracts the selected code as parameter')
	)! !

