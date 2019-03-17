'From Cuis 5.0 of 7 November 2016 [latest update: #3665] on 17 March 2019 at 9:14:29 am'!

!PasteUpMorph methodsFor: 'accessing' stamp: 'GC 3/17/2019 08:16:08'!
handlesKeyboard

	^ true ! !


!BrowserWindow class methodsFor: 'GUI building' stamp: 'GC 3/17/2019 08:13:13'!
findClass

	| allClassNames |
	
	allClassNames _ Smalltalk classNames asOrderedCollection.
	
	self findClassFrom: allClassNames ifFound: [:foundClass | self fullOnClass: foundClass ]
 	
	! !

!BrowserWindow class methodsFor: 'GUI building' stamp: 'GC 3/17/2019 08:15:15'!
findClassFrom: potentialClassNames ifFound: aBlock

	| classNames exactMatch foundClass index pattern toMatch |
	
	pattern _ ClassNameRequestMorph request: 'Class name or fragment?'.
	pattern isEmpty ifTrue: [^ self flash].
	toMatch _ (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	classNames _ (pattern last = $. or: [pattern last = $ ])
		ifTrue: [potentialClassNames select: [:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: [:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ self flash].
	exactMatch _ classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue: [ 1 ]
		ifFalse: [ exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpMenu]].

	index = 0 ifTrue: [^ self flash].
	foundClass _ Smalltalk at: (classNames at: index) asSymbol.
 	
	aBlock value: foundClass.! !

!BrowserWindow class methodsFor: 'GUI building' stamp: 'GC 3/17/2019 08:15:28'!
fullOnClass: aClass

	self fullOnClass: aClass selector: nil! !


!Theme methodsFor: 'keyboard shortcuts' stamp: 'HAW 3/17/2019 08:37:46'!
findClassIn: aWindow

	| isBrowserWindow |
	
	isBrowserWindow _ (aWindow class = BrowserWindow) or: [ aWindow class = HierarchyBrowserWindow ].
	isBrowserWindow
		ifTrue: [ aWindow findClass ] 
		ifFalse: [ BrowserWindow findClass ]! !


!TextEditor methodsFor: 'menu' stamp: 'HAW 3/17/2019 08:53:17'!
getMenu
	
	^(MenuMorph new defaultTarget: self)
		addTitle: self class name;
		addStayUpIcons;
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Find...(F)'.
				#selector 		-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'Find Again (g)'.
				#selector 		-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 		-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Undo - multiple (z)'.
				#selector 		-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 		-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#label 			-> 		'Undo / Redo history'.
				#selector 		-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Copy (c)'.
				#selector 		-> 		#copySelection.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#label 			-> 		'Cut (x)'.
				#selector 		-> 		#cut.
				#icon 			-> 		#cutIcon
			} asDictionary.
			{
				#label 			-> 		'Paste (v)'.
				#selector 		-> 		#paste.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#label 			-> 		'Paste without Format'.
				#selector 		-> 		#pasteString.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#label 			-> 		'Paste...'.
				#selector 		-> 		#pasteRecent.
				#icon 			-> 		#worldIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Toggle WordWrap'.
				#selector 		-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 		-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Clear Font'.
				#selector 		-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Default Font...'.
				#selector 		-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'Set Alignment...'.
				#selector 		-> 		#chooseAlignment.
				#icon 			-> 		#formatJustifyLeftIcon
			} asDictionary.
		}`! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/17/2019 08:58:33'!
basicCmdShortcutsSpec
	"
	Editor initialize
	"

	"arranged in QWERTY keyboard order"
	^#(
		#(		$( 	#enclose:					'Enclose within ( and ), or remove enclosing ( and )')
		#(		$[ 	#enclose:					'Enclose within [ and ], or remove enclosing [ and ]')
		#(		${ 	#enclose:					'Enclose within { and }, or remove enclosing { and }')
		#(		$' 	#enclose:					'Enclose within single quotes, or remove enclosing single quotes')
		#(		$" 	#enclose:					'Enclose within double quotes, or remove enclosing double quotes')
		#(		$` 	#enclose:					'Enclose within backticks, or remove enclosing backticks')
		#(		$< 	#enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:					'Select all')
		#(		$F 	#find:						'Find')
		#(		$g 	#findAgain:					'Find again')
		#(		$h 	#help:						'Open this help')
		#(		$j 	#setSearchString:			'Set selection as search string for find again')

		#(		$z 	#undo:						'Undo (multiple levels)')
		#(		$x 	#cut:							'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:						'Paste Clipboard contents')

		#(		$R		#indent:						'Indent (move selection one tab-stap right)')
		#(		$Y		#makeUppercase:			'Force selection to uppercase')
		#(		$U		#changeLineEndsToLf:		'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H		#cursorTopHome:			'Move cursor to start of text')
		#(		$L		#outdent:					'Outdent (move selection one tab-stop left)')

		#(		$Z		#redo:						'Redo (multiple levels)')
		#(		$X		#makeLowercase:			'Force selection to lowercase')
		#(		$C		#compareToClipboard:		'Compare argument to clipboard')

		#(		$ 		#selectWord:				'Select the current word as with double clicking')
	)! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/17/2019 09:00:30'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$r	#acceptAndDebugTest:		'Saves method and debugs it')
		#(		$t	#acceptAndTest:				'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:				'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:								'Print it (selection is a valid expression)')

		#(		$s	#save:								'Save (i.e. accept)')
		#(		$d	#doIt:								'Do it (selection is a valid expression)')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:						'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:				'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:'Method strings containing it')
		#(		$T	#displayIfTrue:					'Insert #ifTrue:')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$A	#argNext:							'Next argument')
		#(		$D	#debugIt:							'Debug it')
"		#(		$F	#displayIfFalse:					'Insert #ifFalse:')"
		#(		$G	#fileItIn:							'File in selection')

		#(		$V	#pasteInitials:					'Paste author initials')
		#(		$N	#referencesToIt:					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'HAW 3/17/2019 08:53:38'!
smalltalkEditorMenu2Options
	
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Find...(F)'.
				#selector 		-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'Find Again (g)'.
				#selector 		-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 		-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Undo - multiple (z)'.
				#selector 		-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 		-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'Undo / Redo history'.
				#selector 		-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Copy (c)'.
				#selector 		-> 		#copySelection.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Cut (x)'.
				#selector 		-> 		#cut.
				#icon 			-> 		#cutIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'Paste (v)'.
				#selector 		-> 		#paste.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 			-> 		'Paste without Format'.
				#selector 		-> 		#pasteString.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label 			-> 		'Paste...'.
				#selector 		-> 		#pasteRecent.
				#icon 			-> 		#worldIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Toggle WordWrap'.
				#selector 		-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Set Font... (k)'.
				#selector 		-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Clear Font'.
				#selector 		-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'Set Default Font...'.
				#selector 		-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`
! !


!BrowserWindow methodsFor: 'commands' stamp: 'GC 3/17/2019 08:13:00'!
findClass

	| scopedClassNames |
	
	scopedClassNames _ model potentialClassNames asOrderedCollection.
	
	self class findClassFrom: scopedClassNames ifFound: [:foundClass |
		model selectCategoryForClass: foundClass.
		model selectClass: foundClass ]! !


!HandMorph methodsFor: 'events-processing' stamp: 'GC 3/17/2019 08:15:50'!
startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement _ self keyboardFocus ifNil: [ self world ].
	focusedElement handleFocusEvent: aKeyboardEvent.
	
	self mouseOverHandler processMouseOver: self lastMouseEvent! !


!Theme methodsFor: 'keyboard shortcuts' stamp: 'HAW 3/17/2019 08:59:01'!
keyStroke: aKeyboardEvent morph: aMorph

	aKeyboardEvent commandAltKeyPressed | aKeyboardEvent controlKeyPressed
		ifFalse: [ ^ false ].

	aKeyboardEvent keyValue = $w numericValue ifTrue: [
		aMorph owningWindow ifNotNil: [ :w |
			(w morphContainsPoint: (w internalizeFromWorld: aKeyboardEvent eventPosition)) ifTrue: [
				w delete. 
				^ true ]]].
	
	aKeyboardEvent keyValue = $f numericValue ifTrue: [ 
		self findClassIn: aMorph owningWindow.
		^ true].

	^ false! !


!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: setSearch: text totalTextHeight visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selection selectionAsStream selectionIntervalsDo: startBlock startIndex stopBlock stopIndex)
('attributes' changeEmphasisOrAlignment clearFont offerColorMenu offerDefaultFontMenu offerFontMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: tabKey: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model:)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: selectFrom:to: selectInterval: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('scrolling' scrollBy:)
('typing support' addString: backTo: dispatchOn: processKeyStroke: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete: selectAll)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: storeSelectionInComposition unapplyAttributes: zapMultiSelection)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize.
!

