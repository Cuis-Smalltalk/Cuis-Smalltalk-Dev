'From Cuis 5.0 [latest update: #3866] on 17 September 2019 at 9:46:20 pm'!

!TextEditor methodsFor: 'menu' stamp: 'jpb 9/17/2019 21:44:24'!
addCutAndPasteMenuSectionTo: aMenu
	"Adds  typical cut and paste operations section to a menu"
	
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

!TextEditor methodsFor: 'menu' stamp: 'jpb 9/17/2019 21:34:30'!
addFinderMenuSectionTo: aMenu
	"Build a submenu with finding related operations"
	
	aMenu addItemsFromDictionaries:
	 `{
			{
				#label 			-> 		'Find...(f)'.
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
		}`.
		^aMenu! !

!TextEditor methodsFor: 'menu' stamp: 'jpb 9/17/2019 21:41:04'!
addStyleMenuSectionTo: aMenu
	"Adds to the given menu text styiling related operations"
	
	aMenu
		addItemsFromDictionaries: 
	`{
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
		}`.
		^aMenu! !

!TextEditor methodsFor: 'menu' stamp: 'jpb 9/17/2019 21:36:55'!
addUndoMenuSectionTo: aMenu
	"Adds undo operations to the given menu"

	aMenu
		addItemsFromDictionaries: 
		`{	
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
		}`.
		
	^aMenu.! !


!TextEditor methodsFor: 'menu' stamp: 'jpb 9/17/2019 21:44:32'!
getMenu
	| aMenu | 
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addTitle: self class name;
		addStayUpIcons.
		
	aMenu
		add: 'Help...'
		action: #openHelp
		icon: #helpIcon.
	aMenu addLine.
	
	self addFinderMenuSectionTo: aMenu.
	self addUndoMenuSectionTo: aMenu.
	aMenu addLine.
	
	self addCutAndPasteMenuSectionTo: aMenu.
	aMenu addLine.
	
	self addStyleMenuSectionTo: aMenu.
	
	^aMenu ! !

!methodRemoval: TextEditor #addOperationsMenuTo:!
TextEditor removeSelector: #addOperationsMenuTo:!
!methodRemoval: TextEditor #addTextstyleMenuTo:!
TextEditor removeSelector: #addTextstyleMenuTo:!
!methodRemoval: TextEditor #addUndoMenuTo:!
TextEditor removeSelector: #addUndoMenuTo:!

!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: selectionRectangle setSearch: text totalTextHeight visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selectedString selection selectionAsStream selectionIntervalsDo: startBlock startIndex stopBlock stopIndex)
('attributes' changeEmphasisOrAlignment clearFont offerColorMenu offerDefaultFontMenu offerFontMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: tabKey: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model: wordUnder: wordUnderCursor)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: messageSendsRanges: selectFrom:to: selectInterval: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('scrolling')
('typing support' addString: backTo: dispatchOn: processKeyStroke: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete: selectAll)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: shouldDeleteAWordForward: shouldHandleUsingCmdShortcuts: storeSelectionInComposition unapplyAttributes: zapMultiSelection)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' addCutAndPasteMenuSectionTo: addFinderMenuSectionTo: addStyleMenuSectionTo: addUndoMenuSectionTo: getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
!

