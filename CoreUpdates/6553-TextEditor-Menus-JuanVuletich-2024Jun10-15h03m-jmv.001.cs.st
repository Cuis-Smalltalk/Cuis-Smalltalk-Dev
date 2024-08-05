'From Cuis7.1 [latest update: #6459] on 10 June 2024 at 3:23:07 pm'!

!TextEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/10/2024 15:18:35'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((110				'Accept (s)' 					$s 	editor 			acceptContents 						acceptIcon)) ]
		ifFalse: [ #((110 				'Accept (s)' 					nil 	editor 			nil 						acceptIcon)) ].
	^#(
		#(0 		'Text Editor' 					true)
		#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon)
		#(20 		'-----')
		#(30		'Find...(f)' 							$f 	editor 			find 						findIcon)
		#(40		'Find Again (g)' 							$g 	editor 			findAgain 						systemIcon)
		#(50		'Use Selection for Find (j)' 							$j 	editor 			setSearchString 						saveAsIcon)
		#(60 		'-----')
		#(70		'Undo - multiple (z)' 							$z 	editor 			undo 						undoIcon)
		#(80		'Redo - multiple (Z)' 							$Z 	editor 			redo 						redoIcon)
		#(90		'Undo / Redo history' 							nil 	editor 			offerUndoHistory 						changesIcon)
		#(100 		'-----')
	),
	acceptEntry,
	#(
		#(120		'Copy (c)' 							$c 	editor 			copySelection 						copyIcon)
		#(130		'Cut (x)' 							$x 	editor 			cut 						cutIcon)
		#(140		'Paste (v)' 							$v 	editor 			paste 						pasteIcon)
		#(150		'Paste without Format' 							nil 	editor 			pasteString 						pasteIcon)
		#(160		'Paste...' 							nil 	editor 			pasteRecent 						worldIcon)
		#(180 		'-----')
		#(190		'Toggle WordWrap' 							nil 	editor 			wrapOnOff 						genericTextIcon)
		#(200		'Clear Font' 							nil 	editor 			clearFont 						newIcon)
		#(210		'Set Font... (k)' 							$k 	editor 			offerFontMenu 						preferencesDesktopFontIcon)
		#(220		'Set Font Size' 							nil 	editor 			offerFontSizeMenu 						preferencesDesktopFontIcon)
		#(230		'Set Style' 							nil 	editor 			offerFontStyleMenu 						preferencesDesktopFontIcon)
		#(240		'Set Text Color' 							nil 	editor 			offerColorMenu 						graphicsIcon)
		#(250		'Set Alignment...' 							nil 	editor 			chooseAlignment 						formatJustifyLeftIcon)
	).
	
	"como es que h trae el help? ver shortcuts..."! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/10/2024 15:19:03'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((240				'Accept (s)' 					$s 	editor 			acceptContents 						acceptIcon)) ]
		ifFalse: [ #((240 				'Accept (s)' 					nil 	editor 			nil 						acceptIcon)) ].
	^#(
		#(0 		'Smalltalk Editor' 					true)
		#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon)
		#(20 		'-----')
		#(30		'Do it (d)' 							$d 	editor 			doIt 						doItIcon)
		#(40		'Print it (p)' 							$p 	editor 			printIt 						printIcon)
		#(50		'Inspect it (i)' 							$i 	editor 			inspectIt 						inspectIcon)
		#(60		'Explore it (I)' 							$I 	editor 			exploreIt 						exploreIcon)
		#(70		'Debug it (D)' 							$D 	editor 			debugIt 						debugIcon)
		#(80		'Profile it' 							nil 	editor 			profileIt 						clockIcon)
		#(90 		'-----')
		#(100		'Edit' 							nil 	nil 			1000 						textEditorIcon)
		#(110 		'-----')
		#(120		'Explain' 							nil 	editor 			explain 						helpIcon)
		#(130		'Browse it (b)' 							$b 	editor 			browseIt 						editFindReplaceIcon)
		#(140		'Senders of it (n)' 							$n 	editor 			sendersOfIt 						mailForwardIcon)
		#(150		'Implementors of it (m)' 							$m 	editor 			implementorsOfIt 						developmentIcon)
		#(160		'References to it (N)' 							$N 	editor 			referencesToIt 						addressBookIcon)
		#(170 		'-----')
		#(180		'Refactorings' 							nil 	nil 			2000 						listAddIcon)
		#(190 		'-----')
		#(200		'Method Strings with it (E)' 							$E	editor 			methodStringsContainingIt 								genericTextIcon)
		#(210		'Method Source with it (e)' 							$e 	editor 			methodSourceContainingIt 								scriptIcon)
		#(220		'Class Comments with it' 							nil 	editor 			classCommentsContainingIt 								chatIcon)
		#(230 		'-----')
	),
	acceptEntry,
	#(
		#(250		'Accept & Run Test in Class (t)' 									$t 	editor 			acceptAndTest 						weatherFewCloudsIcon)
		#(260		'Accept & Run Test in Category (y)' 									$y 	editor 			acceptAndTestAll 						weatherFewCloudsIcon)
		#(270		'Accept & Debug Test (r)' 									$r 	editor 			acceptAndDebugTest 						weatherFewCloudsIcon)


		" 'Edit' SubMenu"
		#(1010 		'-----')
		#(1020		'Find...(f)' 							$f 	editor 			find 						findIcon)
		#(1030		'Find Again (g)' 							$g 	editor 			findAgain 						systemIcon)
		#(1040		'Use Selection for Find (j)' 							$j 	editor 			setSearchString 						saveAsIcon)
		#(1050 		'-----')
		#(1060		'Undo - multiple (z)' 							$z 	editor 			undo 						undoIcon)
		#(1070		'Redo - multiple (Z)' 							$Z 	editor 			redo 						redoIcon)
		#(1080		'Undo / Redo history' 							nil 	editor 			offerUndoHistory 						changesIcon)
		#(1090 		'-----')
		#(1100		'Copy (c)' 							$c 	editor 			copySelection 						copyIcon)
		#(1110		'Cut (x)' 							$x 	editor 			cut 						cutIcon)
		#(1120		'Paste (v)' 							$v 	editor 			paste 						pasteIcon)
		#(1130		'Paste without Format' 							nil 	editor 			pasteString 						pasteIcon)
		#(1140		'Paste...' 							nil 	editor 			pasteRecent 						worldIcon)
		#(1150 		'-----')
		#(1160		'Toggle WordWrap' 							nil 	editor 			wrapOnOff 						genericTextIcon)
		#(1170		'Clear Font' 							nil 	editor 			clearFont 						newIcon)
		#(1180		'Set Font... (k)' 							$k 	editor 			offerFontMenu 						preferencesDesktopFontIcon)
		#(1190		'Set Font Size' 							nil 	editor 			offerFontSizeMenu 						preferencesDesktopFontIcon)

		" 'Refactorings' SubMenu"
		#(2010 		'-----')
		#(2020		'Rename... (R)' 									$R 	editor 			contextualRename 										saveAsIcon)
		#(2030		'Extract as Parameter... (V)' 									$V 	editor 			contextualExtractAsParameter 										listAddIcon)
		#(2040		'Add Parameter... (A)' 									$A 	editor 			contextualAddParameter 										listAddIcon)
		#(2050		'Remove Parameter... (S)' 									$S 	editor 			contextualRemoveParameter 										listRemoveIcon)
		#(2060 		'-----')
		#(2070		'Extract Temporary... (J)' 									$J 	editor 			extractToTemporary 										saveAsIcon)
		#(2080		'Extract Method... (K)' 									$K 	editor 			extractMethod 										saveAsIcon)
		#(2090 		'-----')
		#(2100		'Inline Temporary... (O)' 									$O 	editor 			inlineTemporaryVariable 										saveAsIcon)
		#(2110		'Inline Method... (F)' 									$F 	editor 			contextualInlineMethod 										saveAsIcon)
		#(2120 		'-----')
		#(2130		'Temporary to Instance Variable (G)' 									$G 	editor 			temporaryToInstanceVariable 										saveAsIcon)
		#(2140		'Push Up Instance Variable' 									nil 	editor 			contextualPushUpInClassDefinition 										goTopIcon)
		#(2150		'Push Down Instance Variable' 									nil 	editor 			contextualPushDownInClassDefinition 										goBottomIcon)
	).
! !


!InnerTextMorph methodsFor: 'menu' stamp: 'jmv 6/10/2024 10:07:58'!
menuNshortcutsSpecsDo: aBlock
	"Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."

	self editor defaultMenuSpec do: aBlock! !


!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 6/10/2024 15:18:27'!
getMenu
	"Answer the right-click (contextual) menu."
	"Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- itemId is a number (integer or float). It specifies the position of the item in the menu, and is also an id, allowing for overrides and deletions.
		- label is a string or symbol. Symbol means self updating menu entry. nil means just keyboard shortcut, no menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Typically #window, #model or #editor. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
			If a Number (multiple of 1000), indicates a subMenu. ItemsIds must be in range. See BrowserWindow>>#classListMenuSpec.
			If nil, the menu entry is created but disabled. The keyboard shortcut is disabled too.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"

	| menuNshortcutsSpecs itemSpec menu submenus |
	menuNshortcutsSpecs := self allMenuNshortcutsSpecs.
	menuNshortcutsSpecs ifEmpty: [ ^nil ].
	menu := MenuMorph new.
	submenus := nil.
	menuNshortcutsSpecs keys sorted do: [ :key |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size <= 2 ifTrue: [
				self addLineTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [ itemSpec size = 3 ifTrue: [
				self addTitleTo: menu fromSpec: itemSpec ]
			ifFalse: [ itemSpec fifth isNumber ifTrue: [
				submenus ifNil: [submenus := Dictionary new ].
				self addSubmenuTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [
				self addItemTo: menu fromSpec: itemSpec submenus: submenus ]]]].
	^ menu! !


!TextModelMorph methodsFor: 'event handling' stamp: 'jmv 6/10/2024 10:27:01'!
mouseButton2Activity
	"Button 2 presses inside us, but not inside our InnerTextMorph (i.e. scroller) are caught here.
	Forward them to the InnerTextMorph, so they behave the same as if the button press actually occured there."

	scroller mouseButton2Activity! !

!methodRemoval: TextModelMorph #menuNshortcutsSpecsDo: stamp: 'jmv 6/10/2024 15:05:51'!
TextModelMorph removeSelector: #menuNshortcutsSpecsDo:!
!methodRemoval: InnerTextMorph #getMenu stamp: 'jmv 6/10/2024 15:07:19'!
InnerTextMorph removeSelector: #getMenu!
!methodRemoval: SmalltalkEditor class #smalltalkEditorMenuOptions stamp: 'jmv 6/10/2024 15:10:55'!
SmalltalkEditor class removeSelector: #smalltalkEditorMenuOptions!
!methodRemoval: SmalltalkEditor class #smalltalkEditorEditingMenuOptions stamp: 'jmv 6/10/2024 15:11:14'!
SmalltalkEditor class removeSelector: #smalltalkEditorEditingMenuOptions!
!methodRemoval: SmalltalkEditor class #editMenuLabel stamp: 'jmv 6/10/2024 15:11:20'!
SmalltalkEditor class removeSelector: #editMenuLabel!
!methodRemoval: SmalltalkEditor #getMenu stamp: 'jmv 6/10/2024 15:09:07'!
SmalltalkEditor removeSelector: #getMenu!
!methodRemoval: TextEditor #addCutAndPasteMenuSectionTo: stamp: 'jmv 6/10/2024 15:09:43'!
TextEditor removeSelector: #addCutAndPasteMenuSectionTo:!
!methodRemoval: TextEditor #getMenu stamp: 'jmv 6/10/2024 15:09:08'!
TextEditor removeSelector: #getMenu!
!methodRemoval: TextEditor #addUndoMenuSectionTo: stamp: 'jmv 6/10/2024 15:09:13'!
TextEditor removeSelector: #addUndoMenuSectionTo:!
!methodRemoval: TextEditor #addStyleMenuSectionTo: stamp: 'jmv 6/10/2024 15:09:31'!
TextEditor removeSelector: #addStyleMenuSectionTo:!
!methodRemoval: TextEditor #addFinderMenuSectionTo: stamp: 'jmv 6/10/2024 15:09:37'!
TextEditor removeSelector: #addFinderMenuSectionTo:!

!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil replaceSelectionWith: selectionRectangle setSearch: text totalTextHeight userHasEdited visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointBlock pointIndex pointIndex: selectedString selection selectionIsEmptyOrSeparators startBlock startIndex stopBlock stopIndex)
('attributes' clearFont offerColorMenu offerFontMenu offerFontSizeMenu offerFontStyleMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: save: tabKey: undo:)
('events' clickAndHalfAt: doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model: wordAt: wordUnderCursor)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: insertAndSelect:at: selectFrom:to: selectLine selectWordOrDelimitedTextAt:)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('typing support' addString: backTo: dispatchOn: processKeystrokeEvent: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete:)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: shouldDeleteAWordForward: shouldHandleUsingCmdShortcuts: storeSelectionInComposition unapplyAttributes:)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
('menu & kbd shortcuts' defaultMenuSpec)
!


!SmalltalkEditor class reorganize!
('keyboard shortcut tables' cmdShortcutsSpec)
('cached state access' releaseClassCachedState)
('explain' plateA plateB plateX)
!


!TextModelMorph reorganize!
('accessing' disableEditing editor emptyTextDisplayMessage: enableEditing innerTextMorph wrapFlag:)
('dependents access' canDiscardEdits)
('drawing' drawOn: wantsFrameAdornments)
('editor access' scrollSelectionIntoView selectAll startIndex)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: mouseButton1Down:localPosition: mouseEnter:)
('event handling' mouseButton2Activity)
('focus handling' focusText)
('geometry' innerHeight: scrollDeltaHeight)
('initialization' acceptOnCR: defaultColor escAction: innerMorphClass keystrokeAction: model:)
('interactive error protocol' correctFrom:to:with: nextTokenFrom:direction: notify:at:in: selectFrom:to: selectInvisiblyFrom:to: selectionInterval)
('model access' setSelection: setTextColor: text)
('notifications' possiblyChanged)
('scrolling' mightNeedHorizontalScrollBar)
('unaccepted edits' askBeforeDiscardingEdits: hasUnacceptedEdits:)
('updating' update: updateAcceptedContents updateActualContents updateClearUserEdits updateInitialSelection updateRefetched updateShoutStyled)
('accept/cancel' acceptOnAny:)
!

!classRemoval: #RefactoringMenues stamp: 'jmv 6/10/2024 15:11:57'!
Smalltalk removeClassNamed: #RefactoringMenues!

