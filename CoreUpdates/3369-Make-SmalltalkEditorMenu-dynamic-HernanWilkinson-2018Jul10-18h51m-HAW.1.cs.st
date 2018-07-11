'From Cuis 5.0 of 7 November 2016 [latest update: #3361] on 10 July 2018 at 7:12:14 pm'!

!SmalltalkEditor methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:08:18'!
createMenuCollectingOptionsWith: aMenuOptionsSelector
	
	^(DynamicMenuBuilder buildTitled: self class name targeting: self collectingMenuOptionsWith: aMenuOptionsSelector)
		addStayUpIcons;
		yourself
		
! !

!SmalltalkEditor methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:08:50'!
getMenu2
	
	^self createMenuCollectingOptionsWith: #smalltalkEditorMenu2Options! !

!SmalltalkEditor methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:09:14'!
openMenu: aMenu
	
	aMenu popUpInWorld: morph world! !


!SmalltalkEditor class methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:03:02'!
smalltalkEditorMenu2Options
	
	^`{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Find...(f)'.
				#selector 		-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'Find Again (g)'.
				#selector 		-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 		-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Undo - multiple (z)'.
				#selector 		-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 		-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'Undo / Redo history'.
				#selector 		-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Copy (c)'.
				#selector 		-> 		#copySelection.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Cut (x)'.
				#selector 		-> 		#cut.
				#icon 			-> 		#cutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'Paste (v)'.
				#selector 		-> 		#paste.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		40.
				#label 			-> 		'Paste without Format'.
				#selector 		-> 		#pasteString.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		50.
				#label 			-> 		'Paste...'.
				#selector 		-> 		#pasteRecent.
				#icon 			-> 		#worldIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Toggle WordWrap'.
				#selector 		-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Set Font... (k)'.
				#selector 		-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Clear Font'.
				#selector 		-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'Set Default Font...'.
				#selector 		-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`
! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'HAW 7/10/2018 18:57:42'!
smalltalkEditorMenuOptions
	
	^`{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Do it (d)'.
				#selector 		-> 		#doIt.
				#icon 			-> 		#doItIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Print it (p)'.
				#selector 		-> 		#printIt.
				#icon 			-> 		#printIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'Inspect it (i)'.
				#selector 		-> 		#inspectIt.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'Explore it (I)'.
				#selector 		-> 		#exploreIt.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'Debug it (D)'.	
				#selector 		-> 		#debugIt.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'Profile it'.	
				#selector 		-> 		#profileIt.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Explain'.
				#selector 		-> 		#explain.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Browse it (b)'.
				#selector 		-> 		#browseIt.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'Senders of it (n)'.
				#selector 		-> 		#sendersOfIt.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		40.
				#label 			-> 		'Implementors of it (m)'.
				#selector 		-> 		#implementorsOfIt.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		50.
				#label 			-> 		'References to it (N)'.
				#selector 		-> 		#referencesToIt.
				#icon 			-> 		#addressBookIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Method Strings with it (E)'.
				#selector 		-> 		#methodStringsContainingit.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Method Source with it'.
				#selector 		-> 		#methodSourceContainingIt.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Class Comments with it'.
				#selector 		-> 		#classCommentsContainingIt.
				#icon 			-> 		#chatIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'Accept (s)'.
				#selector 		-> 		#acceptContents.
				#icon 			-> 		#acceptIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'Cancel (l)'.
				#selector 		-> 		#cancelEdits.
				#icon 			-> 		#cancelIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'Accept & Run Test in Class (t)'.
				#selector 		-> 		#acceptAndTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'Accept & Run Test in Category (y)'.
				#selector 		-> 		#acceptAndTestAll.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		50.
				#label 			-> 		'Accept & Debug Test (r)'.
				#selector 		-> 		#acceptAndDebugTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`! !


!SmalltalkEditor methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:08:33'!
getMenu
	
	^self createMenuCollectingOptionsWith: #smalltalkEditorMenuOptions! !

!SmalltalkEditor methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:09:25'!
openMenu
	
	self openMenu: self getMenu 
	! !

!SmalltalkEditor methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:09:34'!
openMenu2
	
	self openMenu: self getMenu2
	! !


!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt)
('typing/selecting keys' argAdvance: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier selectWord)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('*Refactoring' renameTemporary)
!

