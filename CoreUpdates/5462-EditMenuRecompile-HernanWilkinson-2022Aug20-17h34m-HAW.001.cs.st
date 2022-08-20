'From Cuis 6.0 [latest update: #5461] on 20 August 2022 at 5:35:18 pm'!

!SmalltalkEditor class methodsFor: 'menu' stamp: 'HAW 8/20/2022 17:34:38'!
editMenuLabel

	^'Edit'! !


!SmalltalkEditor class methodsFor: 'menu' stamp: 'HAW 8/20/2022 17:34:38'!
smalltalkEditorEditingMenuOptions
	
	^`{
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Find...(f)'.
				#selector 		-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'Find Again (g)'.
				#selector 		-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 		-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Undo - multiple (z)'.
				#selector 		-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 		-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'Undo / Redo history'.
				#selector 		-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Copy (c)'.
				#selector 		-> 		#copySelection.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Cut (x)'.
				#selector 		-> 		#cut.
				#icon 			-> 		#cutIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'Paste (v)'.
				#selector 		-> 		#paste.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 			-> 		'Paste without Format'.
				#selector 		-> 		#pasteString.
				#icon 			-> 		#pasteIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label 			-> 		'Paste...'.
				#selector 		-> 		#pasteRecent.
				#icon 			-> 		#worldIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Toggle WordWrap'.
				#selector 		-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Set Font... (k)'.
				#selector 		-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Clear Font'.
				#selector 		-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'Set Default Font...'.
				#selector 		-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
		}`
! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'HAW 8/20/2022 17:34:38'!
smalltalkEditorMenuOptions
	
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Help...'.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Do it (d)'.
				#selector 		-> 		#doIt.
				#icon 			-> 		#doItIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'Print it (p)'.
				#selector 		-> 		#printIt.
				#icon 			-> 		#printIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'Inspect it (i)'.
				#selector 		-> 		#inspectIt.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'Explore it (I)'.
				#selector 		-> 		#exploreIt.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'Debug it (D)'.
				#selector 		-> 		#debugIt.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'Profile it'.
				#selector 		-> 		#profileIt.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		5.
				#label 			-> 	SmalltalkEditor editMenuLabel.
				#selector 		-> 		#yourself.
				#icon 			-> 		#textEditorIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Explain'.
				#selector 		-> 		#explain.
				#icon 			-> 		#helpIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Browse it (b)'.
				#selector 		-> 		#browseIt.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'Senders of it (n)'.
				#selector 		-> 		#sendersOfIt.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 			-> 		'Implementors of it (m)'.
				#selector 		-> 		#implementorsOfIt.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label 			-> 		'References to it (N)'.
				#selector 		-> 		#referencesToIt.
				#icon 			-> 		#addressBookIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Method Strings with it (E)'.
				#selector 		-> 		#methodStringsContainingit.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'Method Source with it'.
				#selector 		-> 		#methodSourceContainingIt.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'Class Comments with it'.
				#selector 		-> 		#classCommentsContainingIt.
				#icon 			-> 		#chatIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'Accept (s)'.
				#selector 		-> 		#acceptContents.
				#icon 			-> 		#acceptIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'Accept & Run Test in Class (t)'.
				#selector 		-> 		#acceptAndTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'Accept & Run Test in Category (y)'.
				#selector 		-> 		#acceptAndTestAll.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		50.
				#label 			-> 		'Accept & Debug Test (r)'.
				#selector 		-> 		#acceptAndDebugTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}, SmalltalkEditor smalltalkEditorEditingMenuOptions `! !

!methodRemoval: SmalltalkEditor class #editingMenuLabel stamp: 'HAW 8/20/2022 17:34:38'!
SmalltalkEditor class removeSelector: #editingMenuLabel!

!SmalltalkEditor class reorganize!
('keyboard shortcut tables' cmdShortcutsSpec)
('menu' editMenuLabel smalltalkEditorEditingMenuOptions smalltalkEditorMenuOptions)
('cached state access' releaseClassCachedState)
('explain' plateA plateB plateX)
!

