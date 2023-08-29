'From Cuis 6.0 [latest update: #5987] on 29 August 2023 at 6:45:07 pm'!

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 8/29/2023 17:06:13'!
family
	^ family! !


!TextEditor methodsFor: 'menu' stamp: 'jmv 8/29/2023 17:38:56'!
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
				#label 			-> 		'Clear Font'.
				#selector 			-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 			-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Set Style'.
				#selector 			-> 		#offerFontStyleMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Set Text Color'.
				#selector 			-> 		#offerColorMenu.
				#icon 			-> 		#graphicsIcon
			} asDictionary.
			{
				#label 			-> 		'Set Alignment...'.
				#selector 			-> 		#chooseAlignment.
				#icon 			-> 		#formatJustifyLeftIcon
			} asDictionary.
		}`.
		^aMenu! !


!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 8/29/2023 17:36:34'!
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
				#itemOrder 			-> 		30.
				#label 			-> 		'Clear Font'.
				#selector 			-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#submenuOf -> SmalltalkEditor editMenuLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Set Font... (k)'.
				#selector 			-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
		}`.! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 8/29/2023 17:36:40'!
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
		}, SmalltalkEditor smalltalkEditorEditingMenuOptions `.! !


!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 8/29/2023 17:06:00'!
familyName
	^ family familyName! !

!methodRemoval: AbstractFont class #fromUser stamp: 'jmv 8/29/2023 18:42:24'!
AbstractFont class removeSelector: #fromUser!
!methodRemoval: TextEditor #offerDefaultFontMenu stamp: 'jmv 8/29/2023 18:41:41'!
TextEditor removeSelector: #offerDefaultFontMenu!
