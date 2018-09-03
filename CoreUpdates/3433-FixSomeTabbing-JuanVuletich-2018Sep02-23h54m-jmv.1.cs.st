'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 3 September 2018 at 12:02:50 am'!

!Collection methodsFor: 'converting' stamp: 'jmv 9/3/2018 00:00:57'!
asDictionary
	"Answer a Dictionary. Assume our elements are Associations.
	{
			#itemGroup 		-> 	10.
			#itemOrder 		-> 	30.
			#label 			-> 	'Browser'.
			#object 			-> 	BrowserWindow.
			#selector 		-> 	#openBrowser.
			#icon 			-> 	#editFindReplaceIcon.
			#balloonText 	-> 	'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary
	"

	^ self as: Dictionary! !


!SmalltalkEditor class methodsFor: 'menu' stamp: 'HAW 7/10/2018 19:03:02'!
smalltalkEditorMenu2Options
	
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Find...(f)'.
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

!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 9/2/2018 23:59:47'!
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
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'More...'.
				#selector 		-> 		#openMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`! !


!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/7/2018 18:45:13'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	^DynamicMenuBuilder 
		buildTitled: 'Message List' 
		targeting: self 
		collectingMenuOptionsWith: #messageListMenu2Options 
		changingThemWith: [ :options |
			self addExtraMenu2ItemsTo: options.
			model canShowMultipleMessageCategories ifTrue: [	
				options add: `{ 
					#itemGroup 		-> 		40.
					#itemOrder 		-> 		45.		
					#label 			-> 		'show category (C)'.
					#object 			-> 		#model.
					#selector 		-> 		#showHomeCategory.
					#icon 			-> 		#packageIcon
				} asDictionary` ]].
		
	! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/6/2018 12:21:10'!
classListMenu2Options

	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'unsent methods'.
				#selector 		-> 		#browseUnusedMethods.
				#icon 			->			#junkIcon.
				#balloonText 	-> 		'browse all methods defined by this class that have no senders'
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'unreferenced inst vars'.
				#selector 		-> 		#showUnreferencedInstVars.
				#icon 			->			#junkIcon.
				#balloonText 	-> 		'show a list of all instance variables that are not referenced in methods'
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'unreferenced class vars'.
				#selector 		-> 		#showUnreferencedClassVars.
				#icon 			->			#junkIcon.
				#balloonText 	-> 		'show a list of all class variables that are not referenced in methods'
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'subclass template'.
				#object 			-> 		#model.
				#selector 		-> 		#makeNewSubclass.
				#icon 			->			#classIcon.
				#balloonText 	-> 		'put a template into the code pane for defining of a subclass of this class'
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			->			#instanceIcon.
				#balloonText 	-> 		'give me a sample instance of this class, if possible'
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			->			#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class'
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			->			#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class and of all of its subclasses'
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'create inst var accessors'.
				#object 			-> 		#model.
				#selector 		-> 		#createInstVarAccessors.
				#icon 			->			#sendReceiveIcon.
				#balloonText 	-> 		'compile instance-variable access methods for any instance variables that do not yet have them'
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'more...'.
				#selector 		-> 		#offerClassListMenu.
				#icon 			->			#listAddIcon.
				#balloonText 	-> 		'return to the standard class-list menu'
			} asDictionary.
		}`! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 8/3/2018 14:47:08'!
classListMenuOptions

	^ `{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		5.
				#label 			-> 		'show classes hierarchically'.
				#object 			-> 		#model.
				#selector 		-> 		#enableListClassesHierarchically.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		6.
				#label 			-> 		'show classes alphabetically'.
				#object 			-> 		#model.
				#selector 		-> 		#enableListClassesAlphabetically.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'show hierarchy'.
				#object 			-> 		#model.
				#selector 		-> 		#hierarchy.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'show definition'.
				#object 			-> 		#model.
				#selector 		-> 		#editClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'show comment'.
				#object 			-> 		#model.
				#selector 		-> 		#editComment.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'class vars'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename class ...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'copy class...'.
				#object 			-> 		#model.
				#selector 		-> 		#copyClass.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove class (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 		-> 		70.
				#itemOrder 		-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runClassTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		70.
				#itemOrder 		-> 		20.
				#label 			-> 		'more...'.
				#selector 		-> 		#offerClassListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/6/2018 12:24:58'!
messageCategoryMenuOptions

	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut (o)'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessageCategories.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'reorganize'.
				#object 			-> 		#model.
				#selector 		-> 		#editMessageCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'alphabetize'.
				#object 			-> 		#model.
				#selector 		-> 		#alphabetizeMessageCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove empty categories'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyCategories.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'categorize all uncategorized'.
				#object 			-> 		#model.
				#selector 		-> 		#categorizeAllUncategorizedMethods.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'new category...'.
				#object 			-> 		#model.
				#selector 		-> 		#addCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'remove (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessageCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runMessageCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/7/2018 18:22:02'!
messageListMenu2Options
	
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

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 8/11/2018 18:51:50'!
messageListMenuOptions
	
	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'what to show...'.
				#selector 		-> 		#offerWhatToShowMenu.
				#icon 			-> 		#preferencesIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'browse method (O)'.
				#selector 		-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect CompiledMethod'.
				#object 			-> 		#model.
				#selector 		-> 		#inspectCompiledMethod.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'senders of... (n)'.
				#selector 		-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'implementors of... (m)'.
				#selector 		-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'inheritance (i)'.
				#selector 		-> 		#methodHierarchy.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'versions (v)'.
				#selector 		-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'class variables'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		50.
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'debug test (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#debugMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		70.
				#itemOrder 		-> 		10.
				#label 			-> 		'more...'.
				#selector 		-> 		#openMessageListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/7/2018 19:04:27'!
systemCatSingletonMenuOptions

	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'find class... (f)'.
				#selector 		-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse all'.
				#selector 		-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse'.
				#selector 		-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'update'.
				#object 			-> 		#model.
				#selector 		-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSystemCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/7/2018 19:02:29'!
systemCategoryMenuOptions

	^`{
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'find class... (f)'.
				#selector 		-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'recent classes... (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#recent.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse all'.
				#selector 		-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse'.
				#selector 		-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'reorganize'.
				#object 			-> 		#model.
				#selector 		-> 		#editSystemCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'alphabetize'.
				#object 			-> 		#model.
				#selector 		-> 		#alphabetizeSystemCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'update'.
				#object 			-> 		#model.
				#selector 		-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'add item...'.
				#object 			-> 		#model.
				#selector 		-> 		#addSystemCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSystemCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'move to top'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryTop.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'move up'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryUp.
				#icon 			-> 		#goUpIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'move down'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryDown.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		40.
				#label 			-> 		'move to bottom'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryBottom.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#itemGroup 		-> 		70.
				#itemOrder 		-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runSystemCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	! !


!MessageSetWindow methodsFor: 'menu building' stamp: 'HAW 7/7/2018 18:31:51'!
addExtraMenu2ItemsTo: options
	"The shifted selector-list menu is being built.  Add items specific to MessageSet"

	model growable ifTrue: [
		options add: `{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		31.		
			#label 			-> 		'remove from this browser'.
			#object 			-> 		#model.
			#selector 		-> 		#removeMessageFromBrowser.
			#icon 			-> 		#listRemoveIcon 
		} asDictionary`.
		options add:`{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		32.		
			#label 			-> 		'filter message list...'.
			#selector 		-> 		#filterMessageList.
			#icon 			-> 		#findIcon
		} asDictionary`	].
	
	options add: `{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		33.		
			#label 			-> 		'sort by date'.
			#object 			-> 		#model.	
			#selector 		-> 		#sortByDate.
			#icon 			-> 		#dateIcon
		} asDictionary`.

! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/5/2018 18:37:46'!
worldMenuOptions

	^`{{
		#itemGroup 		-> 		10.
		#itemOrder 		-> 		10.
		#label 			-> 		'Open...'.
		#selector 		-> 		#openOpenMenu.
		#icon 			-> 		#openIcon.
		#balloonText 	-> 		'Submenu to open various system tools'.
	} asDictionary.
	{
		#itemGroup 		-> 		10.
		#itemOrder 		-> 		20.
		#label 			-> 		'New morph...'.
		#selector 		-> 		#newMorph.
		#icon 			-> 		#morphsIcon.
		#balloonText 	-> 		'Offers a variety of ways to create new objects'.
	} asDictionary.
	{
		#itemGroup 		-> 		10.
		#itemOrder 		-> 		30.
		#label 			-> 		'Preferences...'.
		#selector 		-> 		#openPreferencesMenu.
		#icon 			-> 		#preferencesIcon.
		#balloonText 	-> 		'Opens a menu offering many controls over appearance and system preferences.'.
	} asDictionary.
				{
		#itemGroup 		-> 		10.
		#itemOrder 		-> 		40.
		#label 			-> 		'Windows...'.
		#selector 		-> 		#openWindowsMenu.
		#icon 			-> 		#windowIcon.
		#balloonText 	-> 		'Manage open windows'.
	} asDictionary.
	{
		#itemGroup 		-> 		10.
		#itemOrder 		-> 		50.
		#label 			-> 		'Help...'.
		#selector 		-> 		#openHelpMenu.
		#icon 			-> 		#helpIcon.
		#balloonText 	-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
	} asDictionary.
	{
		#itemGroup 		-> 		20.
		#itemOrder 		-> 		10.
		#label 			-> 		'Changes...'.
		#selector 		-> 		#openChangesMenu.
		#icon 			-> 		#changesIcon.
		#balloonText 	-> 		'Opens a menu of useful tools for dealing with changes'.
	} asDictionary.
	{
		#itemGroup 		-> 		20.
		#itemOrder 		-> 		20.
		#label 			-> 		'Debug...'.
		#selector 		-> 		#openDebugMenu.
		#icon 			-> 		#debugIcon.
		#balloonText 	-> 		'a menu of debugging items'.
	} asDictionary.
	{
		#itemGroup 		-> 		20.
		#itemOrder 		-> 		30.
		#label 			-> 		'Restore Display (r)'.
		#object 			-> 		#myWorld.
		#selector 		-> 		#restoreDisplay.
		#icon 			-> 		#displayIcon.
		#balloonText 	-> 		'Repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'.
	} asDictionary.
	{
		#itemGroup 		-> 		30.
		#itemOrder 		-> 		10.
		#label 			-> 		'Save'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveSession.
		#icon 			-> 		#saveIcon.
		#balloonText 	-> 		'Save the current version of the image on disk'.
	} asDictionary.
	{
		#itemGroup 		-> 		30.
		#itemOrder 		-> 		20.
		#label 			-> 		'Save as...'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveAs.
		#icon 			-> 		#saveAsIcon.
		#balloonText 	-> 		'Save the current version of the image on disk under a new name.'.
	} asDictionary.
	{
		#itemGroup 		-> 		30.
		#itemOrder 		-> 		30.
		#label 			-> 		'Save as New Version'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveAsNewVersion.
		#icon 			-> 		#saveAsNewVersionIcon.
		#balloonText 	-> 		('Save the current image a new version-stamped name\and save it under that name on disk.\',
										'Clear all user preferences and user state (class vars).') withNewLines.
	} asDictionary.
	{
		#itemGroup 		-> 		30.
		#itemOrder 		-> 		40.
		#label 			-> 		'Save and Quit'.
		#selector 		-> 		#saveAndQuit.
		#icon 			-> 		#saveAndQuitIcon.
		#balloonText 	-> 		'Save the image and quit out of Cuis.'.
	} asDictionary.
	{
		#itemGroup 		-> 		30.
		#itemOrder 		-> 		50.
		#label 			-> 		'Quit'.
		#selector 		-> 		#quitSession.
		#icon 			-> 		#quitIcon.
		#balloonText 	-> 		'Quit out of Cuis.'.
	} asDictionary.
}`! !

