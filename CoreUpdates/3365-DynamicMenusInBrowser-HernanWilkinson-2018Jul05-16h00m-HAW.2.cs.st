'From Cuis 5.0 of 7 November 2016 [latest update: #3358] on 7 July 2018 at 7:15:34 pm'!
!classDefinition: #DynamicMenuBuilder category: #'Morphic-Menus'!
Object subclass: #DynamicMenuBuilder
	instanceVariableNames: 'title defaultTarget menuOptionsSelector menu items groups optionsChanger '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/6/2018 12:21:10'!
classListMenu2Options

	^`{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'unsent methods'.
				#selector 		-> 		#browseUnusedMethods.
				#icon 			->		#junkIcon.
				#balloonText 	-> 		'browse all methods defined by this class that have no senders'
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'unreferenced inst vars'.
				#selector 		-> 		#showUnreferencedInstVars.
				#icon 			->		#junkIcon.
				#balloonText 	-> 		'show a list of all instance variables that are not referenced in methods'
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'unreferenced class vars'.
				#selector 		-> 		#showUnreferencedClassVars.
				#icon 			->		#junkIcon.
				#balloonText 	-> 		'show a list of all class variables that are not referenced in methods'
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'subclass template'.
				#object 			-> 		#model.
				#selector 		-> 		#makeNewSubclass.
				#icon 			->		#classIcon.
				#balloonText 	-> 		'put a template into the code pane for defining of a subclass of this class'
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			->		#instanceIcon.
				#balloonText 	-> 		'give me a sample instance of this class, if possible'
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			->		#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class'
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			->		#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class and of all of its subclasses'
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'create inst var accessors'.
				#object 			-> 		#model.
				#selector 		-> 		#createInstVarAccessors.
				#icon 			->		#sendReceiveIcon.
				#balloonText 	-> 		'compile instance-variable access methods for any instance variables that do not yet have them'
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'more...'.
				#selector 		-> 		#offerClassListMenu.
				#icon 			->		#listAddIcon.
				#balloonText 	-> 		'return to the standard class-list menu'
			} asDictionary.
		}`! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/6/2018 11:58:54'!
classListMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'show hierarchy'.
				#object 			-> 		#model.
				#selector 		-> 		#hierarchy.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'show definition'.
				#object 			-> 		#model.
				#selector 		-> 		#editClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'show comment'.
				#object 			-> 		#model.
				#selector 		-> 		#editComment.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'class vars'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename class ...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'copy class...'.
				#object 			-> 		#model.
				#selector 		-> 		#copyClass.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove class (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		70.
				#itemOrder 		-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runClassTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		70.
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
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut (o)'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessageCategories.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'reorganize'.
				#object 			-> 		#model.
				#selector 		-> 		#editMessageCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'alphabetize'.
				#object 			-> 		#model.
				#selector 		-> 		#alphabetizeMessageCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove empty categories'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyCategories.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'categorize all uncategorized'.
				#object 			-> 		#model.
				#selector 		-> 		#categorizeAllUncategorizedMethods.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'new category...'.
				#object 			-> 		#model.
				#selector 		-> 		#addCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'remove (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessageCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
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
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'implementors of sent messages'.
				#selector 		-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'local senders of...'.
				#selector 		-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'local implementors of...'.
				#selector 		-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'spawn sub-protocol'.
				#selector 		-> 		#browseProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'spawn full protocol'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#speadsheetTemplateIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'change category...'.
				#object 			-> 		#model.
				#selector 		-> 		#changeCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		50.
				#label 			-> 		'change sets with this method'.
				#selector 		-> 		#findMethodInChangeSets.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		60.
				#label 			-> 		'revert to previous version'.
				#object 			-> 		#model.
				#selector 		-> 		#revertToPreviousVersion.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'more...'.
				#selector 		-> 		#openMessageListMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.			
		}`! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 7/7/2018 19:10:48'!
messageListMenuOptions
	
	^`{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'what to show...'.
				#selector 		-> 		#offerWhatToShowMenu.
				#icon 			-> 		#preferencesIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'browse method (O)'.
				#selector 		-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'explore CompiledMethod'.
				#object 			-> 		#model.
				#selector 		-> 		#exploreCompiledMethod.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'senders of... (n)'.
				#selector 		-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'implementors of... (m)'.
				#selector 		-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		30.
				#label 			-> 		'inheritance (i)'.
				#selector 		-> 		#methodHierarchy.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		40.
				#label 			-> 		'versions (v)'.
				#selector 		-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'class variables'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		50.
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'debug test (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#debugMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		70.
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
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'find class... (f)'.
				#selector 		-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse all'.
				#selector 		-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse'.
				#selector 		-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'update'.
				#object 			-> 		#model.
				#selector 		-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
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
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'find class... (f)'.
				#selector 		-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'recent classes... (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#recent.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse all'.
				#selector 		-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse'.
				#selector 		-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'reorganize'.
				#object 			-> 		#model.
				#selector 		-> 		#editSystemCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'alphabetize'.
				#object 			-> 		#model.
				#selector 		-> 		#alphabetizeSystemCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'update'.
				#object 			-> 		#model.
				#selector 		-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'add item...'.
				#object 			-> 		#model.
				#selector 		-> 		#addSystemCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		40.
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSystemCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'move to top'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryTop.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'move up'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryUp.
				#icon 			-> 		#goUpIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'move down'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryDown.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		40.
				#label 			-> 		'move to bottom'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryBottom.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#itemGroup 	-> 		70.
				#itemOrder 		-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runSystemCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	! !


!DynamicMenuBuilder methodsFor: 'initialization' stamp: 'HAW 7/7/2018 18:43:12'!
initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock

	title := aTitle.
	defaultTarget := aDefaultTarget.
	menuOptionsSelector := aMenuOptionsSelector.
	optionsChanger := anOptionsChangerBlock ! !

!DynamicMenuBuilder methodsFor: 'testing' stamp: 'HAW 7/5/2018 18:26:14'!
hasTitle

	^title ~= self class noTitle ! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/7/2018 18:40:31'!
changeOptions

	optionsChanger value: items! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 18:41:03'!
classesProvidingMenuOptions

	^(Smalltalk allClassesImplementing: menuOptionsSelector) select: [ :aClass | aClass isMeta ]! !


!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/5/2018 18:25:03'!
targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^self titled: self noTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

! !

!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/7/2018 18:42:10'!
titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock

	^self new initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock

! !

!DynamicMenuBuilder class methodsFor: 'defaults' stamp: 'HAW 7/5/2018 18:25:09'!
noTitle

	^nil! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/5/2018 18:27:20'!
buildTargeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^(self targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector) build! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/5/2018 18:27:45'!
buildTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^(self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector) build! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/7/2018 18:42:56'!
buildTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock

	^(self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock) build! !


!TheWorldMenu methodsFor: 'construction' stamp: 'HAW 7/6/2018 12:14:46'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	^(DynamicMenuBuilder buildTargeting: self collectingMenuOptionsWith: #worldMenuOptions)
		addStayUpIcons;
		yourself
	
	! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/5/2018 18:37:46'!
worldMenuOptions

	^`{{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		10.
		#label 			-> 		'Open...'.
		#selector 		-> 		#openOpenMenu.
		#icon 			-> 		#openIcon.
		#balloonText 	-> 		'Submenu to open various system tools'.
	} asDictionary.
	{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		20.
		#label 			-> 		'New morph...'.
		#selector 		-> 		#newMorph.
		#icon 			-> 		#morphsIcon.
		#balloonText 	-> 		'Offers a variety of ways to create new objects'.
	} asDictionary.
	{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		30.
		#label 			-> 		'Preferences...'.
		#selector 		-> 		#openPreferencesMenu.
		#icon 			-> 		#preferencesIcon.
		#balloonText 	-> 		'Opens a menu offering many controls over appearance and system preferences.'.
	} asDictionary.
				{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		40.
		#label 			-> 		'Windows...'.
		#selector 		-> 		#openWindowsMenu.
		#icon 			-> 		#windowIcon.
		#balloonText 	-> 		'Manage open windows'.
	} asDictionary.
	{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		50.
		#label 			-> 		'Help...'.
		#selector 		-> 		#openHelpMenu.
		#icon 			-> 		#helpIcon.
		#balloonText 	-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
	} asDictionary.
	{
		#itemGroup 	-> 		20.
		#itemOrder 		-> 		10.
		#label 			-> 		'Changes...'.
		#selector 		-> 		#openChangesMenu.
		#icon 			-> 		#changesIcon.
		#balloonText 	-> 		'Opens a menu of useful tools for dealing with changes'.
	} asDictionary.
	{
		#itemGroup 	-> 		20.
		#itemOrder 		-> 		20.
		#label 			-> 		'Debug...'.
		#selector 		-> 		#openDebugMenu.
		#icon 			-> 		#debugIcon.
		#balloonText 	-> 		'a menu of debugging items'.
	} asDictionary.
	{
		#itemGroup 	-> 		20.
		#itemOrder 		-> 		30.
		#label 			-> 		'Restore Display (r)'.
		#object 			-> 		#myWorld.
		#selector 		-> 		#restoreDisplay.
		#icon 			-> 		#displayIcon.
		#balloonText 	-> 		'Repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		10.
		#label 			-> 		'Save'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveSession.
		#icon 			-> 		#saveIcon.
		#balloonText 	-> 		'Save the current version of the image on disk'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		20.
		#label 			-> 		'Save as...'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveAs.
		#icon 			-> 		#saveAsIcon.
		#balloonText 	-> 		'Save the current version of the image on disk under a new name.'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		30.
		#label 			-> 		'Save as New Version'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveAsNewVersion.
		#icon 			-> 		#saveAsNewVersionIcon.
		#balloonText 	-> 		('Save the current image a new version-stamped name\and save it under that name on disk.\',
									'Clear all user preferences and user state (class vars).') withNewLines.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		40.
		#label 			-> 		'Save and Quit'.
		#selector 		-> 		#saveAndQuit.
		#icon 			-> 		#saveAndQuitIcon.
		#balloonText 	-> 		'Save the image and quit out of Cuis.'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		50.
		#label 			-> 		'Quit'.
		#selector 		-> 		#quitSession.
		#icon 			-> 		#quitIcon.
		#balloonText 	-> 		'Quit out of Cuis.'.
	} asDictionary.
}`! !


!Workspace class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:15:27'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 		-> 		20.
			#label 			->		'Workspace'.
			#object 			-> 		Workspace.
			#selector 		-> 		#openWorkspace.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary}`! !


!Transcripter class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:15:19'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		30.
			#itemOrder 		-> 		20.
			#label 			->		'Emergency Evaluator'.
			#object 			-> 		Transcripter.
			#selector 		-> 		#emergencyEvaluator.
			#icon 			-> 		#emblemImportantIcon.
			#balloonText 	-> 		'When all else fails...'.
		} asDictionary}`! !


!TextEditor class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:14:50'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 		-> 		10.
			#label 			->		'Text Editor'.
			#object 			-> 		TextEditor.
			#selector 		-> 		#openTextEditor.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary}`! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'HAW 7/5/2018 18:24:37'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: Preferences desktopMenuTitle.
	menu popUpInWorld: self! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:13:05'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		20.
			#itemOrder 		-> 		10.
			#label 			->		'Installed Packages'.
			#object 			-> 		CodePackageListWindow.
			#selector 		-> 		#openPackageList.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`! !


!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/7/2018 18:32:00'!
addExtraMenu2ItemsTo: optoins
	"The shifted selector-list menu is being built; some menu items are appropriate only for certain kinds of browsers, and this gives a hook for them to be added as approrpiate.  If any is added here, a line should be added first -- browse reimplementors of this message for examples."! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/6/2018 11:59:53'!
classListMenu
	"Set up the menu to apply to the receiver's class list, honoring the #shifted boolean"

	^DynamicMenuBuilder buildTitled: 'Class List' targeting: self collectingMenuOptionsWith: #classListMenuOptions.! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/6/2018 12:18:34'!
classListMenu2
	"Set up the menu to apply to the receiver's class list when the shift key is down"

	^DynamicMenuBuilder buildTargeting: self collectingMenuOptionsWith: #classListMenu2Options.
	! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/6/2018 12:22:51'!
messageCategoryMenu

	^DynamicMenuBuilder buildTitled: 'Message Category' targeting: self collectingMenuOptionsWith: #messageCategoryMenuOptions.
! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/7/2018 19:09:07'!
messageListMenu
	"Answer the message-list menu"
	"Changed by emm to include menu-item for breakpoints"

	^DynamicMenuBuilder buildTitled: 'Message List' targeting: self collectingMenuOptionsWith: #messageListMenuOptions.
	! !

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
					#itemGroup 	-> 		40.
					#itemOrder 		-> 		45.		
					#label 			-> 		'show category (C)'.
					#object 			-> 		#model.
					#selector 		-> 		#showHomeCategory.
					#icon 			-> 		#packageIcon
				} asDictionary` ]].
		
	! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/7/2018 18:57:48'!
systemCatSingletonMenu

	^DynamicMenuBuilder buildTitled: 'Class category' targeting: self collectingMenuOptionsWith: #systemCatSingletonMenuOptions.
! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 7/7/2018 19:03:15'!
systemCategoryMenu

	^DynamicMenuBuilder buildTitled: 'Class category' targeting: self collectingMenuOptionsWith: #systemCategoryMenuOptions.! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/6/2018 11:53:52'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 		-> 		30.
			#label 			->		'Browser'.
			#object 			-> 		BrowserWindow.
			#selector 		-> 		#openBrowser.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !


!MessageSetWindow methodsFor: 'menu building' stamp: 'HAW 7/7/2018 18:31:51'!
addExtraMenu2ItemsTo: options
	"The shifted selector-list menu is being built.  Add items specific to MessageSet"

	model growable ifTrue: [
		options add: `{
			#itemGroup 	-> 		40.
			#itemOrder 		-> 		31.		
			#label 			-> 		'remove from this browser'.
			#object 			-> 		#model.
			#selector 		-> 		#removeMessageFromBrowser.
			#icon 			-> 		#listRemoveIcon 
		} asDictionary`.
		options add:`{
			#itemGroup 	-> 		40.
			#itemOrder 		-> 		32.		
			#label 			-> 		'filter message list...'.
			#selector 		-> 		#filterMessageList.
			#icon 			-> 		#findIcon
		} asDictionary`	].
	
	options add: `{
			#itemGroup 	-> 		40.
			#itemOrder 		-> 		33.		
			#label 			-> 		'sort by date'.
			#object 			-> 		#model.	
			#selector 		-> 		#sortByDate.
			#icon 			-> 		#dateIcon
		} asDictionary`.

! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:13:59'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 		-> 		40.
			#label 			->		'Message Names'.
			#object 			-> 		MessageNamesWindow.
			#selector 		-> 		#openMessageNames.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:12:50'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		20.
			#itemOrder 		-> 		20.
			#label 			->		'Change Sorter'.
			#object 			-> 		ChangeSorterWindow.
			#selector 		-> 		#openChangeSorter.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:13:27'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		40.
			#itemOrder 		-> 		10.
			#label 			->		'File List'.
			#object 			-> 		FileListWindow.
			#selector 		-> 		#openFileList.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary}`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:14:22'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		30.
			#itemOrder 		-> 		10.
			#label 			->		'Process Browser'.
			#object 			-> 		ProcessBrowserWindow.
			#selector 		-> 		#openProcessBrowser.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:14:27'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		40.
			#itemOrder 		-> 		20.
			#label 			->		'SUnit Test Runner'.
			#object 			-> 		TestRunnerWindow.
			#selector 		-> 		#openTestRunner.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary}`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:15:13'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 		-> 		50.
			#label 			->		'Transcript'.
			#object 			-> 		TranscriptWindow.
			#selector 		-> 		#openTranscript.
			#icon 			-> 		#printerIcon.
			#balloonText 	-> 		'A window showing contents of the System Transcript'.
		} asDictionary}`! !


!DynamicMenuBuilder methodsFor: 'building' stamp: 'HAW 7/7/2018 18:40:16'!
build

	self 
		createMenu;
	 	collectMenuOptions;
		changeOptions;
		addGroupedMenuOptionsToMenu.		
			
	^ menu.! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 18:41:32'!
collectMenuOptions

	items := OrderedCollection new.
	self classesProvidingMenuOptions do: [ :aClass | items addAll: (aClass soleInstance perform: menuOptionsSelector) ].
	! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/6/2018 12:15:07'!
createMenu

	menu := self hasTitle ifTrue: [ MenuMorph entitled: title] ifFalse: [ MenuMorph new ].
	menu defaultTarget: defaultTarget! !


!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/7/2018 18:41:32'!
titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: [ :options | ]! !


!TheWorldMenu methodsFor: 'construction' stamp: 'HAW 7/6/2018 12:14:08'!
openMenu
	"Build the open window menu for the world."

	^(DynamicMenuBuilder buildTitled: 'Open...' targeting: self collectingMenuOptionsWith: #worldMenuForOpenGroup) 
		addStayUpIcons;
		yourself
	! !

!methodRemoval: DynamicMenuBuilder class #buildTitled:targeting:collectingMenuOptionsWith:adding:!
DynamicMenuBuilder class removeSelector: #buildTitled:targeting:collectingMenuOptionsWith:adding:!
!methodRemoval: DynamicMenuBuilder class #buildTitled:targeting:collectingMenuOptionsWith:changingOptionsWith:!
DynamicMenuBuilder class removeSelector: #buildTitled:targeting:collectingMenuOptionsWith:changingOptionsWith:!
!methodRemoval: DynamicMenuBuilder class #titled:targeting:collectingMenuOptionsWith:adding:!
DynamicMenuBuilder class removeSelector: #titled:targeting:collectingMenuOptionsWith:adding:!
!methodRemoval: DynamicMenuBuilder class #titled:targeting:collectingMenuOptionsWith:changingOptionsWith:!
DynamicMenuBuilder class removeSelector: #titled:targeting:collectingMenuOptionsWith:changingOptionsWith:!
!methodRemoval: DynamicMenuBuilder #addOptionalOptions!
DynamicMenuBuilder removeSelector: #addOptionalOptions!
!methodRemoval: DynamicMenuBuilder #initializeTitled:targeting:collectingMenuOptionsWith:!
DynamicMenuBuilder removeSelector: #initializeTitled:targeting:collectingMenuOptionsWith:!
!methodRemoval: DynamicMenuBuilder #initializeTitled:targeting:collectingMenuOptionsWith:adding:!
DynamicMenuBuilder removeSelector: #initializeTitled:targeting:collectingMenuOptionsWith:adding:!
!methodRemoval: DynamicMenuBuilder #initializeTitled:targeting:collectingMenuOptionsWith:changingOptionsWith:!
DynamicMenuBuilder removeSelector: #initializeTitled:targeting:collectingMenuOptionsWith:changingOptionsWith:!

!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumn buildMorphicClassList buildMorphicCodePane buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('commands')
('keyboard shortcuts' systemCatSingletonKey:from:)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('*Refactoring' addInstVar removeInstVar renameInstVar renameMethod)
('*DenotativeObject-Tools' classNameRequester findClass findClassDialogTitle)
!


!BrowserWindow class reorganize!
('GUI building' fullOnClass:selector: openBrowser)
('browser menues' classListMenu2Options classListMenuOptions messageCategoryMenuOptions messageListMenu2Options messageListMenuOptions systemCatSingletonMenuOptions systemCategoryMenuOptions)
('menu-world' worldMenuForOpenGroup)
!

!classDefinition: #DynamicMenuBuilder category: #'Morphic-Menus'!
Object subclass: #DynamicMenuBuilder
	instanceVariableNames: 'title defaultTarget menuOptionsSelector menu items groups optionsChanger'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!DynamicMenuBuilder class reorganize!
('instance creation' targeting:collectingMenuOptionsWith: titled:targeting:collectingMenuOptionsWith: titled:targeting:collectingMenuOptionsWith:changingThemWith:)
('defaults' noTitle)
('building' buildTargeting:collectingMenuOptionsWith: buildTitled:targeting:collectingMenuOptionsWith: buildTitled:targeting:collectingMenuOptionsWith:changingThemWith:)
!


!TheWorldMenu reorganize!
('action')
('commands' cleanUpWorld fullScreenOff fullScreenOn garbageCollect newMorphOfClass:event: quitSession saveAndQuit saveWorldInFile splitNewMorphList:depth: startMessageTally vmStatistics)
('construction' alphabeticalMorphMenu buildWorldMenu changesMenu debugMenu helpMenu newMorph openMenu preferencesMenu)
('mechanics' menu: myWorld world:hand:)
('popups' doPopUp: openChangesMenu openDebugMenu openHelpMenu openOpenMenu openPreferencesMenu openWindowsMenu)
('windows & flaps menu' windowsMenu)
('menu' openCommentGuide openTerseGuide)
('*DenotativeObject')
!


!TheWorldMenu class reorganize!
('menu building' worldMenuOptions)
!

