'From Cuis6.3 [latest update: #6288] on 21 March 2024 at 11:44:55 am'!

!Workspace class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 11:42:00'!
worldMenuOptions
	^ `{{
			#submenuOf 				-> 	TheWorldMenu openLabel.
			#itemGroup 				-> 	10.
			#itemOrder 				-> 	20.
			#label 				-> 	'Workspace'.
			#object 				-> 	Workspace.
			#selector 				-> 	#open.
			#icon 				-> 	#terminalIcon.
			#balloonText 				-> 	'A window for evaluating Smalltalk expressions'.
		} asDictionary}`! !


!Collection methodsFor: 'converting' stamp: 'jmv 3/21/2024 10:49:13'!
asDictionary
	"Answer a Dictionary. Assume our elements are Associations.
	{
			#itemGroup 			-> 	10.
			#itemOrder 			-> 	30.
			#label 			-> 	'Browser'.
			#object 			-> 	Browser.
			#selector 			-> 	#open.
			#icon 			-> 	#editFindReplaceIcon.
			#balloonText 			-> 	'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary
	"

	^ self as: Dictionary! !


!Transcripter class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 11:42:31'!
worldMenuOptions
	^ `{{
			#submenuOf 				-> 	TheWorldMenu openLabel.
			#itemGroup 				-> 	30.
			#itemOrder 				-> 	20.
			#label 				-> 	'Emergency Evaluator'.
			#object 				-> 	Transcripter.
			#selector 				-> 	#emergencyEvaluator.
			#icon 				-> 	#emblemImportantIcon.
			#balloonText 				-> 	'When all else fails...'.
		} asDictionary}`! !


!TextEditor class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 11:42:46'!
worldMenuOptions
	^ `{{
			#submenuOf 				-> 	TheWorldMenu openLabel.
			#itemGroup 				-> 	10.
			#itemOrder 				-> 	10.
			#label 				-> 	'Text Editor'.
			#object 				-> 	TextEditor.
			#selector 				-> 	#open.
			#icon 				-> 	#textEditorIcon.
			#balloonText 				-> 	'A window for composing text'.
		} asDictionary}`! !


!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 3/21/2024 11:24:01'!
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
				#label 			-> 		SmalltalkEditor editMenuLabel.
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
		}, SmalltalkEditor smalltalkEditorEditingMenuOptions `. ! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 3/21/2024 11:24:17'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 			-> 		[ scrollBar scrollUp: 1 ].
			[ #down ] 			-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 			-> 		[ hScrollBar scrollUp: 1 ].
			[ #right ] 			-> 		[ hScrollBar scrollDown: 1 ] }! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:59:06'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> TheWorldMenu openLabel.
			#itemGroup 			-> 	20.
			#itemOrder 			-> 	10.
			#label 			-> 	'Installed Packages'.
			#object 			-> 	CodePackageList.
			#selector 			-> 	#open.
			#icon 			-> 	#packageIcon.
			#balloonText 			-> 	'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 3/21/2024 10:55:52'!
classListMoreMenuOptions

	^ `{			
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		10.
				#label 				-> 		'unsent methods'.
				#selector 				-> 		#browseUnusedMethods.
				#icon 				-> 		#junkIcon.
				#balloonText 				-> 		'browse all methods defined by this class that have no senders'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		20.
				#label 				-> 		'unreferenced inst vars'.
				#selector 				-> 		#showUnreferencedInstVars.
				#icon 				-> 		#junkIcon.
				#balloonText 				-> 		'show a list of all instance variables that are not referenced in methods'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		30.
				#label 				-> 		'unreferenced class vars'.
				#selector 				-> 		#showUnreferencedClassVars.
				#icon 				-> 		#junkIcon.
				#balloonText 				-> 		'show a list of all class variables that are not referenced in methods'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		10.
				#itemOrder 				-> 		40.
				#label 				-> 		'subclass template'.
				#object 				-> 		#model.
				#selector 				-> 		#makeNewSubclass.
				#icon 				-> 		#classIcon.
				#balloonText 				-> 		'put a template into the code pane for defining of a subclass of this class'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		10.
				#label 				-> 		'sample instance'.
				#selector 				-> 		#makeSampleInstance.
				#icon 				-> 		#instanceIcon.
				#balloonText 				-> 		'give me a sample instance of this class, if possible'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		20.
				#label 				-> 		'inspect instances'.
				#selector 				-> 		#inspectInstances.
				#icon 				-> 		#inspectIcon.
				#balloonText 				-> 		'open an inspector on all the extant instances of this class'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		20.
				#itemOrder 				-> 		30.
				#label 				-> 		'inspect subinstances'.
				#selector 				-> 		#inspectSubInstances.
				#icon 				-> 		#inspectIcon.
				#balloonText 				-> 		'open an inspector on all the extant instances of this class and of all of its subclasses'
			} asDictionary.
			{
				#submenuOf 				-> 		BrowserWindow moreMenuLabel.
				#itemGroup 				-> 		30.
				#itemOrder 				-> 		10.
				#label 				-> 		'create inst var accessors'.
				#object 				-> 		#model.
				#selector 				-> 		#createInstVarAccessors.
				#icon 				-> 		#sendReceiveIcon.
				#balloonText 				-> 		'compile instance-variable access methods for any instance variables that do not yet have them'
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 3/21/2024 11:09:48'!
messageListMenu2Options
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'implementors of sent messages'.
				#selector 			-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'local senders of...'.
				#selector 			-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'local implementors of...'.
				#selector 			-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'spawn sub-protocol'.
				#selector 			-> 		#browseProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'spawn full protocol'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#speadsheetTemplateIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'sample instance'.
				#selector 			-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 			-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 			-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'change category...'.
				#object 			-> 		#model.
				#selector 			-> 		#changeCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		50.
				#label 			-> 		'change sets with this method'.
				#selector 			-> 		#findMethodInChangeSets.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		60.
				#label 			-> 		'revert to previous version'.
				#object 			-> 		#model.
				#selector 			-> 		#revertToPreviousVersion.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'more...'.
				#selector 			-> 		#openMessageListMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.			
		}`! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 3/21/2024 11:10:04'!
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
				#selector       			-> 		#browseCategoryOfCurrentMethod.
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
				#label 			-> 		'remove method in hierarchy'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessageInHierarchy.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		40.
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

!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 3/21/2024 11:10:27'!
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
				#balloonText 			-> 		'Move all classes in this category to another category'       
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

!BrowserWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:59:13'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 		TheWorldMenu openLabel.
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		30.
			#label 			-> 		'Browser'.
			#object 			-> 		Browser.
			#selector 			-> 		#open.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 			-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !


!ChangeListWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:15:05'!
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
				#selector 			-> 		#fileInSelections.
				#balloonText 		-> 		'import the selected items into the image'.
			} asDictionary.
			{
				#label 			-> 		'fileOut selections...	'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutSelections.
				#balloonText 		-> 		'create a new file containing the selected items'.
			} asDictionary.
			{
				#label 			-> 		'fileOut current version of selections...'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutCurrentVersionsOfSelections.
				#balloonText 		-> 		'create a new file containing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'compare to current'.
				#object 			-> 		#model.
				#selector 			-> 		#compareToCurrentVersion.
				#balloonText 		-> 		'open a separate window which shows the text differences between the on-file version and the in-image version.'.
			} asDictionary.
			{
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleDiffing.
				#balloonText 	-> 			'start or stop showing diffs in the code pane.'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select new methods'.
				#object 			-> 		#model.
				#selector 			-> 		#selectNewMethods.
				#balloonText 	-> 			'select methods in the file that do not currently exist in the image'.
			} asDictionary.
			{
				#label 			-> 		'select changes for absent classes'.
				#object 			-> 		#model.
				#selector 			-> 		#selectAllForAbsentClasses.
				#balloonText 	-> 			'select methods in the file for classes that are not defined in the image'.
			} asDictionary.
			{
				#label 			-> 		'select all changes for this class'.
				#object 			-> 		#model.
				#selector 			-> 		#selectAllForThisClass.
				#balloonText 	-> 			'select all methods in the file that belong to the currently-selected class'.
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
				#selector 			-> 		#selectEquivalentMethods.
				#balloonText 	-> 			'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods older than current'.
				#object 			-> 		#model.
				#selector 			-> 		#selectMethodsOlderThanCurrent.
				#balloonText 	-> 			'select methods in the file that are older than the one currently in the image'.
			} asDictionary.
			{
				#label 			-> 		'select removals of sent methods'.
				#object 			-> 		#model.
				#selector 			-> 		#selectRemovalsOfSent.
				#balloonText 	-> 			'select all method removals of methods that have some sender in the image'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select all (a)'.
				#object 			-> 		#model.
				#selector 			-> 		#selectAll.
				#balloonText 	-> 			'select all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'deselect all (A)'.
				#object 			-> 		#model.
				#selector 			-> 		#deselectAll.
				#balloonText 	-> 			'deselect all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'invert selections'.
				#object 			-> 		#model.
				#selector 			-> 		#invertSelections.
				#balloonText 	-> 			'select every item that is not currently selected, and deselect every item that *is* currently selected'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse class and method'.
				#selector 			-> 		#browseMethodFull.
				#balloonText 	-> 			'open a full browser showing the selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse all versions of single selection'.
				#selector 			-> 		#browseVersions.
				#balloonText 	-> 			'open a version browser showing the versions of the currently selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse current versions of selections'.
				#selector 			-> 		#browseCurrentVersionsOfSelections.
				#balloonText 	-> 			'open a message-list browser showing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			{
				#label 			-> 		'destroy current methods of selections'.
				#object 			-> 		#model.
				#selector 			-> 		#destroyCurrentCodeOfSelections.
				#balloonText 	-> 			'remove (*destroy*) the in-image counterparts of all selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove doIts'.
				#object 			-> 		#model.
				#selector 			-> 		#removeDoIts.
				#balloonText 	-> 			'remove all items that are doIts rather than definitions'.
			} asDictionary.
			{
				#label 			-> 		'remove older versions'.
				#object 			-> 		#model.
				#selector 			-> 		#removeOlderMethodVersions.
				#balloonText 	-> 			'remove all but the most recent versions of methods in the list'.
			} asDictionary.
			{
				#label 			-> 		'remove up-to-date versions'.
				#object 			-> 		#model.
				#selector 			-> 		#removeUpToDate.
				#balloonText 	-> 			'remove all items whose code is the same as the counterpart in-image code'.
			} asDictionary.
			{
				#label 			-> 		'remove empty class comments'.
				#object 			-> 		#model.
				#selector 			-> 		#removeEmptyClassComments.
				#balloonText 	-> 			'remove all empty class comments'.
			} asDictionary.
			{
				#label 			-> 		'remove selected items'.
				#object 			-> 		#model.
				#selector 			-> 		#removeSelections.
				#balloonText 	-> 			'remove the selected items from the change-list'.
			} asDictionary.
			{
				#label 			-> 		'remove unselected items'.
				#object 			-> 		#model.
				#selector 			-> 		#removeNonSelections.
				#balloonText 	-> 			'remove all the items not currently selected from the change-list'.
			} asDictionary.
		}`.
	^ aMenu! !


!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:14:16'!
classCommentVersionsMenu
	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: model.				"all commands are implemented by the model, not the view"
	aMenu addTitle: 'versions'.
	aMenu addStayUpIcons.
	aMenu addItemsFromDictionaries: `{
		{
			#label 			-> 		'compare to current'.
			#selector 			-> 		#compareToCurrentVersion.
			#balloonText 	-> 			'compare selected version to the current version'
		} asDictionary.
		{
			#label 			-> 		'revert to selected version'.
			#selector 			-> 		#fileInSelections.
			#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
		} asDictionary.
		nil.
		{
			#label 			-> 		'toggle diffing (D)'.
			#selector 			-> 		#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 		'update list'.
			#selector 			-> 		#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 		'help...'.
			#selector 			-> 		#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu! !

!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:15:32'!
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
					#selector 			-> 			#compareToCurrentVersion.
					#balloonText 	-> 			'compare selected version to the current version'
				} asDictionary.
				{
					#label 			-> 			'compare to version...'.
					#object 			-> 			#model.
					#selector 			-> 			#compareToOtherVersion.
					#balloonText 	-> 			'compare selected version to another selected version'
				} asDictionary.
			}` ].
		"Note: Revert to selected should be visible for lists of length one for having the ability to revert to an accidentally deleted method"
		aMenu addItemsFromDictionaries: `{
			{
				#label 			-> 			'revert to selected version (z)'.
				#object 			-> 			#model.
				#selector 			-> 			#fileInSelections.
				#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
			} asDictionary.
		}` ].

	aMenu addItemsFromDictionaries: `{
		{
			#label 			-> 			'edit current method (O)'.
			#selector 			-> 			#openSingleMessageBrowser.
			#balloonText 	-> 			'open a single-message browser on the current version of this method'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#object 			-> 			#model.
			#selector 			-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#object 			-> 			#model.
			#selector 			-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'senders (n)'.
			#selector 			-> 			#browseSenders.
			#balloonText 	-> 			'browse all senders of this selector'
		} asDictionary.
		{
			#label 			-> 			'implementors (m)'.
			#selector 			-> 			#browseImplementors.
			#balloonText 	-> 			'browse all implementors of this selector'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#object 			-> 			#model.
			#selector 			-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:15:56'!
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
				#selector 			-> 		#forgetClass.
				#icon 			-> 		#warningIcon
			} asDictionary.
			{
				#label 			-> 		'remove class from system (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeClass.
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
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'inst var refs...'.
				#selector 			-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'inst var defs...'.
				#selector 			-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'class var refs...'.
				#selector 			-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class vars'.
				#selector 			-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class refs (N)'.
				#selector 			-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
		}`.
	^ aMenu! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:59:19'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 		TheWorldMenu openLabel.
			#itemGroup 			-> 		20.
			#itemOrder 			-> 		20.
			#label 			-> 		'Change Sorter'.
			#object 			-> 		ChangeSorter.
			#selector 			-> 		#open.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 			-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`! !


!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:16:08'!
contextFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 			-> 		#exploreContextSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#contextClassHierarchy
			} asDictionary.
		}`.
	^ aMenu! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:16:21'!
contextStackMenu
	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fullStack (f)'.
				#selector 			-> 		#fullStack
			} asDictionary.
			{
				#label 			-> 		'restart (r)'.
				#selector 			-> 		#restart
			} asDictionary.
			{
				#label 			-> 		'proceed (p)'.
				#selector 			-> 		#proceed
			} asDictionary.
			{
				#label 			-> 		'step (t)'.
				#selector 			-> 		#doStep
			} asDictionary.
			{
				#label 			-> 		'step through (T)'.
				#selector 			-> 		#stepIntoBlock
			} asDictionary.
			{
				#label 			-> 		'send (e)'.
				#selector 			-> 		#send
			} asDictionary.
			{
				#label 			-> 		'where (w)'.
				#selector 			-> 		#where
			} asDictionary.
			{
				#label 			-> 		'peel to first like this'.
				#selector 			-> 		#peelToFirst
			} asDictionary.
			nil.
			{
				#label 			-> 		'return entered value'.
				#selector 			-> 		#returnValue
			} asDictionary.
			{
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleBreakOnEntry
			} asDictionary.
		}`.
	^aMenu! !

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:16:27'!
receiverFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 			-> 		#exploreReceiverSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#receiverClassHierarchy
			} asDictionary.
		}`.
	^ aMenu! !


!MethodSetWindow class methodsFor: 'browser menues' stamp: 'jmv 3/21/2024 11:17:31'!
messageSetMenu2Options
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'implementors of sent messages'.
				#selector 			-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'local senders of...'.
				#selector 			-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'local implementors of...'.
				#selector 			-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'spawn sub-protocol'.
				#selector 			-> 		#browseProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'spawn full protocol'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#speadsheetTemplateIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'sample instance'.
				#selector 			-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 			-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 			-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'change category...'.
				#object 			-> 		#model.
				#selector 			-> 		#changeCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		50.
				#label 			-> 		'change sets with this method'.
				#selector 			-> 		#findMethodInChangeSets.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		60.
				#label 			-> 		'revert to previous version'.
				#object 			-> 		#model.
				#selector 			-> 		#revertToPreviousVersion.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'more...'.
				#selector 			-> 		#openMessageListMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.			
		}`! !

!MethodSetWindow class methodsFor: 'browser menues' stamp: 'jmv 3/21/2024 11:17:40'!
messageSetMenuOptions

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
				#selector       			-> 			#browseCategoryOfCurrentMethod.
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


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:59:22'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 		TheWorldMenu openLabel.
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		40.
			#label 			-> 		'Message Names'.
			#object 			-> 		MessageNames.
			#selector 			-> 		#open.
			#icon 			-> 		#inspectIcon.
			#balloonText 			-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:59:26'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 		TheWorldMenu openLabel.
			#itemGroup 			-> 		40.
			#itemOrder 			-> 		10.
			#label 			-> 		'File List'.
			#object 			-> 		FileList.
			#selector 			-> 		#open.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 			-> 		'An explorer of the File System'.
		} asDictionary}`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:59:30'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 		TheWorldMenu openLabel.
			#itemGroup 			-> 		30.
			#itemOrder 			-> 		10.
			#label 			-> 		'Process Browser'.
			#object 			-> 		ProcessBrowser.
			#selector 			-> 		#open.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 			-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:59:33'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 		TheWorldMenu openLabel.
			#itemGroup 			-> 		40.
			#itemOrder 			-> 		20.
			#label 			-> 		'SUnit Test Runner'.
			#object 			-> 		TestRunner.
			#selector 			-> 		#open.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 			-> 		'Smalltalk''s testing framework'.
		} asDictionary}`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'jmv 3/21/2024 10:58:51'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 	TheWorldMenu openLabel.
			#itemGroup 			-> 	10.
			#itemOrder 			-> 	50.
			#label 			-> 	'Transcript'.
			#object 			-> 	Transcript.
			#selector 			-> 	#open.
			#icon 			-> 	#printerIcon.
			#balloonText 			-> 	'A window showing contents of the System Transcript'.
		} asDictionary}`! !


!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 3/21/2024 11:27:20'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 		-> 	[ angle := angle + 0.02. self redrawNeeded ].
			[ #down ] 		-> 	[ angle := (angle - 0.02) max: 0. self redrawNeeded ].
			[ #left ] 		-> 	[ depthLimit := depthLimit - 1 max: 1. self redrawNeeded].
			[ #right ] 		-> 	[ depthLimit := depthLimit + 1 min: 15. self redrawNeeded]
		}! !


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 3/21/2024 11:31:16'!
offAxisEdgeWeight: aSymbolOrNumber
	"A LayoutSpec may indicate a weighting perpendicular to the Layout Axis.   
	
	 This is the LayoutSpec's offAxisEdgeWeight which is between 0.0 and 1.0.

	As with LayoutSpec's a symbol may be used.

	If in a Row (preferred): { #rowTop (0.0), #center (0.5), #rowBottom (1.0)}

	If in a Column (preferred): { #columnLeft (0.0), #center (0.5), #columnRight (1.0) }

	Also accepted: { #leftOrTop (0.0), #center (0.5), #rightOrBottom (1.0) }
	"
	| edgeWeight |
	edgeWeight := (aSymbolOrNumber is: #Number)
		ifTrue: [ (aSymbolOrNumber between: 0.0 and: 1.0)
			ifTrue: [ aSymbolOrNumber ] 
			ifFalse: [self error: 'axisEdgeWeight ', aSymbolOrNumber printString, ' is out of range 0.0 to 1.0']
		]
		ifFalse: [ aSymbolOrNumber
			caseOf: {
				[ #leftOrTop ] 				-> [ 0.0 ].
				[ #rowTop ] 				-> [ 0.0 ].
				[ #columnLeft ] 				-> [ 0.0 ].
				[ #center ] 				-> [ 0.5 ].
				[ #rightOrBottom ] 				-> [ 1.0 ].
				[ #rowBottom ] 				-> [ 1.0 ].
				[ #columnRight ] 				-> [ 1.0 ]
			}
			otherwise: [ self error: 'bad offAxisEdgeWeight specifier: ', aSymbolOrNumber  printString ]
		].
	offAxisEdgeWeight := edgeWeight.
	self updateContainingLayout.! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/21/2024 10:59:47'!
additionalMenuOptions

	^`{
		{
			#submenuOf  				-> 		TheWorldMenu openLabel.
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		30.
			#label 				-> 		'Package Installer'.
			#selector 				-> 		#openPackageInstaller.
			#icon 				-> 		#packageIcon.
			#balloonText 				-> 		'A tool for installing Cuis packages from the local file system.'.
		} asDictionary.
		{
			#submenuOf  				-> 		TheWorldMenu openLabel.
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		40.
			#label 				-> 		'Package Downloader'.
			#selector 				-> 		#openPackageDownloader.
			#icon 				-> 		#packageIcon.
			#balloonText 				-> 		'A tool for downloading Cuis packages from the internet.'.
		} asDictionary.
	}`	
	! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:20:09'!
changesMenuOptions

	^`{
			{
				#submenuOf  			-> 		TheWorldMenu changesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Change Sorter'.
				#object 			-> 		ChangeSorter.
				#selector 			-> 		#open.
				#icon 			-> 		#halfRefreshIcon.
				#balloonText 	-> 		'Open a 3-paned changed-set viewing tool'.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu changesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Install New Updates'.
				#object 			-> 		ChangeSet.
				#selector 			-> 		#installNewUpdates.
				#icon 			-> 		#updateIcon.
				#balloonText 	-> 		'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu changesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Browse my Changes'.
				#object 			-> 		Smalltalk.
				#selector 			-> 		#browseMyChanges.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Browse all of my changes since the last time #condenseSources was run.'.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu changesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Recently logged Changes...'.
				#object 			-> 		ChangeList.
				#selector 			-> 		#browseRecentLog.
				#icon 			-> 		#clockIcon.
				#balloonText 	-> 		'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu changesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Save World as morph file'.
				#selector 			-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:01:02'!
debugMenuOptions

	^`{
			{
				#submenuOf  			-> 		TheWorldMenu debugLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Inspect World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#inspect.
				#icon 			-> 		#inspectIcon.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu debugLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Explore World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#explore.
				#icon 			-> 		#exploreIcon.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu debugLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'MessageTally all Processes'.
				#selector 			-> 		#startMessageTally.
				#icon 			-> 		#systemMonitorIcon.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu debugLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Start drawing all again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#removeAllKnownFailing.
				#icon 			-> 		#graphicsIcon.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu debugLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Start stepping again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#resumeAfterStepError.
				#icon 			-> 		#mediaPlaybackStartIcon.
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu debugLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'Close all Debuggers'.
				#object 			-> 		DebuggerWindow.
				#selector 			-> 		#closeAllDebuggers.
				#icon 			-> 		#closeIcon.
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:03:15'!
helpMenuOptions

	^ `{
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 			-> 		#aboutThisSystem.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'current version information.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Terse Guide to Cuis'.
				#selector 			-> 		#openTerseGuide.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Class Comment Browser'.
				#selector 			-> 		#openCommentGuide.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'search & explore Cuis Class Comments'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCodeManagementInCuis.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		50.
				#label 			-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCuisAndGitHub.
				#icon 			-> 		#helpIcon.
				#balloonText 			-> 		'GitHub usage pattern.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 			-> 		#openHelp.
				#icon 			-> 		#keyboardShortcutsIcon.
				#balloonText 			-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openUsefulExpressions.
				#icon 			-> 		#chatIcon.
				#balloonText 			-> 		'a window full of useful expressions.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'VM Statistics'.
				#selector 			-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 			-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#submenuOf 			-> 		TheWorldMenu helpLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Space Left'.
				#selector 			-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 			-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:20:31'!
preferencesMenuOptions

	^`{
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Focus follows Mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Focus when Click'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.

			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Show ST-80 assignments'.
				#selector 			-> 		#enableShowAssignmentAsLeftArrow.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Show ANSI assignments'.
				#selector 			-> 		#disableShowAssignmentAsLeftArrow.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		40.
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		50.
				#label			-> 		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		60.
				#label			-> 		'Load all TrueType Fonts'.
				#object 			-> 		TrueTypeFontFamily.
				#selector 			-> 		#readAdditionalFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		70.
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		80.
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'Save Prefs in UserPrefs.txt'.
				#selector 			-> 		#enableUsePrefsFile.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Preferences are saved without need to save the Smalltalk Image.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'Save Prefs in the Image'.
				#selector 			-> 		#disableUsePrefsFile.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Preferences are lost unless the Smalltalk Image is saved.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		40.
				#label 			-> 		'All preferences...'.
				#object 			-> 		PreferenceSet.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}`.! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/21/2024 11:06:05'!
windowsMenuOptions

	^`{
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findWindow:.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyBrowsers:.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyWindows:.
				#icon 			-> 		#newWindowIcon.
				#balloonText 	-> 		'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  ->  					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findATranscript:.
				#icon 			-> 		#printerIcon.
				#balloonText 	-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAFileList:.
				#icon 			-> 		#systemFileManagerIcon.
				#balloonText 	-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAChangeSorter:.
				#icon 			-> 		#changesIcon.
				#balloonText 	-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAMessageNamesWindow:.
				#icon 			-> 		#inspectIcon.
				#balloonText 	-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Tile open windows'.
				#object          			-> 		TileResizeMorph.
				#selector 			-> 		#tileOpenWindows.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Tile open windows'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#collapseAll.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#restoreAll.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		40.
				#label 			-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#closeTopWindow.
				#icon 			-> 		#closeIcon.
				#balloonText 	-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		50.
				#label 			-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#sendTopWindowToBack.
				#icon 			-> 		#goBottomIcon.
				#balloonText 	-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		60.
				#label 			-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#bringWindowsFullOnscreen.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#closeUnchangedWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#deleteNonWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'Delete Both of the Above'.
				#selector 			-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'
			} asDictionary.
		}`! !


!Theme methodsFor: 'shout' stamp: 'jmv 3/21/2024 11:21:30'!
shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	^ {
		#selector  					-> nil.
		#arguments 					-> #(cyan muchDarker).
		#comment 					-> #(green muchDarker).
		#tempBar 					-> #gray.
		#tempVars 					-> #(gray muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#pseudoVariables 					-> #(red muchDarker).
		#literals 					-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#incompleteMessages 					-> #(gray veryMuchDarker).
		#blockLevelZero 					-> #black.
		#blockLevelOne 					-> #brown.
		#blockLevelTwo 					-> #magenta.
		#blockLevelThree 					-> #red.
		#blockLevelFour 					-> #(orange darker).
		#blockLevelFive 					-> #(orange muchDarker).
		#blockLevelSix 					-> #(green muchDarker).
		#blockLevelSeven 					-> #blue.
		#defaults 					-> #black.
		#undefined 					-> #red.
		#methodTags 					-> #(green muchDarker).
	}! !


!DarkTheme methodsFor: 'colors' stamp: 'jmv 3/21/2024 11:21:59'!
shout
	"Color symbols as an association list."

	^ {
		#selector  					-> '#B59F60'.
		#arguments 					-> '#289078'.
		#comment 					-> #(green duller).
		#tempBar 					-> #gray.
		#tempVars 					-> '#767836'.
		#instVar 					-> '#B3695A'.
		#pseudoVariables 					-> '#2070E0'.
		#literals 					-> #green.
		#messages 					-> '#6FB3BD'.
		#incompleteMessages  					-> '#F08060'.
		#blockLevelZero 					-> '#6FB3BD'.
		#blockLevelOne 					-> '#FFB0B0'.
		#blockLevelTwo 					-> '#B0FFB0'.
		#blockLevelThree 					-> '#B0B0FF'.
		#blockLevelFour 					-> '#00B0B0'.
		#blockLevelFive 					-> '#E03030'.
		#blockLevelSix 					-> '#30E030'.
		#blockLevelSeven 					-> '#3030E0'.
		#defaults 					-> '#A1AFBF'.
		#undefined 					-> '#E04020'.
		#methodTags 					-> #green.
	}! !

