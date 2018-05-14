'From Cuis 5.0 of 7 November 2016 [latest update: #3319] on 13 May 2018 at 3:03:58 pm'!

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 18:53:59'!
classListMenu
	"Set up the menu to apply to the receiver's class list, honoring the #shifted boolean"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	aMenu
		addItemsFromDictionaries: `{
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
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'show hierarchy'.
				#object 			-> 		#model.
				#selector 		-> 		#hierarchy.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'show definition'.
				#object 			-> 		#model.
				#selector 		-> 		#editClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'show comment'.
				#object 			-> 		#model.
				#selector 		-> 		#editComment.
				#icon 			-> 		#findIcon
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
			nil.
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
			nil.
			{
				#label 			-> 		'rename class ...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#label 			-> 		'copy class...'.
				#object 			-> 		#model.
				#selector 		-> 		#copyClass.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#label 			-> 		'remove class (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runClassTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#label 			-> 		'more...'.
				#selector 		-> 		#offerClassListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 18:42:05'!
classListMenu2
	"Set up the menu to apply to the receiver's class list when the shift key is down"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'unsent methods'.
				#selector 		-> 		#browseUnusedMethods.
				#icon 			->		#junkIcon.
				#balloonText 	-> 		'browse all methods defined by this class that have no senders'
			} asDictionary.
			{
				#label 			-> 		'unreferenced inst vars'.
				#selector 		-> 		#showUnreferencedInstVars.
				#icon 			->		#junkIcon.
				#balloonText 	-> 		'show a list of all instance variables that are not referenced in methods'
			} asDictionary.
			{
				#label 			-> 		'unreferenced class vars'.
				#selector 		-> 		#showUnreferencedClassVars.
				#icon 			->		#junkIcon.
				#balloonText 	-> 		'show a list of all class variables that are not referenced in methods'
			} asDictionary.
			{
				#label 			-> 		'subclass template'.
				#object 			-> 		#model.
				#selector 		-> 		#makeNewSubclass.
				#icon 			->		#classIcon.
				#balloonText 	-> 		'put a template into the code pane for defining of a subclass of this class'
			} asDictionary.
			nil.
			{
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			->		#instanceIcon.
				#balloonText 	-> 		'give me a sample instance of this class, if possible'
			} asDictionary.
			{
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			->		#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class'
			} asDictionary.
			{
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			->		#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class and of all of its subclasses'
			} asDictionary.
			nil.
			{
				#label 			-> 		'create inst var accessors'.
				#object 			-> 		#model.
				#selector 		-> 		#createInstVarAccessors.
				#icon 			->		#sendReceiveIcon.
				#balloonText 	-> 		'compile instance-variable access methods for any instance variables that do not yet have them'
			} asDictionary.
			nil.
			{
				#label 			-> 		'more...'.
				#selector 		-> 		#offerClassListMenu.
				#icon 			->		#listAddIcon.
				#balloonText 	-> 		'return to the standard class-list menu'
			} asDictionary.
		}`.
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 18:59:39'!
messageCategoryMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.			"All the options are for the model."
	aMenu addTitle: 'Message Category'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileOut'.
				#selector 		-> 		#fileOutMessageCategories.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'reorganize'.
				#selector 		-> 		#editMessageCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#label 			-> 		'alphabetize'.
				#selector 		-> 		#alphabetizeMessageCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'remove empty categories'.
				#selector 		-> 		#removeEmptyCategories.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#label 			-> 		'categorize all uncategorized'.
				#selector 		-> 		#categorizeAllUncategorizedMethods.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#label 			-> 		'new category...'.
				#selector 		-> 		#addCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'rename...'.
				#selector 		-> 		#renameCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#label 			-> 		'remove'.
				#selector 		-> 		#removeMessageCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Run tests'.
				#selector 		-> 		#runMessageCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 20:10:56'!
messageListMenu
	"Answer the message-list menu"
	"Changed by emm to include menu-item for breakpoints"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message List'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'what to show...'.
				#selector 		-> 		#offerWhatToShowMenu.
				#icon 			-> 		#preferencesIcon
			} asDictionary.
			{
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry.
				#icon 			-> 		#debugIcon
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
				#label 			-> 		'browse method (O)'.
				#selector 		-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#label 			-> 		'explore CompiledMethod'.
				#object 			-> 		#model.
				#selector 		-> 		#exploreCompiledMethod.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'senders of... (n)'.
				#selector 		-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'implementors of... (m)'.
				#selector 		-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#label 			-> 		'inheritance (i)'.
				#selector 		-> 		#methodHierarchy.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#label 			-> 		'versions (v)'.
				#selector 		-> 		#browseVersions.
				#icon 			-> 		#clockIcon
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
				#label 			-> 		'class variables'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#label 			-> 		'Run test (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#label 			-> 		'Debug test (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#debugMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'more...'.
				#selector 		-> 		#openMessageListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 20:25:02'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message List'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#label 			-> 		'implementors of sent messages'.
				#selector 		-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'local senders of...'.
				#selector 		-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'local implementors of...'.
				#selector 		-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'spawn sub-protocol'.
				#selector 		-> 		#browseProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#label 			-> 		'spawn full protocol'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#speadsheetTemplateIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
		}`.

	self addExtraMenu2ItemsTo: aMenu.
	aMenu add: 'change category...' target: model action: #changeCategory icon: #saveAsIcon.

	model canShowMultipleMessageCategories ifTrue: [	
		aMenu add: 'show category (C)' target: model action: #showHomeCategory icon: #packageIcon ].
	aMenu
		add: 'change sets with this method' action: #findMethodInChangeSets icon: #changesIcon;
		add: 'revert to previous version' target: model action: #revertToPreviousVersion icon: #undoIcon;
		addLine;
		add: 'more...' action: #openMessageListMenu icon: #listAddIcon.
	^ aMenu! !


!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 18:53:57'!
classListMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'definition'.
				#object 			-> 		#model.
				#selector 		-> 		#editClass.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'comment'.
				#object 			-> 		#model.
				#selector 		-> 		#editComment.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileIn'.
				#object 			-> 		#model.
				#selector 		-> 		#fileInClass.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove existing'.
				#object 			-> 		#model.
				#selector 		-> 		#removeUnmodifiedCategories.
				#icon 			-> 		#deleteIcon
			} asDictionary.
		}`.
	^aMenu! !

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 19:03:29'!
messageCategoryMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.			"All the options are for the model."
	aMenu addTitle: 'Message Category'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileIn'.
				#selector 		-> 		#fileInMessageCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#label 			-> 		'fileOut'.
				#selector 		-> 		#fileOutMessageCategories.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'reorganize'.
				#selector 		-> 		#editMessageCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'add item...'.
				#selector 		-> 		#addCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'rename...'.
				#selector 		-> 		#renameCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#label 			-> 		'remove'.
				#selector 		-> 		#removeMessageCategory.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove existing'.
				#selector 		-> 		#removeUnmodifiedMethods.
				#icon 			-> 		#deleteIcon
			} asDictionary.
		}`.
	^ aMenu! !

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 20:10:06'!
messageListMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message List'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileIn'.
				#object 			-> 		#model.
				#selector 		-> 		#fileInMessage.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'senders (n)'.
				#selector 		-> 		#browseSenders.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'implementors (m)'.
				#selector 		-> 		#browseImplementors.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#label 			-> 		'method inheritance (h)'.
				#selector 		-> 		#methodHierarchy.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#label 			-> 		'versions (v)'.
				#selector 		-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
		}`.
	^ aMenu! !


!MessageSetWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 20:22:14'!
addExtraMenu2ItemsTo: aMenu
	"The shifted selector-list menu is being built.  Add items specific to MessageSet"

	model growable ifTrue: [
		aMenu
			addItemsFromDictionaries: `{
				{
					#label 			-> 		'remove from this browser'.
					#object 			-> 		#model.
					#selector 		-> 		#removeMessageFromBrowser.
					#icon 			-> 		#listRemoveIcon
				} asDictionary.
				{
					#label 			-> 		'filter message list...'.
					#selector 		-> 		#filterMessageList.
					#icon 			-> 		#findIcon
				} asDictionary.
			}`.
		].
	aMenu add: 'sort by date' target: model action: #sortByDate icon: #dateIcon! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/7/2018 18:53:45'!
classListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
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

