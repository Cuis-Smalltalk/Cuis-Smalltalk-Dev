'From Cuis 6.0 [latest update: #5443] on 13 August 2022 at 4:46:06 pm'!

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 8/13/2022 16:39:04'!
moreMenuLabel

	^'more'! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 8/13/2022 16:42:45'!
classListMoreMenuOptions

	^ `{			
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'unsent methods'.
				#selector 		-> 		#browseUnusedMethods.
				#icon 			->			#junkIcon.
				#balloonText 	-> 		'browse all methods defined by this class that have no senders'
			} asDictionary.
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'unreferenced inst vars'.
				#selector 		-> 		#showUnreferencedInstVars.
				#icon 			->			#junkIcon.
				#balloonText 	-> 		'show a list of all instance variables that are not referenced in methods'
			} asDictionary.
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'unreferenced class vars'.
				#selector 		-> 		#showUnreferencedClassVars.
				#icon 			->			#junkIcon.
				#balloonText 	-> 		'show a list of all class variables that are not referenced in methods'
			} asDictionary.
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'subclass template'.
				#object 			-> 		#model.
				#selector 		-> 		#makeNewSubclass.
				#icon 			->			#classIcon.
				#balloonText 	-> 		'put a template into the code pane for defining of a subclass of this class'
			} asDictionary.
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'sample instance'.
				#selector 		-> 		#makeSampleInstance.
				#icon 			->			#instanceIcon.
				#balloonText 	-> 		'give me a sample instance of this class, if possible'
			} asDictionary.
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 		-> 		#inspectInstances.
				#icon 			->			#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class'
			} asDictionary.
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 		-> 		#inspectSubInstances.
				#icon 			->			#inspectIcon.
				#balloonText 	-> 		'open an inspector on all the extant instances of this class and of all of its subclasses'
			} asDictionary.
			{
				#submenuOf -> BrowserWindow moreMenuLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'create inst var accessors'.
				#object 			-> 		#model.
				#selector 		-> 		#createInstVarAccessors.
				#icon 			->			#sendReceiveIcon.
				#balloonText 	-> 		'compile instance-variable access methods for any instance variables that do not yet have them'
			} asDictionary.
		}`.
	! !

!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 8/13/2022 16:43:41'!
classListMenuOptions

	^ `{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		5.
				#label 			-> 		'show classes hierarchically'.
				#object 			-> 		#model.
				#selector 			-> 		#enableListClassesHierarchically.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		6.
				#label 			-> 		'show classes alphabetically'.
				#object 			-> 		#model.
				#selector 			-> 		#enableListClassesAlphabetically.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'show definition'.
				#object 			-> 		#model.
				#selector 			-> 		#editClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'show comment'.
				#object 			-> 		#model.
				#selector 			-> 		#editComment.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 			-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 			-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'class var refs...'.
				#selector 			-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'class vars'.
				#selector 			-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'class refs (N)'.
				#selector 			-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'rename class... (R)'.
				#object 			-> 		#model.
				#selector 			-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'copy class...'.
				#object 			-> 		#model.
				#selector 			-> 		#copyClass.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove class (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runClassTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		20.
				#label 			-> 		BrowserWindow moreMenuLabel.
				#selector 			-> 		#yourself.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}, BrowserWindow classListMoreMenuOptions`.
	! !

!methodRemoval: BrowserWindow class #classListMenu2Options stamp: 'HAW 8/13/2022 16:41:58'!
BrowserWindow class removeSelector: #classListMenu2Options!
!methodRemoval: BrowserWindow #classListMenu2 stamp: 'HAW 8/13/2022 16:41:53'!
BrowserWindow removeSelector: #classListMenu2!
!methodRemoval: CodeWindow #offerClassListMenu stamp: 'HAW 8/13/2022 16:41:11'!
CodeWindow removeSelector: #offerClassListMenu!
!methodRemoval: CodeWindow #offerClassListMenu2 stamp: 'HAW 8/13/2022 16:41:48'!
CodeWindow removeSelector: #offerClassListMenu2!
