'From Cuis6.3 [latest update: #6350] on 8 July 2024 at 9:49:40 pm'!

!Browser methodsFor: 'system category functions' stamp: 'IB 7/8/2024 21:46:08'!
fileInSystemCategory

	[| fileName |

	fileName := StringRequestMorph request: 'Enter file name' orCancel: [ ^self ].
	ChangeSet fileIn: fileName asFileEntry ]
		on: FileDoesNotExistException
		do: [:anError |
			(PopUpMenu confirm: 'File ', anError messageText, ' does not exist. Try again?') ifTrue: [ anError retry ] ifFalse: [ anError return ]]! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'IB 7/8/2024 21:45:27'!
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
				#label			->		'fileIn'.
				#object			->		#model.
				#selector		    -> 			#fileInSystemCategory.
				#icon			->		#scriptIcon.
				#itemOrder		->			10.
				#itemGroup		->			30
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

