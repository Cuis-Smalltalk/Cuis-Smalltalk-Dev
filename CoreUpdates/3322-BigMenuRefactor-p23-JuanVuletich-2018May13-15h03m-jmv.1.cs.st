'From Cuis 5.0 of 7 November 2016 [latest update: #3319] on 13 May 2018 at 3:33:06 pm'!

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 15:28:48'!
systemCatSingletonMenu

	| aMenu |
	self flag: #renameSystemCategory.	"temporarily disabled"
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class category'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'find class... (f)'.
				#selector 		-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse all'.
				#selector 		-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse'.
				#selector 		-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'update'.
				#object 			-> 		#model.
				#selector 		-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSystemCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
		}`.
	^aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 15:32:04'!
systemCategoryMenu

	| aMenu |
	self flag: #renameSystemCategory.	"temporarily disabled"
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class category'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'find class... (f)'.
				#selector 		-> 		#findClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'recent classes... (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#recent.
				#icon 			-> 		#clockIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse all'.
				#selector 		-> 		#browseAllClasses.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse'.
				#selector 		-> 		#openSystemCategoryBrowser.
				#icon 			-> 		#packageIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSystemCategory.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'reorganize'.
				#object 			-> 		#model.
				#selector 		-> 		#editSystemCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#label 			-> 		'alphabetize'.
				#object 			-> 		#model.
				#selector 		-> 		#alphabetizeSystemCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'update'.
				#object 			-> 		#model.
				#selector 		-> 		#updateSystemCategories.
				#icon 			-> 		#updateIcon
			} asDictionary.
			{
				#label 			-> 		'add item...'.
				#object 			-> 		#model.
				#selector 		-> 		#addSystemCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameSystemCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#label 			-> 		'remove'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSystemCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'move to top'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryTop.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'move up'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryUp.
				#icon 			-> 		#goUpIcon
			} asDictionary.
			{
				#label 			-> 		'move down'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryDown.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#label 			-> 		'move to bottom'.
				#object 			-> 		#model.
				#selector 		-> 		#moveSystemCategoryBottom.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runSystemCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	^aMenu! !

