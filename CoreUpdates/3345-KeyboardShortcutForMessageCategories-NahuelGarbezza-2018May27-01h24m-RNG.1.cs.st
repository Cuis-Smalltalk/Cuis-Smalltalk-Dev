'From Cuis 5.0 of 7 November 2016 [latest update: #3333] on 27 May 2018 at 1:46:20 am'!

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'RNG 5/27/2018 01:44:26'!
messageCatListKey: aChar from: view

	aChar == $o ifTrue: [^ model fileOutMessageCategories ].
	aChar == $t ifTrue: [^ model runMessageCategoryTests ].
	aChar == $x ifTrue: [^ model removeMessageCategory ].! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'RNG 5/27/2018 01:43:23'!
buildMorphicMessageCatList

	^PluggableListMorph
		model: model
		listGetter: #messageCategoryList
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:
		mainView: self
		menuGetter: #messageCategoryMenu
		keystrokeAction: #messageCatListKey:from:! !

!BrowserWindow methodsFor: 'menu building' stamp: 'RNG 5/27/2018 01:43:51'!
messageCategoryMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message Category'.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileOut (o)'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessageCategories.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'reorganize'.
				#object 			-> 		#model.
				#selector 		-> 		#editMessageCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#label 			-> 		'alphabetize'.
				#object 			-> 		#model.
				#selector 		-> 		#alphabetizeMessageCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'remove empty categories'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyCategories.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#label 			-> 		'categorize all uncategorized'.
				#object 			-> 		#model.
				#selector 		-> 		#categorizeAllUncategorizedMethods.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#label 			-> 		'new category...'.
				#object 			-> 		#model.
				#selector 		-> 		#addCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'rename...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#label 			-> 		'remove (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessageCategory.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'Run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runMessageCategoryTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
		}`.
	^ aMenu! !

