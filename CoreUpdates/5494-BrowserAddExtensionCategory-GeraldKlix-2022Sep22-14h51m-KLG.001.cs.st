'From Haver 6.0 [latest update: #5492] on 22 September 2022 at 5:29:48 pm'!

!Browser methodsFor: 'message category functions' stamp: 'KLG 9/22/2022 17:19:50'!
addExtensionCategory
	"Present a choice package names and let the user enter a new category suffx.
	Take the suffix form the selected category."

	| packageNames packageNameIndex |
	
	selectedClassName ifNil: [ ^self ].

	packageNames _ CodePackage installedPackages collect: [ :package |
		package packageName ] :: sorted.
	packageNames ifEmpty: [ ^ self ].
	
	packageNameIndex _ PopUpMenu 
		withCaption: 'Which package to extend?'
		chooseFrom: packageNames.
	packageNameIndex = 0 ifTrue: [ ^ self ].

	self 
		request: 'Enter the category name'
		initialAnswer: 
			(String streamContents: [ :stream |
				stream
					nextPut: $*;
					nextPutAll: (packageNames at: packageNameIndex).
				selectedMessageCategory ifNotNil: [
					stream
						nextPut: $-;
						nextPutAll: selectedMessageCategory ] ])
		 verifying: [ :newCategoryName | 
			((newCategoryName at: 1) = $* or: [
				self confirm:
					"I know about with #withNewlines ..."
					`'The selected category does not start with an asterix (*).',
					String newLineString,
					'Do you want to use it anyway?'` ]) and: [
						packageNames includes:				(newCategoryName copyUpTo: $-) allButFirst :: or: [
							self confirm:
								`'The selected prefix does not match any package name.', 
								String newLineString,
								'Do you want to use it anyway?'` ] ] ]
		do: [ :newCategoryName | | oldMessageCategoryListIndex |
			oldMessageCategoryListIndex _ self messageCategoryListIndex.
			self classOrMetaClassOrganizer
				addCategory: newCategoryName
				before: selectedMessageCategory.
			self changed: #messageCategoryList.
			self messageCategoryListIndex:
				(oldMessageCategoryListIndex = 0
					ifTrue: [ self classOrMetaClassOrganizer categories size + 1 ]
					ifFalse: [ oldMessageCategoryListIndex ]).
			self changed: #messageCategoryList ]! !


!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'KLG 9/22/2022 16:48:58'!
messageCatListKey: aChar from: view

	aChar = $o ifTrue: [ ^ model fileOutMessageCategories ].
	aChar = $t ifTrue: [ ^ model runMessageCategoryTests ].
	aChar = $x ifTrue: [ ^ model removeMessageCategory ].
	aChar = $R ifTrue: [ ^ model renameCategory ].
	aChar = $n ifTrue: [ ^ model addCategory ].
	aChar = $N ifTrue: [ ^ model addExtensionCategory ].
	aChar = $e ifTrue: [ ^ model removeEmptyCategories ].
	aChar = $c ifTrue: [ ^ model categorizeAllUncategorizedMethods ].
	aChar = $a ifTrue: [ ^ model alphabetizeMessageCategories ].
	aChar = $r ifTrue: [ ^ model editMessageCategories ]! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'KLG 9/22/2022 15:55:53'!
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
				#label 			-> 		'reorganize (r)'.
				#object 			-> 		#model.
				#selector 		-> 		#editMessageCategories.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'alphabetize (a)'.
				#object 			-> 		#model.
				#selector 		-> 		#alphabetizeMessageCategories.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove empty categories (e)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyCategories.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'categorize all uncategorized (c)'.
				#object 			-> 		#model.
				#selector 		-> 		#categorizeAllUncategorizedMethods.
				#icon 			-> 		#packageIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'new category... (n)'.
				#object 			-> 		#model.
				#selector 		-> 		#addCategory.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'new extension category... (N)'.
				#object 			-> 		#model.
				#selector 		-> 		#addExtensionCategory.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
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


!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors editClass editComment explainSpecial: fileOutClass makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: classNamesListForSelecting createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on: flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList hierarchicalClassNamesList listClassesHierarchically listClassesHierarchically: originalSelectedClassName recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory addExtensionCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategory:class: categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveAllToOtherSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: newSystemCategoryNameInitialAnswer potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectedSystemCategory setSelectedSystemCategory: setSelectedSystemCategoryTreeItem: systemCategoryList systemCategoryRoots systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' removeClass renameGlobal safelyRemoveClass)
('testing' is: isEditingClass isEditingExistingClass isEditingMethod isEditingNewClass)
('self-updating' updateIfNeeded)
!

