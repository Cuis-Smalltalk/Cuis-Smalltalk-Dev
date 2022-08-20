'From Cuis 6.0 [latest update: #5462] on 20 August 2022 at 7:36:21 pm'!

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 8/20/2022 19:33:29'!
renameSystemCategory: oldName to: newName.

	"If it's a top category, rename subcategories first:"
	(self withSubCategoriesOf: oldName) reverseDo: [ :each |
		self renameCategory: each to: newName, (each allButFirst: oldName size)]
	! !

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 8/20/2022 19:16:16'!
subCategoriesOf: aRootCategoryName

	^self categoriesMatching: (self subCategoriesPatternOf: aRootCategoryName)
	! !

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 8/20/2022 19:16:16'!
subCategoriesPatternOf: aRootCategoryName

	^ aRootCategoryName,'-*'! !

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 8/20/2022 19:34:51'!
withSubCategoriesOf: aRootCategoryName

	"Returns the root category and subcategories if they exist
	The order is important because it is assumed like that in some senders
	If aRootCategoryName does not exist as category, it will return it anyway - Hernan"
	^(OrderedCollection with: aRootCategoryName) 
		addAll: (self subCategoriesOf: aRootCategoryName);
		yourself.
	
	! !


!Browser methodsFor: 'system category functions' stamp: 'HAW 8/20/2022 19:14:35'!
removeSystemCategory
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	selectedSystemCategory ifNil: [^ self].
	(self classList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this system category 
and all its classes?'])
		ifTrue: [
			systemOrganizer removeSystemCategory: selectedSystemCategory.
			self setSelectedSystemCategory: nil.
			self changed: #systemCategoryRoots ]! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 8/20/2022 19:30:35'!
renameSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection
	
	21-Mar-2012 jmv Note: This is not recorded appropriately in change sets.
	The easiest solution is to trigger #classRecategorized for all classes in the category.
	But this is not a real solution, as the resulting changeset would not do a rename,
	but create a new category (that would go to the bottom) with all the classes.
	
	In the meantime, disable the menu entry. This is not so important after all.
	"
	| oldName newName |
	selectedSystemCategory ifNil: [^ self].  "no selection"
	oldName _ selectedSystemCategory.
	newName _ self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName _ aString asSymbol.
			systemOrganizer renameSystemCategory: oldName to: newName.
			self setSelectedSystemCategory: newName.
			self changed: #systemCategoryRoots]! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'HAW 8/20/2022 19:08:08'!
fileOutCategory: aCategoryRoot
	"FileOut all the classes in the named system category."

	DirectoryEntry smalltalkImageDirectory // (aCategoryRoot asFileName , '.st') writeStreamDo: [ :fileStream |
		(self withSubCategoriesOf: aCategoryRoot) do: [ :category | 
			self fileOutCategory: category on: fileStream initializing: true ]]! !

!SystemOrganizer methodsFor: 'remove' stamp: 'HAW 8/20/2022 19:15:35'!
removeSystemCategory: category
	"remove all the classes associated with the category and its subcategories.
	If category does not exist, it does nothing"

	(self withSubCategoriesOf: category) reverseDo: [ :categoryToRemove |
		(self superclassOrderIn: categoryToRemove) reverseDo: [ :class | class removeFromSystem].
		self removeCategory: categoryToRemove ]! !

!methodRemoval: SystemOrganizer #withAllSubCategoriesOf: stamp: 'HAW 8/20/2022 19:08:08'!
SystemOrganizer removeSelector: #withAllSubCategoriesOf:!
!methodRemoval: SystemOrganizer #removeCategoriesMatching: stamp: 'HAW 8/20/2022 19:14:51'!
SystemOrganizer removeSelector: #removeCategoriesMatching:!

!SystemOrganizer reorganize!
('fileIn/Out' fileOutAllCategories fileOutCategory: fileOutCategory:on:initializing: fileOutCategoryNoPoolsNoInit:on: objectForDataStream: superclassOrderIn:)
('remove' categoriesMatching: removeMissingClasses removeSystemCategory:)
('categories' allClassesAt: classesAt: fullListAtCategoryNamed: hasCategory: removeSystemCategories: renameSystemCategory:to: subCategoriesOf: subCategoriesPatternOf: superclassOrderInAll: testCaseClassesAt: withSubCategoriesOf:)
('source code management' classMethodCountOf: instanceMethodCountOf: linesOfCodeOf:)
!

