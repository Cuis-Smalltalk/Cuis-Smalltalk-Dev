'From Cuis 6.0 [latest update: #5056] on 1 February 2022 at 7:53:06 pm'!

!Categorizer methodsFor: 'accessing' stamp: 'len 2/1/2022 19:45:05'!
renameCategory: oldCatString to: newCatString
	"Rename a category. No action if new name already exists, or if old name does not exist."
	
	| index newCategory |
	
	newCategory _ newCatString withBlanksTrimmed.
	"new name exists, so no action"
	(categoryArray indexOf: newCategory) > 0 ifTrue: [^ self].
	
	"old name not found, so no action"
	(index _ categoryArray indexOf: oldCatString) = 0 ifTrue: [^ self].	
	
	"need to change identity so smart list update will notice the change"
	categoryArray _ categoryArray copy.  
	categoryArray at: index put: newCategory! !


!ClassOrganizer methodsFor: 'accessing' stamp: 'len 2/1/2022 19:46:01'!
renameCategory: oldCatString to: newCatString

	| newCategory oldElementsBefore oldElementsAfter |
	
	newCategory _ newCatString withBlanksTrimmed.
	oldElementsBefore _ self listAtCategoryNamed: oldCatString.
	SystemChangeNotifier uniqueInstance doSilently: [
		super renameCategory: oldCatString to: newCatString].
	
	oldElementsAfter _ (self listAtCategoryNamed: oldCatString) asSet.
	oldElementsBefore do: [:each | (oldElementsAfter includes: each)
		ifFalse: [self notifyOfChangedSelector: each from: oldCatString to: newCategory]].
	
	self notifyOfChangedCategoryFrom: oldCatString to: newCategory.! !


!Browser methodsFor: 'message category functions' stamp: 'len 2/1/2022 19:45:44'!
renameCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	| oldIndex oldName newName |
	selectedClassName ifNil: [^ self].
	selectedMessageCategory ifNil: [ ^self ].
	oldIndex _ self messageCategoryListIndex.
	oldName _ self selectedMessageCategoryName.
	newName _ self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName _ aString asSymbol.
			self classOrMetaClassOrganizer
				renameCategory: oldName
				to: newName.
			self classListIndex: self classListIndex.
			self messageCategoryListIndex: oldIndex.
			self changed: #messageCategoryList]! !

!Browser methodsFor: 'system category functions' stamp: 'len 2/1/2022 19:35:19'!
removeSystemCategory
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	selectedSystemCategory ifNil: [^ self].
	(self classList size = 0
		or: [self confirm: 'Are you sure you want to
remove this system category 
and all its classes?'])
		ifTrue: [
			"Remove subcategories:"
			systemOrganizer removeCategoriesMatching: selectedSystemCategory, '-*'.
			"Then remove the top category (it doesn't do anything if it doesn't exist):"
			systemOrganizer removeSystemCategory: selectedSystemCategory.
			self setSelectedSystemCategory: nil.
			self changed: #systemCategoryRoots ]! !

!Browser methodsFor: 'system category functions' stamp: 'len 2/1/2022 19:45:55'!
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
			"If it's a top category, rename subcategories first:"
			(oldName includes: $-) ifFalse:
				[(systemOrganizer categoriesMatching: oldName, '-*')
					do: [:each| systemOrganizer renameCategory: each to: newName, (each allButFirst: oldName size)]].
			systemOrganizer renameCategory: oldName to: newName.
			self setSelectedSystemCategory: newName.
			self changed: #systemCategoryRoots]! !


!SystemCategoryWrapper methodsFor: 'accessing' stamp: 'len 2/1/2022 19:50:41'!
contents
	| prefix subcategories |
	(item includes: $-) ifTrue: [^ #()].
	prefix _ item, '-'.
	subcategories _ OrderedCollection new.
	model systemCategoryList do: [:each|
		(each beginsWith: prefix)
			ifTrue: [subcategories add: (each allButFirst: prefix size)]].
	^ subcategories collect: [:each| self class with: prefix, each name: each model: model]! !

!methodRemoval: ClassOrganizer #renameCategory:toBe: stamp: 'len 2/1/2022 19:46:07'!
ClassOrganizer removeSelector: #renameCategory:toBe:!
!methodRemoval: Categorizer #renameCategory:toBe: stamp: 'len 2/1/2022 19:45:13'!
Categorizer removeSelector: #renameCategory:toBe:!
