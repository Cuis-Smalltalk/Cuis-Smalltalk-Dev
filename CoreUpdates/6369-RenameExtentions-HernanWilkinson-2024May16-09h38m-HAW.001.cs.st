'From Cuis7.1 [latest update: #6368] on 16 May 2024 at 10:02:21 am'!

!ClassOrganizer methodsFor: 'renaming' stamp: 'HAW 5/16/2024 09:41:31'!
renamePackageCategoriesFrom: oldPackageName to: newPackageName

	self categories do: [ :aCategory |
		(self isPackageCategoryExtension: aCategory fromPackage: oldPackageName) ifTrue: [
			self renamePackageCategory: aCategory from: oldPackageName to: newPackageName.
		]
	]! !

!ClassOrganizer methodsFor: 'renaming' stamp: 'HAW 5/16/2024 10:01:30'!
renamePackageCategory: aCategory from: oldPackageName to: newPackageName

	| newCategory |
			
	newCategory := aCategory copyReplaceFrom: 2 to: oldPackageName size + 1 with: newPackageName.
	self renameCategory: aCategory to: newCategory! !


!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 5/16/2024 09:57:40'!
renameSystemCategory: oldName to: newName

	"If it's a top category, rename subcategories first:"
	(self withSubCategoriesOf: oldName) reverseDo: [ :each |
		self renameCategory: each to: newName, (each allButFirst: oldName size)].

	Smalltalk allBehaviorsDo: [ :aClass | 
		aClass organization renamePackageCategoriesFrom: oldName to: newName ]! !

!methodRemoval: ClassOrganizer #renamePackageCategories:to: stamp: 'HAW 5/16/2024 09:41:31'!
ClassOrganizer removeSelector: #renamePackageCategories:to:!

!ClassOrganizer reorganize!
('private' notifyOfChangedCategoriesFrom:to: notifyOfChangedCategoryFrom:to: notifyOfChangedSelector:from:to: notifyOfChangedSelectorsOldDict:newDict:)
('accessing' addCategory:before: changeFromCategorySpecs: classify:under:suppressIfDefault: removeCategory: removeElement: removeEmptyCategories setDefaultList: sortCategories)
('renaming' renameCategory:to: renamePackageCategoriesFrom:to: renamePackageCategory:from:to:)
!


!SystemOrganizer reorganize!
('fileIn/Out' fileOutAllCategories fileOutCategory: fileOutCategory:on:initializing: fileOutCategoryNoPoolsNoInit:on: objectForDataStream: superclassOrderIn:)
('remove' categoriesMatching: removeEmptySystemCategories removeMissingClasses removeSystemCategories: removeSystemCategory:)
('categories' allClassesAt: classesAt: fullListAtCategoryNamed: hasCategory: renameSystemCategory:to: subCategoriesOf: subCategoriesPatternOf: superclassOrderInAll: testCaseClassesAt: withSubCategoriesOf:)
('source code management' classMethodCountOf: instanceMethodCountOf: linesOfCodeOf: messageSendsCountOf:)
!

