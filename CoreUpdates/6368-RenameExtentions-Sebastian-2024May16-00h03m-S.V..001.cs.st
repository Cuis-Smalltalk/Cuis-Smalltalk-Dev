'From Cuis7.1 [latest update: #6367] on 16 May 2024 at 12:06:13 am'!
!classDefinition: #SystemOrganizerTest category: #'BaseImageTests-System-Support'!
TestCase subclass: #SystemOrganizerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BaseImageTests-System-Support'!

!Categorizer methodsFor: 'testing' stamp: 'S.V. 5/16/2024 00:04:40'!
isPackageCategoryExtension: aCategory fromPackage: aPackageName

	^ (self isPackageCategoryExtension: aCategory) and: [ aCategory 	is: aPackageName substringAt: 2 ]! !


!ClassOrganizer methodsFor: 'accessing' stamp: 'S.V. 5/16/2024 00:05:12'!
renamePackageCategories: oldPackageName to: newPackageName

	self categories do: [ :aCategory |
		(self isPackageCategoryExtension: aCategory fromPackage: oldPackageName) ifTrue: [
			| newCategory |
			newCategory := aCategory copyReplaceFrom: 2 to: (oldPackageName size + 1) with: newPackageName.
			self renameCategory: aCategory to: newCategory.
		]
	]! !


!SystemOrganizerTest methodsFor: 'testing' stamp: 'S.V. 5/16/2024 00:03:44'!
testRenameSystemCategoryAlsoRenameClassExtensions
	| oldCategoryName oldExtensionName newCategoryName newExtensionName aClassWithCategoryToRename classOrganizer |

	oldCategoryName := 'ACategoryToRename'.
	oldExtensionName := '*ACategoryToRename SubCategory'.
	newCategoryName := 'NewCategoryName'.
	newExtensionName := '*NewCategoryName SubCategory'.
	aClassWithCategoryToRename := Object subclass: #AClassWithCategoryToRename
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: oldCategoryName.
	classOrganizer := aClassWithCategoryToRename organization.
	classOrganizer addCategory: oldExtensionName.

	self assert: aClassWithCategoryToRename category equals: oldCategoryName.
	self assert: classOrganizer categories includes: oldExtensionName.
	
	SystemOrganization renameSystemCategory: oldCategoryName to: newCategoryName.

	self assert: aClassWithCategoryToRename category equals: newCategoryName.
	self deny: (classOrganizer categories includes: oldExtensionName).
	self assert: (classOrganizer categories includes: newExtensionName).

	SystemOrganization removeSystemCategory: newCategoryName.! !


!SystemOrganizer methodsFor: 'categories' stamp: 'S.V. 5/16/2024 00:05:52'!
renameSystemCategory: oldName to: newName.

	"If it's a top category, rename subcategories first:"
	(self withSubCategoriesOf: oldName) reverseDo: [ :each |
		self renameCategory: each to: newName, (each allButFirst: oldName size)].

	Smalltalk classes do: [ :aClass | 
		aClass organization renamePackageCategories: oldName to: newName
	]! !


!SystemOrganizerTest reorganize!
('testing' testRenameSystemCategoryAlsoRenameClassExtensions)
!

