Class {
	#name : 'SystemOrganizerTest',
	#superclass : 'DynamicallyCodeCreationTest',
	#category : 'BaseImageTests-System-Support',
	#package : 'BaseImageTests',
	#tag : 'System-Support'
}

{ #category : 'testing' }
SystemOrganizerTest >> testRenameSystemCategoryAlsoRenameClassExtensions [
	
	| oldCategoryName oldExtensionName newCategoryName newExtensionName classWithCategoryToRename classOrganizer metaclassOrganizer |

	oldCategoryName := 'ACategoryToRename'. 
	oldExtensionName := '*', oldCategoryName, ' SubCategory'.
	newCategoryName := self classCategoryOfTestData.
	newExtensionName := '*', newCategoryName, ' SubCategory'.
	classWithCategoryToRename := self createClassNamed: #AClassWithCategoryToRename.
	[ classOrganizer := classWithCategoryToRename organization.
	classOrganizer addCategory: oldExtensionName.
	metaclassOrganizer := classWithCategoryToRename class organization.
	metaclassOrganizer addCategory: oldExtensionName.

	SystemOrganization renameSystemCategory: oldCategoryName to: newCategoryName.

	self assert: classWithCategoryToRename category equals: newCategoryName.
	self deny: (classOrganizer categories includes: oldExtensionName).
	self assert: (classOrganizer categories includes: newExtensionName).
	self deny: (metaclassOrganizer categories includes: oldExtensionName).
	self assert: (metaclassOrganizer categories includes: newExtensionName) ] ensure: [
		SystemOrganization removeSystemCategory: oldCategoryName ].

]
