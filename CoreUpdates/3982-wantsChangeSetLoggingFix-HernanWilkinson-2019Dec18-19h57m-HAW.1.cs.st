'From Cuis 5.0 [latest update: #3981] on 18 December 2019 at 8:57:41 pm'!

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:57:53'!
aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			aboutToRenameClass: aClass
			from: oldClassName
			to: newClassName
			inCategory: aCategoryName ].
	packageOrNil ifNotNil: [ 
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:57:46'!
classAdded: aClass inCategory: aCategoryName

	| packageOrNil |

	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classAdded: aClass inCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:58:08'!
classCommented: aClass

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classCommented: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ].! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:58:19'!
classDefinitionChangedFrom: oldClass to: newClass
	"In case the class is moved from one package to another, both change sets should be affected.
	But there's no need to do it here, as #classRecategorized:from:to: is also called."

	| packageOrNil |
	
	newClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfClass: newClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classDefinitionChangedFrom: oldClass to: newClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:58:38'!
classRecategorized: aClass from: oldCategory to: newCategory
	"If the class was moved to a dfferent package, out of the base system, record the fact in the change set.
	The actual class redefinition is done at #classDefinitionChangedFrom:to: that is also called (if the class really changed)."

	| oldPackageOrNil newPackageOrNil newChangeSet |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	newPackageOrNil _ CodePackage
		packageOfSystemCategory: newCategory
		ifNone: nil.
	newPackageOrNil ifNotNil: [ newPackageOrNil hasUnsavedChanges: true ].
	newChangeSet _ ChangeSet changeSetForPackage: newPackageOrNil.
	newChangeSet noteRecategorizationOfClass: aClass.

	oldPackageOrNil _ CodePackage
		packageOfSystemCategory: oldCategory
		ifNone: nil.
	oldPackageOrNil
		ifNotNil: [ oldPackageOrNil hasUnsavedChanges: true ]
		ifNil: [
			"If destination is a package, but source isn't, then record the change in the base system changeset"
			newPackageOrNil ifNotNil: [
				self changeSetForBaseSystem noteClassMoveToOtherPackage: aClass ]]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:59:02'!
classRemoved: aClass fromCategory: aCategoryName

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classRemoved: aClass fromCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:59:14'!
classReorganized: aClass

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classReorganized: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:59:24'!
methodAdded: aCompiledMethod selector: aSymbol inClass: aClass requestor: requestor

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfMethod: aCompiledMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodAdded: aCompiledMethod
			selector: aSymbol
			inClass: aClass
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:59:32'!
methodAdded: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfMethod: aCompiledMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodAdded: aCompiledMethod
			selector: aSymbol
			inProtocol: aCategoryName
			class: aClass
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:59:39'!
methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfMethod: newMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodChangedFrom: oldMethod
			to: newMethod
			selector: aSymbol
			inClass: aClass
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 19:59:51'!
methodRemoved: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil _ CodePackage packageOfMethodCategory: aCategoryName ofClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodRemoved: aCompiledMethod
			selector: aSymbol
			inProtocol: aCategoryName
			class: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 20:00:33'!
selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass
	"If the method was moved to a dfferent package, affect the package that lost the it. Tell it that it lost the method.
	The actual method redefinition is done at one of the method definition methods, that is also called."

	| newPackageOrNil newChangeSet oldPackageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	newPackageOrNil _ CodePackage packageOfMethodCategory: newCategory ofClass: aClass ifNone: nil.
	newPackageOrNil ifNotNil: [ newPackageOrNil hasUnsavedChanges: true ].
	newChangeSet _ ChangeSet changeSetForPackage: newPackageOrNil.
	newChangeSet selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass.

	oldPackageOrNil _ CodePackage packageOfMethodCategory: oldCategory ofClass: aClass ifNone: nil.
	oldPackageOrNil
		ifNotNil: [ oldPackageOrNil hasUnsavedChanges: true ]
		ifNil: [
			"If destination is a package, but source isn't, then record the change in the base system changeset"
			newPackageOrNil ifNotNil: [
				self changeSetForBaseSystem noteMethodMoveToOtherPackage: selector forClass: aClass ]]! !

