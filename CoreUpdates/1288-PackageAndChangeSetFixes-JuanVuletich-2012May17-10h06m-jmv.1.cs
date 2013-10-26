'From Cuis 4.0 of 21 April 2012 [latest update: #1287] on 17 May 2012 at 10:08:11 am'!

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 5/17/2012 09:06'!
noteRecategorizationOfClass: aClass
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	aClass wantsChangeSetLogging ifFalse: [^ self].
	self atClass: aClass add: #change.
	self addCoherency: aClass name.
	self hasUnsavedChanges: true! !


!ChangeSet methodsFor: 'method changes' stamp: 'jmv 5/17/2012 09:41'!
removeSelectorChanges: selector class: class 
	"Remove all memory of changes associated with the argument, selector, in 
	this class."

	| chgRecord |
	self hasUnsavedChanges: true.	"set the flag in any case"
	(chgRecord _ changeRecords at: class name ifAbsent: [^ self])
		removeSelector: selector.
	chgRecord hasNoChanges ifTrue: [changeRecords removeKey: class name]! !

!ChangeSet methodsFor: 'class changes' stamp: 'jmv 5/17/2012 09:45'!
noteClassForgotten: className
	"Remove from structures if class is not a superclass of some other one we are remembering"

	self hasUnsavedChanges: true.	"set the flag in any case"
	structures ifNil: [^ self].
	Smalltalk at: className ifPresent: [:cls |
		cls subclasses do: [:sub | (structures includesKey: sub) ifTrue: [
			^ self]]].  "No delete"
	structures removeKey: className ifAbsent: nil! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 5/17/2012 09:04'!
classRecategorized: aClass from: oldCategory to: newCategory

	self noteRecategorizationOfClass: aClass! !


!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 5/17/2012 09:20'!
classRecategorized: aClass from: oldCategory to: newCategory
	"If the class was moved to a dfferent package, out of the base system, record the fact in the change set.
	The actual class redefinition is done at #classDefinitionChangedFrom:to: that is also called (if the class really changed)."

	| oldPackageOrNil newPackageOrNil newChangeSet |
	newPackageOrNil _ CodePackage
		packageOfSystemCategory: newCategory
		ifNone:	nil.
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

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 5/17/2012 09:43'!
selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass
	"If the method was moved to a dfferent package, affect the package that lost the it. Tell it that it lost the method.
	The actual method redefinition is done at one of the method definition methods, that is also called."

	| newPackageOrNil newChangeSet oldPackageOrNil |
	newPackageOrNil _ CodePackage
		packageOfMethodCategory: newCategory
		ofClass: aClass
		ifNone:	nil.
	newPackageOrNil ifNotNil: [ newPackageOrNil hasUnsavedChanges: true ].
	newChangeSet _ ChangeSet changeSetForPackage: newPackageOrNil.
	newChangeSet selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass.

	oldPackageOrNil _ CodePackage
		packageOfMethodCategory: oldCategory
		ofClass: aClass
		ifNone: nil.
	oldPackageOrNil
		ifNotNil: [ oldPackageOrNil hasUnsavedChanges: true ]
		ifNil: [
			"If destination is a package, but source isn't, then record the change in the base system changeset"
			newPackageOrNil ifNotNil: [
				self changeSetForBaseSystem noteMethodMoveToOtherPackage: selector forClass: aClass ]]! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 5/17/2012 10:03'!
register: aCodePackage
	"
	Usually call #named:createIfAbsent:registerIfNew: instead
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	"
	| any base |
	InstalledPackages at: aCodePackage packageName put: aCodePackage.

	"Anything that now belongs in this package, was moved out of the base system!!"
	base _ ChangeSet changeSetForBaseSystem.
	any _ false.
	aCodePackage classes do: [ :cls |
		any _ true.
		base noteClassMoveToOtherPackage: cls ].
	aCodePackage extensionMethods do: [ :methodReference |
		methodReference isValid ifTrue: [
			any _ true.
			base
				noteMethodMoveToOtherPackage: methodReference selector
				forClass: methodReference actualClass ]].
	any ifTrue: [
		aCodePackage hasUnsavedChanges: true ].

	self triggerEvent: #installedPackagesChanged! !

