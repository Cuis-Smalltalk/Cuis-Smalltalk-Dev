'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 21 March 2012 at 4:54:36 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 16:50'!
                           changeSetForBaseSystem

	^ChangeSorter existingOrNewChangeSetNamed: 'ChangesToBaseSystem'! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 09:25'!
                   changeSetForClass: aClass

	| package |
	package _ CodePackage
		packageOfClass: aClass
		ifNone: [ ^self changeSetForBaseSystem ].
	^self changeSetForPackage: package! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 16:51'!
     changeSetForMethod: aCompiledMethod

	| package |
	package _ CodePackage
		packageOfMethod: aCompiledMethod methodReference
		ifNone: [ ^self changeSetForBaseSystem ].
	^self changeSetForPackage: package! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 10:40'!
 changeSetForMethodCategory: categoryName ofClass: aClass

	| package |
	package _ CodePackage
		packageOfMethodCategory: categoryName
		ofClass: aClass
		ifNone: [ ^self changeSetForBaseSystem ].
	^self changeSetForPackage: package! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 16:50'!
     changeSetForPackage: aCodePackage

	^ChangeSorter existingOrNewChangeSetNamed: 'ChangesToPackage', aCodePackage name! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 09:45'!
                        changeSetForSystemCategory: aSystemCategory

	| package |
	package _ CodePackage
		packageOfSystemCategory: aSystemCategory
		ifNone: [ ^self changeSetForBaseSystem ].
	^self changeSetForPackage: package! !


!Browser methodsFor: 'system category functions' stamp: 'jmv 3/21/2012 10:10'!
renameSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection
	
	21-Mar-2012 jmv Note: This is not recorded appropriately in change sets.
	The easiest solution is to trigger #classRecategorized for all classes in the category.
	But this is not a real solution, as the resulting changeset would not do a rename,
	but create a new category (that would go to the bottom) with all the classes.
	
	In the meantime, disable the menu entry. This is not so important after all.
	"
	| oldIndex oldName newName |
	selectedSystemCategory ifNil: [ ^ self].  "no selection"
	oldIndex _ self systemCategoryListIndex.
	oldName _ selectedSystemCategory.
	newName _ self
		request: 'Please type new category name'
		initialAnswer: oldName.
	newName isEmpty
		ifTrue: [^ self]
		ifFalse: [newName _ newName asSymbol].
	oldName = newName ifTrue: [^ self].
	systemOrganizer
		renameCategory: oldName
		toBe: newName.
	self systemCategoryListIndex: oldIndex.
	self changed: #systemCategoryList.! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2012 10:12'!
                            systemCatSingletonMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	self flag: #renameSystemCategory.	"temporarily disabled"
	aMenu addList: #(
		('find class... (f)'				findClass)
		-
		('browse all'				browseAllClasses)
		('browse'					openSystemCategoryBrowser)
		-
		('fileOut'					fileOutSystemCategory				''		model)
		-
		('update'					updateSystemCategories				''		model)
"		('rename...'					renameSystemCategory				''		model)"
		('remove'					removeSystemCategory				''		model)).
	^aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2012 10:12'!
                       systemCategoryMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	self flag: #renameSystemCategory.	"temporarily disabled"
	aMenu addList: #(
		('find class... (f)'				findClass)
		('recent classes... (r)'		recent									''		model)
		-
		('browse all'				browseAllClasses)
		('browse'					openSystemCategoryBrowser)
		-
		('fileOut'					fileOutSystemCategory				''		model)
		-
		('reorganize'				editSystemCategories					''		model)
		('alphabetize'				alphabetizeSystemCategories		''		model)
		-
		('update'					updateSystemCategories				''		model)
		('add item...'				addSystemCategory					''		model)
"		('rename...'					renameSystemCategory				''		model)"
		('remove'					removeSystemCategory				''		model)
		-
		('move to top'				moveSystemCategoryTop				''		model)
		('move up'					moveSystemCategoryUp				''		model)
		('move down'				moveSystemCategoryDown			''		model)
		('move to bottom' 			moveSystemCategoryBottom			''		model)).
	^aMenu! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 3/21/2012 10:00'!
                           renameCategory: oldCatString toBe: newCatString
	"Rename a category. No action if new name already exists, or if old name does not exist."
	| index oldCategory newCategory |
	oldCategory _ oldCatString asSymbol.
	newCategory _ newCatString asSymbol.
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^ self].	"new name exists, so no action"
	(index _ categoryArray indexOf: oldCategory) = 0
		ifTrue: [^ self].	"old name not found, so no action"
	categoryArray _ categoryArray copy.  "need to change identity so smart list update will notice the change"
	categoryArray at: index put: newCategory! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 3/21/2012 11:37'!
          addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category notifying: requestor
	| priorMethodOrNil priorProtocolOrNil |
	priorMethodOrNil _ self compiledMethodAt: selector ifAbsent: nil.
	priorProtocolOrNil _ self whichCategoryIncludesSelector: selector.
	self addSelectorSilently: selector withMethod: compiledMethod.
	SystemChangeNotifier uniqueInstance doSilently: [self organization classify: selector under: category].
	priorMethodOrNil
		ifNil: [
			SystemChangeNotifier uniqueInstance 
				methodAdded: compiledMethod 
				selector: selector 
				inProtocol: category 
				class: self 
				requestor: requestor ]
		ifNotNil: [
			SystemChangeNotifier uniqueInstance 
				methodChangedFrom: priorMethodOrNil 
				to: compiledMethod 
				selector: selector 
				inClass: self 
				requestor: requestor.
			category = priorProtocolOrNil ifFalse: [
				SystemChangeNotifier uniqueInstance
					selectorRecategorized: selector
					from: priorProtocolOrNil
					to: category
					inClass: self ]]! !

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 3/21/2012 11:37'!
    addSelector: selector withMethod: compiledMethod notifying: requestor
	| priorMethodOrNil newProtocolOrNil priorProtocolOrNil |
	priorMethodOrNil _ self compiledMethodAt: selector ifAbsent: nil.
	priorProtocolOrNil _ self whichCategoryIncludesSelector: selector.
	self addSelectorSilently: selector withMethod: compiledMethod.
	newProtocolOrNil _ self whichCategoryIncludesSelector: selector.
	priorMethodOrNil
		ifNil: [
			SystemChangeNotifier uniqueInstance 
				methodAdded: compiledMethod 
				selector: selector 
				inClass: self 
				requestor: requestor ]
		ifNotNil: [
			SystemChangeNotifier uniqueInstance 
				methodChangedFrom: priorMethodOrNil 
				to: compiledMethod 
				selector: selector 
				inClass: self 
				requestor: requestor.

			newProtocolOrNil = priorProtocolOrNil ifFalse: [
				SystemChangeNotifier uniqueInstance
					selectorRecategorized: selector
					from: priorProtocolOrNil
					to: newProtocolOrNil
					inClass: self ]]! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 3/21/2012 09:37'!
        initialize
	"
	ChangeSet initialize
	"
	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:52'!
               classAdded: aClass inCategory: aCategoryName

	| changeSet |
	changeSet _ self changeSetForSystemCategory: aCategoryName.
	^changeSet classAdded: aClass inCategory: aCategoryName! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:52'!
                classCommented: aClass

	| changeSet |
	changeSet _ self changeSetForClass: aClass.
	^changeSet classCommented: aClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:52'!
            classDefinitionChangedFrom: oldClass to: newClass

	| changeSet |
	"In case the class is moved from one package to another, both change sets should be affected.
	But there's no need to do it here, as #classRecategorized:from:to: is also called."
	changeSet _ self changeSetForClass: newClass.
	^changeSet classDefinitionChangedFrom: oldClass to: newClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:52'!
classRecategorized: aClass from: oldCategory to: newCategory

	| changeSet |
	"Ambos deberian ser afectados?"
	changeSet _ self changeSetForSystemCategory: oldCategory.
	changeSet print.
	changeSet _ self changeSetForSystemCategory: newCategory.
	changeSet print.
	self flag: #ojo.
	"Atencion, afectar los 2 solo si son distintas!! (NO las categorias, sino los changesets!!!!!!!!!!)"
	^current classRecategorized: aClass from: oldCategory to: newCategory! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:53'!
                            classRemoved: aClass fromCategory: aCategoryName

	| changeSet |
	changeSet _ self changeSetForSystemCategory: aCategoryName.
	^changeSet classRemoved: aClass fromCategory: aCategoryName! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:53'!
        classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| changeSet |
	changeSet _ self changeSetForSystemCategory: aCategoryName.
	^changeSet classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:53'!
    classReorganized: aClass

	| changeSet |
	changeSet _ self changeSetForClass: aClass.
	^changeSet classReorganized: aClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:53'!
        methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor

	| changeSet |
	changeSet _ self changeSetForMethod: aMethod.
	^changeSet methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:54'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	| changeSet |
	changeSet _ self changeSetForMethod: aMethod.
	^changeSet methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:54'!
                methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	| changeSet |
	changeSet _ self changeSetForMethod: newMethod.
	^changeSet methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:54'!
                  methodRemoved: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass

	| changeSet |
	changeSet _ self changeSetForMethodCategory: aCategoryName ofClass: aClass.
	^changeSet methodRemoved: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 16:54'!
                        selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	"Ambos deberian ser afectados?"
	| changeSet |
	changeSet _ self changeSetForMethodCategory: oldCategory ofClass: aClass.
	oldCategory print.
	changeSet print.
	changeSet _ self changeSetForMethodCategory: newCategory ofClass: aClass.
	newCategory print.
	changeSet print.
self flag: #ojo.
	"Atencion, afectar los 2 solo si son distintas (no las categorias, sino los changesets!!!!!!!!!!)!!"
	^current selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass! !


!CodePackage class methodsFor: 'searching' stamp: 'jmv 3/21/2012 10:47'!
                          packageOfMethodCategory: categoryName ofClass: aClass ifNone: errorBlock

	^ InstalledPackages
		detect: [:ea | ea includesMethodCategory: categoryName ofClass: aClass]
		ifNone: errorBlock! !


!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/21/2012 09:37'!
                   triggerEvent: anEventSelector withArguments: anArgumentList

	self isBroadcasting ifTrue: [
		"Solo un par de pruebas..."
		(anEventSelector printString, ' -> ', anArgumentList printString) print.
		^super triggerEvent: anEventSelector withArguments: anArgumentList ]! !

!methodRemoval: ChangeSet class #changeSetToBaseSystem!
ChangeSet class removeSelector: #changeSetToBaseSystem!
ChangeSet initialize!
