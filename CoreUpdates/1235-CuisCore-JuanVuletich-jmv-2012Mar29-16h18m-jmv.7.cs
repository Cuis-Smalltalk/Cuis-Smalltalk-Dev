'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 6:59:36 pm'!
!classDefinition: #SinglePackageBrowser category: #'Package Support'!
Browser subclass: #SinglePackageBrowser
	instanceVariableNames: 'package'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!
!classDefinition: #SingleSetChangeSorter category: #'Tools-Changes'!
ChangeSorter subclass: #SingleSetChangeSorter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!SingleSetChangeSorter commentStamp: '<historical>' prior: 0!
                A Change Sorter limited to operate on a single ChangeSet.!


!CodePackage methodsFor: 'listing' stamp: 'jmv 3/29/2012 18:36'!
  extensionClassNamesIn: aSystemCategory
	^ (SystemOrganization listAtCategoryNamed: aSystemCategory) select: [ :className |
		(self extensionCategoriesForClass: (Smalltalk at: className)) notEmpty ]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/29/2012 18:32'!
                     systemCategoriesWithExtensionMethods

	^ SystemOrganization categories select: [ :cat |
		(SystemOrganization listAtCategoryNamed: cat) anySatisfy: [ :className |
			(self extensionCategoriesForClass: (Smalltalk at: className)) notEmpty ]]! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 3/29/2012 16:36'!
                               browseChanges
	| current |
	current _ model selection.
	current ifNil: [ ^self ].

	ChangeSorterWindow
		open: (SingleSetChangeSorter new 
			myChangeSet: (ChangeSet changeSetForPackage: current))
		label: nil! !


!SinglePackageBrowser methodsFor: 'accessing' stamp: 'jmv 3/29/2012 17:17'!
                             defaultBrowserTitle
	^ 'Browser for package: ', package name! !

!SinglePackageBrowser methodsFor: 'accessing' stamp: 'jmv 3/29/2012 16:43'!
                   package: aCodePackage

	package _ aCodePackage! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 18:51'!
     classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer _ selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer listAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn: (selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer size = 0 ifFalse: [
			selectedClassName _ answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 18:54'!
   messageCategoryList
	"Answer the selected category of messages."

	"Do not include the -- all -- category"
	| answer |
	answer _ self rawMessageCategoryList.
	selectedMessageCategory ifNil: [
		answer size = 0 ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessageCategory _ answer first ]]].
	^answer! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 18:59'!
                       messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!) (??), it is reset to zero."

	| answer |
	answer _ selectedMessageCategory
		ifNil: [ #() ]
		ifNotNil: [
			(self classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory) ifNil: [
				selectedMessageCategory _ nil.
				#() ]].
	selectedMessage ifNil: [
		answer size = 0 ifFalse: [
			selectedMessage _ answer first.
			self editSelection: #editMessage ]].
	^answer! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 18:18'!
                  rawMessageCategoryList
	"Answer the selected category of messages."


	(package includesSystemCategory: selectedSystemCategory)
		ifTrue: [
			^super rawMessageCategoryList reject: [ :cat | package isForeignClassExtension: cat ]]
		ifFalse: [
			^super rawMessageCategoryList select: [ :cat | package isYourClassExtension: cat ]]! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 18:33'!
          systemCategoryList

	^package systemCategories,
		(package systemCategoriesWithExtensionMethods collect: [ :cat | '*', cat ])! !


!SingleSetChangeSorter methodsFor: 'accessing' stamp: 'jmv 3/29/2012 16:37'!
                changeSetList

	^{ myChangeSet name }! !


!Browser methodsFor: 'class list' stamp: 'jmv 3/29/2012 18:24'!
                     classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	^selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [systemOrganizer listAtCategoryNamed: selectedSystemCategory]! !

!Browser methodsFor: 'message category list' stamp: 'jmv 3/29/2012 18:00'!
    messageCategoryList
	"Answer the selected category of messages."
	^selectedClassName
		ifNil: [ #() ]
		ifNotNil: [ (Array with: ClassOrganizer allCategory), self rawMessageCategoryList ]! !

!Browser methodsFor: 'message category list' stamp: 'jmv 3/29/2012 17:50'!
                     messageCategoryListIndex
	"Answer the index of the selected message category."


	selectedMessageCategory ifNil: [ ^0 ].
	^self messageCategoryList indexOf: selectedMessageCategory! !

!Browser methodsFor: 'message category list' stamp: 'jmv 3/29/2012 17:53'!
                            messageCategoryListIndex: anInteger
	"Set the selected message category to be the one indexed by anInteger."

	selectedMessageCategory _ anInteger = 0 ifFalse: [self messageCategoryList at: anInteger].
	selectedMessage _ nil.
	self changed: #messageCategorySelectionChanged.
	self changed: #messageCategoryListIndex. "update my selection"
	self changed: #messageList.
	self editSelection: (anInteger > 0
		ifTrue: [#newMessage]
		ifFalse: [self classListIndex > 0
			ifTrue: [#editClass]
			ifFalse: [#newClass]]).
	self acceptedContentsChanged.! !

!Browser methodsFor: 'message category list' stamp: 'jmv 3/29/2012 18:08'!
               rawMessageCategoryList

	^ selectedClassName
		ifNil: [ #() ]
		ifNotNil: [ self classOrMetaClassOrganizer categories ]! !

!Browser methodsFor: 'system category list' stamp: 'jmv 3/29/2012 17:16'!
                          systemCategoryListIndex
	"Answer the index of the selected class category."

	systemOrganizer ifNil: [ ^0 ].
	selectedSystemCategory ifNil: [ ^0 ].
	^self systemCategoryList indexOf: selectedSystemCategory! !

!Browser methodsFor: 'system category list' stamp: 'jmv 3/29/2012 17:05'!
    systemCategoryListIndex: anInteger 
	"Set the selected system category index to be anInteger. Update all other 
	selections to be deselected."

	selectedSystemCategory _ anInteger = 0 ifFalse: [ self systemCategoryList at: anInteger].
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self editSelection: ( anInteger = 0 ifTrue: [#none] ifFalse: [#newClass]).
	metaClassIndicated _ false.
	self setClassOrganizer.
	self changed: #systemCategorySelectionChanged.
	self changed: #systemCategoryListIndex.	"update my selection"
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 3/29/2012 17:58'!
                 buildMorphicMessageCatList
	| myMessageCatList |
	myMessageCatList _ PluggableListMorph
		model: model
		listGetter: #messageCategoryList
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:
		mainView: self
		menuGetter: #messageCategoryMenu
		keystrokeAction: nil.
	myMessageCatList enableDragNDrop: false.
	^myMessageCatList! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/29/2012 16:30'!
     buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow description summary buttonRow browseChangesButton saveButton browseButton createButton deleteButton |
	dirtyFlags _ PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: Color white.
	dirtyFlags _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	names _ PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: Color white.
	names _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: 16;
		addMorphUseAll: names.

	fileNames _ PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: Color white.
	fileNames _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: 16;
		addMorphUseAll: fileNames.

	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.1;
		addAdjusterAndMorph: names proportionalWidth: 0.2;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.7.

	description _ TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.

	summary _ TextModelMorph
		textProvider: model
		textGetter: #summary.

	saveButton _ PluggableButtonMorph model: model action: #save label: 'Save (overwrite)'.
	createButton _ PluggableButtonMorph model: self action: #createPackage label: 'Create Package'.
	deleteButton _ PluggableButtonMorph model: self action: #deletePackage label: 'Delete (merge in Cuis)'.
	browseChangesButton _ PluggableButtonMorph model: self action: #browseChanges label: 'Browse unsaved changes'.
	browseButton _ PluggableButtonMorph model: self action: #browse label: 'Browse package code'.
	buttonRow _ LayoutMorph newRow.
	buttonRow
		color: self windowColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: summary proportionalHeight: 0.13;
		addAdjusterAndMorph: description proportionalHeight: 0.3;
		addAdjusterAndMorph: buttonRow proportionalHeight: 0.07.
	self setLabel: 'Installed Packages'! !

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 3/29/2012 16:43'!
                  browse

	| current browser |
	current _ model selection.
	current ifNil: [ ^self ].

	browser _ SinglePackageBrowser new.
	browser package: current.
	BrowserWindow open: browser label: browser labelString! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'jmv 3/29/2012 17:16'!
 classListIndex: newIndex
	"Cause system organization to reflect appropriate category"
	| newClassName ind i |
	newIndex ~= 0 ifTrue: [
		newClassName _ (classList at: newIndex) copyWithout: $ .
		i _ systemOrganizer numberOfCategoryOfElement: newClassName.
		selectedSystemCategory _ i = 0 ifFalse: [ self systemCategoryList at: i]].
	ind _ super classListIndex: newIndex.
	self changed: #systemCategorySingleton.
	^ ind! !


!SingleSetChangeSorter reorganize!
('accessing' changeSetList)
!


!SinglePackageBrowser reorganize!
('accessing' defaultBrowserTitle package:)
('lists' classList messageCategoryList messageList rawMessageCategoryList systemCategoryList)
!

!methodRemoval: CodePackageListWindow #diffs!
CodePackageListWindow removeSelector: #diffs!
!methodRemoval: CodePackage #extensionClassesIn:!
CodePackage removeSelector: #extensionClassesIn:!
!classRemoval: #PluggableMessageCategoryListMorph!
Smalltalk removeClassNamed: #PluggableMessageCategoryListMorph!
