"
I am a class browser view on a fileout (either a source file (.st) or change set (.cs.st)). I do not actually load the code into to the system, nor do I alter the classes in the image. Use me to vet code in a comfortable way before loading it into your image.

From a FileList, I can be invoked by selecting a source file and selecting the "browse code" menu item from the context (right click) menu.

I use PseudoClass, PseudoClassOrganizers, and PseudoMetaclass to model the class structure of the source file.
"
Class {
	#name : 'CodeFileBrowser',
	#superclass : 'Browser',
	#instVars : [
		'baseCodeSource',
		'caseCodeSource'
	],
	#category : 'Tools-Code File Browser',
	#package : 'Tools',
	#tag : 'Code File Browser'
}

{ #category : 'instance creation' }
CodeFileBrowser class >> browseCodeFileEntry: aFileEntry [
	^ self browseCodeSource: (CodeFile newFromFile: aFileEntry )
]

{ #category : 'instance creation' }
CodeFileBrowser class >> browseCodeSource: aCaseCodeSource [
	^ self browseCodeSource: aCaseCodeSource base: nil
]

{ #category : 'instance creation' }
CodeFileBrowser class >> browseCodeSource: aCaseCodeSource base: aBaseCodeSource [
	| useCaseCodeSource useCaseClasses browser useCaseOrganizer useHeading |
	browser := self new.
	browser
		caseCodeSource: aCaseCodeSource;
		baseCodeSource: aBaseCodeSource.
	useCaseCodeSource := browser caseCodeSource.
	useCaseClasses := useCaseCodeSource classes collect: [ :ea |
		ea name ].
	useCaseOrganizer := useCaseCodeSource organization.
	useHeading := (useCaseCodeSource isLiveSmalltalkImage not and: [ browser baseCodeSource isLiveSmalltalkImage ])
		ifTrue: [ useCaseCodeSource name ]
		ifFalse: [ "This is a non-standard configuration... make the user aware"
			useCaseCodeSource name , '(' , useCaseCodeSource class name , '), target: ' , aBaseCodeSource name , '(' , aBaseCodeSource class name , ')' ].
	(useCaseCodeSource notNil and: [ useCaseCodeSource isLiveSmalltalkImage not ]) ifTrue: [
		useCaseOrganizer
			classifyAll: useCaseClasses
			under: useHeading ].
	browser
		systemOrganizer: useCaseOrganizer;
		caseCodeSource: useCaseCodeSource.
	aBaseCodeSource ifNotNil: [ browser baseCodeSource: aBaseCodeSource ].
	^ CodeFileBrowserWindow
		open: browser
		label: nil.
]

{ #category : 'instance creation' }
CodeFileBrowser class >> browsePackageFileEntry: aFileEntry [
	^ self browseCodeSource: (CodePackageFile newFromFile: aFileEntry )
]

{ #category : 'instance creation' }
CodeFileBrowser class >> fileReaderServicesForFileEntry: filename suffix: suffix [
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(#('st' 'cs' 'cs.st') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowsePackage } ].

	^#()
]

{ #category : 'instance creation' }
CodeFileBrowser class >> serviceBrowseCode [
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse code'
		selector: #browseCodeFileEntry:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code'
		icon: #editFindReplaceIcon)
			sortOrder: 10
]

{ #category : 'instance creation' }
CodeFileBrowser class >> serviceBrowsePackage [
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse package'
		selector: #browsePackageFileEntry:
		description: 'open a "package file browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'package browser'
		icon: #editFindReplaceIcon)
			sortOrder: 10
]

{ #category : 'accessing' }
CodeFileBrowser >> acceptedStringOrText [
	self updateInfoView.
	(editSelection == #newClass and: [ caseCodeSource notNil ])
		ifTrue: [ ^caseCodeSource description ].
	editSelection == #editClass
		ifTrue:[ ^self modifiedClassDefinition ].
	^super acceptedStringOrText
]

{ #category : 'shout styling' }
CodeFileBrowser >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable
]

{ #category : 'infoView' }
CodeFileBrowser >> annotation [
	^self infoViewContents
]

{ #category : 'accessing' }
CodeFileBrowser >> baseCodeSource [
	^ baseCodeSource ifNil: [ Smalltalk ].
]

{ #category : 'accessing' }
CodeFileBrowser >> baseCodeSource: aCodeFile [
	baseCodeSource := aCodeFile
]

{ #category : 'change list' }
CodeFileBrowser >> browseCodeFileChanges [

	caseCodeSource browseFileChanges.
]

{ #category : 'accessing' }
CodeFileBrowser >> caseCodeSource [
	^ caseCodeSource ifNil: [ Smalltalk ].
]

{ #category : 'accessing' }
CodeFileBrowser >> caseCodeSource: aCodeFile [
	caseCodeSource := aCodeFile
]

{ #category : 'class list' }
CodeFileBrowser >> classList [
	"Answer a collection of the class names of the selected category. Answer an 
	empty array if no selection exists."

	(selectedSystemCategory isNil or: [ caseCodeSource isNil ])
		ifTrue: [ ^#() ].

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [(caseCodeSource classes collect: [:ea| ea name]) sort].
]

{ #category : 'accessing' }
CodeFileBrowser >> contents: input notifying: aRequestor  [
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| codeString theClass |
	codeString := input string.

	editSelection == #editComment 
		ifTrue: [
			theClass := self selectedClass.
			theClass ifNil: [
				self inform: 'You must select a class
before giving it a comment.'.
				^ false ].
			theClass comment: codeString. 
			^ true ].

	self inform:'You cannot change the current selection'.
	^false
]

{ #category : 'accessing' }
CodeFileBrowser >> contentsSymbol [
	"Answer a symbol indicating what kind of content should be shown for the method"

	^ contentsSymbol ifNil: [
		contentsSymbol := self defaultDiffsSymbol ]
]

{ #category : 'class list' }
CodeFileBrowser >> createHierarchyTreeOf: pseudoClasses  [
	"Create a tree from a collection of pseudoclasses"
	
	^self createHierarchyTreeOf: pseudoClasses comparingWith: [:aPotentialSuperClass 			:aPotentialSubClass | aPotentialSuperClass name = aPotentialSubClass superclassName].
]

{ #category : 'other' }
CodeFileBrowser >> didCodeChangeElsewhere [
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass |
	(aClass := self selectedClassOrMetaClass) ifNil: [^ false].

	(aClass is: #PseudoClass) ifTrue: [^ false]. "class not installed"
	^super didCodeChangeElsewhere
]

{ #category : 'infoView' }
CodeFileBrowser >> extraInfo [
	^ (self
		methodDiffFor: (self selectedClassOrMetaClass sourceCodeAt: self selectedMessageName)
		selector: self selectedMessageName)
			hasAnyAttribute
				ifTrue: [' - **MODIFIED**']
				ifFalse: [' - identical']
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileIn [
	caseCodeSource fileIn
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileInClass [

	self selectedClass fileIn
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileInMessage [
	
	self selectedMessageName ifNil: [^self].
	self selectedClassOrMetaClass fileInMethod: self selectedMessageName
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileInMessageCategories [

	self selectedClassOrMetaClass fileInCategory: self selectedMessageCategoryName
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileOut [

	caseCodeSource fileOut
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileOutClass [

	self selectedClass fileOut
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileOutMessage [

	self selectedMessageName ifNil: [^self].
	self selectedClassOrMetaClass fileOutMethod: self selectedMessageName
]

{ #category : 'fileIn/fileOut' }
CodeFileBrowser >> fileOutMessageCategories [

	self selectedClassOrMetaClass fileOutCategory: self selectedMessageCategoryName
]

{ #category : 'class list' }
CodeFileBrowser >> hierarchicalClassList [
	"Order the pseudoclasses filed-in lexicographically. Then order them hierarchically."

	| pseudoClasses |
	pseudoClasses := caseCodeSource classes sorted: [:firstPseudoClass :secondPseudoClass | firstPseudoClass name < secondPseudoClass name].
	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: pseudoClasses)
		on: OrderedCollection new
		indent: ''.
]

{ #category : 'infoView' }
CodeFileBrowser >> infoViewContents [
	| theClass selector useLabel |
	useLabel := self baseCodeSource baseLabel.
	editSelection == #newClass ifTrue: [
		^ caseCodeSource
			ifNil: [ 'No file selected' ]
			ifNotNil: [ caseCodeSource summary ]].
	self selectedClass ifNil: [ ^ '' ].
	theClass := self pvtBaseClassOrMetaclass.
	editSelection == #editClass ifTrue: [
		^ theClass
			ifNil: [ 'Class not in the ' , useLabel ]
			ifNotNil: [ 'Class exists already in the ' , useLabel ]].
	editSelection == #editMessage ifFalse: [ ^ '' ].
	selector := self selectedMessageName.
	^ (theClass notNil and: [ theClass includesSelector: selector ])
		ifTrue: [ 'Method already exists' , self extraInfo ]
		ifFalse: [			
			(self classOrMetaClassOrganizer isRemoved: selector)
				ifTrue: [ 'Method not in the ' , useLabel ]
				ifFalse: [ '**NEW** Method not in the ' , useLabel ]]
]

{ #category : 'testing' }
CodeFileBrowser >> isEditingExistingClass [

	^false
]

{ #category : 'testing' }
CodeFileBrowser >> isEditingExistingMethod [

	^false
]

{ #category : 'testing' }
CodeFileBrowser >> isEditingMethod [

	^false
]

{ #category : 'testing' }
CodeFileBrowser >> isEditingNewClass [

	^false
]

{ #category : 'other' }
CodeFileBrowser >> labelString [
	"Answer the string for the window title"

	^ selectedSystemCategory ifNil: ['']
]

{ #category : 'message list' }
CodeFileBrowser >> messageList [
	"Colorize messages as needed"
	^ super messageList collect: [ :eaListItem | | useAttr |
		useAttr := (self classOrMetaClassOrganizer isRemoved: eaListItem)
			ifTrue: [ TextColor red ]
			ifFalse: [ | baseSrc |
				baseSrc := self pvtBaseSelectedMessageSourceCodeFor: eaListItem.
				baseSrc
					ifNil: [ TextColor green ]
					ifNotNil: [ | caseSrc |
						caseSrc := self pvtCaseSelectedMessageSourceCodeFor: eaListItem.
						baseSrc = caseSrc ifFalse: [ TextColor blue ]]].
		useAttr
			ifNil: [ eaListItem ]
			ifNotNil: [ :attr |
				Text
					string: eaListItem
					attribute: attr ]].
]

{ #category : 'diffs' }
CodeFileBrowser >> methodDiffFor: aString selector: selector [
	"Answer the diff between the current copy of the given class/selector/meta for the string provided"
	| theClass base |
	base := ''.
	theClass := self pvtBaseClassOrMetaclass.
	theClass ifNotNil: [
		(theClass includesSelector: selector) ifTrue: [
			base := theClass sourceCodeAt: selector ]].
	^ DifferenceFinder
		displayPatchFrom: base
		to: aString
		tryWords: self shouldDiffWords
		prettyPrintedIn:
			(self showingAnyKindOfPrettyDiffs ifTrue: [ theClass ]).
]

{ #category : 'diffs' }
CodeFileBrowser >> modifiedClassDefinition [
	| pClass rClass old new |
	pClass := self selectedClassOrMetaClass.
	pClass ifNil: [^''].
	pClass hasDefinition ifFalse: [ ^pClass definition].
	rClass := [self baseCodeSource classNamed: self selectedClass name] on: Error do: ["Missing class"].
	rClass ifNil: [ ^pClass definition].
	self metaClassIndicated ifTrue:[ rClass := rClass class].
	old := rClass definition.
	new := pClass definition.
	^ DifferenceFinder displayPatchFrom: old to: new tryWords: true
]

{ #category : 'private' }
CodeFileBrowser >> pvtBaseClassOrMetaclass [
	| theClass |
	theClass := [self baseCodeSource classNamed: self selectedClass name asSymbol] on: Error do: ["Class not found in base?"].
	^ (theClass notNil and: [ self metaClassIndicated ])
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
]

{ #category : 'private' }
CodeFileBrowser >> pvtBaseSelectedMessageSourceCodeFor: selector [
	^ self pvtBaseClassOrMetaclass ifNotNil: [ :theClass |
		(theClass includesSelector: selector) ifTrue: [ theClass sourceCodeAt: selector ]].
]

{ #category : 'private' }
CodeFileBrowser >> pvtCaseSelectedMessageSourceCodeFor: selector [
	| class  |
	class := self selectedClassOrMetaClass.
	^ class sourceCodeAt: selector.
]

{ #category : 'removing' }
CodeFileBrowser >> removeClass [
	| class |
	selectedClassName ifNil: [ ^self ].
	class := self selectedClass.
	(self confirm:'Are you certain that you
want to delete the class ', class name, '?') ifFalse:[^self].
	caseCodeSource removeClass: class.
	self classListIndex: 0.
	self changed: #classList.
]

{ #category : 'removing' }
CodeFileBrowser >> removeMessage [
	| messageName |
	selectedMessage ifNil: [ ^self ].
	messageName := self selectedMessageName.
	(self selectedClass confirmRemovalOf: messageName)
		ifFalse: [^ false].
	self selectedClassOrMetaClass removeMethod: self selectedMessageName.
	self reformulateList.
	self setClassOrganizer.
	"In case organization not cached"
	self changed: #messageList
]

{ #category : 'removing' }
CodeFileBrowser >> removeMessageCategory [
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	selectedMessageCategory ifNil: [ ^self ].
	messageCategoryName := self selectedMessageCategoryName.
	(self messageList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?']) ifFalse: [^ self].
	self selectedClassOrMetaClass removeCategory: messageCategoryName.
	self messageCategoryListIndex: 0.
	self changed: #messageCategoryList.
]

{ #category : 'removing' }
CodeFileBrowser >> removeUnmodifiedCategories [
	| theClass |
	theClass := self selectedClass.
	theClass ifNil: [^self].
	theClass removeUnmodifiedMethods: theClass selectors.
	theClass theMetaClass removeUnmodifiedMethods: theClass theMetaClass selectors.
	self messageCategoryListIndex: 0.
	self changed: #messageCategoryList
]

{ #category : 'removing' }
CodeFileBrowser >> removeUnmodifiedClasses [
	caseCodeSource isLiveSmalltalkImage
		ifTrue: [ self error: 'Do not perform on a live image!' ]
		ifFalse: [
			caseCodeSource classDictionary copy do: [ :theClass |
				theClass removeAllUnmodified.
				theClass hasChanges ifFalse: [ caseCodeSource removeClass: theClass ]].
			self classListIndex: 0.
			self changed: #classList ].
]

{ #category : 'removing' }
CodeFileBrowser >> removeUnmodifiedMethods [
	| theClass cat |
	theClass := self selectedClassOrMetaClass.
	theClass ifNil: [ ^self].
	cat := self selectedMessageCategoryName.
	cat ifNil: [ ^self].
	theClass removeUnmodifiedMethods: (theClass organization listAtCategoryNamed: cat).
	self reformulateList.
	self changed: #messageList.
]

{ #category : 'class list' }
CodeFileBrowser >> renameClass [
	| oldName newName |
	selectedClassName ifNil: [ ^self ].
	oldName := self selectedClass name.
	self request: 'New class name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName := aString asSymbol.
			(caseCodeSource classDictionary includesKey: newName)
				ifTrue: [self error: newName , ' already exists in the CodeFile'].
			systemOrganizer classify: newName under: selectedSystemCategory.
			systemOrganizer removeElement: oldName.
			caseCodeSource renameClass: self selectedClass to: newName.
			self changed: #classList.
			self classListIndex: ((systemOrganizer listAtCategoryNamed: selectedSystemCategory) indexOf: newName)]
]

{ #category : 'edit pane' }
CodeFileBrowser >> selectedBytecodes [
	"Not supported"
	^''
]

{ #category : 'class list' }
CodeFileBrowser >> selectedClass [
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	^selectedClassName ifNotNil: [
		caseCodeSource classNamed: selectedClassName ]
]

{ #category : 'metaclass' }
CodeFileBrowser >> selectedClassOrMetaClass [
	"Answer the selected class or metaclass."

	| cls |
	self metaClassIndicated
		ifTrue: [^ (cls := self selectedClass) ifNotNil: [cls theMetaClass]]
		ifFalse: [^ self selectedClass]
]

{ #category : 'edit pane' }
CodeFileBrowser >> selectedMessage [
	"Answer a copy of the source code for the selected message selector."

	| class selector answer |
	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	answer := class sourceCodeAt: selector.
	(self classOrMetaClassOrganizer isRemoved: selector) ifTrue: [
		^ Text
			string: answer
			attribute: TextColor red ].
	(Preferences at: #browseWithPrettyPrint) ifTrue: [
		answer := class compilerClass new
						format: answer in: class notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		answer := self
			methodDiffFor: answer
			selector: self selectedMessageName ].
	^ answer
]

{ #category : 'metaclass' }
CodeFileBrowser >> setClassOrganizer [
	"Install whatever organization is appropriate"
	| theClass |
	classOrganizer := nil.
	metaClassOrganizer := nil.
	selectedClassName ifNil: [ ^self ].
	theClass := self selectedClass.
	theClass ifNil: [classOrganizer := self baseCodeSource organization.
		metaClassOrganizer := self baseCodeSource organization] ifNotNil: [
	classOrganizer := theClass organization.
	metaClassOrganizer := theClass theMetaClass organization.
	
]
]

{ #category : 'infoView' }
CodeFileBrowser >> updateInfoView [

	self changed: #infoViewContents
]

{ #category : 'controls' }
CodeFileBrowser >> whatToShowSubmenuSpec [
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 		nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingLineDiffsString. 		nil. 	#codeProvider. 	#showLineDiffs. 			nil. 'the textual source lines diffed from its prior version'}.
		{3050.	self showingWordDiffsString. 		nil. 	#codeProvider. 	#showWordDiffs. 			nil. 'the textual source words diffed from its prior version'}.
		{3060.	self showingPrettyLineDiffsString. 	nil. 	#codeProvider. 	#showPrettyLineDiffs. 		nil. 'formatted source lines diffed from formatted prior version'}.
		{3070.	self showingPrettyWordDiffsString. nil. 	#codeProvider. 	#showPrettyWordDiffs.	nil. 'formatted source words diffed from formatted prior version'}.
	}.

]
