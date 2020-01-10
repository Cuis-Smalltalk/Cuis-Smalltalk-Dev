'From Cuis 5.0 [latest update: #4005] on 9 January 2020 at 3:57:20 pm'!
!classDefinition: #CodeFileBrowser category: #'Tools-Code File Browser'!
Browser subclass: #CodeFileBrowser
	instanceVariableNames: 'codeFile caseCodeSource baseCodeSource '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Code File Browser'!

!CodeFileBrowser methodsFor: 'accessing' stamp: 'pb 12/11/2019 01:12:35'!
baseCodeSource
	^ baseCodeSource ifNil: [ Smalltalk ].! !

!CodeFileBrowser methodsFor: 'accessing' stamp: 'pb 12/11/2019 01:12:28'!
baseCodeSource: aCodeFile
	baseCodeSource _ aCodeFile! !

!CodeFileBrowser methodsFor: 'accessing' stamp: 'pb 12/11/2019 01:12:39'!
caseCodeSource
	^ caseCodeSource ifNil: [ Smalltalk ].! !

!CodeFileBrowser methodsFor: 'accessing' stamp: 'pb 12/11/2019 01:12:44'!
caseCodeSource: aCodeFile
	caseCodeSource _ aCodeFile! !

!CodeFileBrowser methodsFor: 'private' stamp: 'pb 12/11/2019 23:51:51'!
pvtBaseClassOrMetaclass
	| theClass |
	theClass _ [self baseCodeSource classNamed: self selectedClass name asSymbol] on: Error do: ["Class not found in base?"].
	^ (theClass notNil and: [ self metaClassIndicated ])
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 12/5/2019 03:00:05'!
browseCodeFileEntry: aFileEntry
	self browseCodeSource: (CodeFile newFromFile: aFileEntry )! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 12/11/2019 01:23:20'!
browseCodeSource: aCaseCodeSource
	self browseCodeSource: aCaseCodeSource base: nil! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 12/12/2019 01:50:16'!
browseCodeSource: aCaseCodeSource base: aBaseCodeSource
	| useCaseCodeSource useCaseClasses browser useCaseOrganizer useHeading |
	browser _ self new.
	browser
		caseCodeSource: aCaseCodeSource;
		baseCodeSource: aBaseCodeSource.
	useCaseCodeSource _ browser caseCodeSource.
	useCaseClasses _ useCaseCodeSource classes collect: [ :ea |
		ea name ].
	useCaseOrganizer _ useCaseCodeSource organization.
	useHeading _ (useCaseCodeSource isLiveSmalltalkImage not and: [ browser baseCodeSource isLiveSmalltalkImage ])
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
	CodeFileBrowserWindow
		open: browser
		label: nil.! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 12/5/2019 02:59:59'!
browsePackageFileEntry: aFileEntry
	self browseCodeSource: (CodePackageFile newFromFile: aFileEntry )! !


!ClassDescription methodsFor: 'testing' stamp: 'pb 12/11/2019 23:57:24'!
hasDefinition
	^ true! !


!SystemDictionary methodsFor: 'accessing' stamp: 'pb 12/11/2019 02:56:50'!
description
	"Needed by CodeFileBrowser to use Smalltalk as the 'case' source"
	^ self name! !

!SystemDictionary methodsFor: 'accessing' stamp: 'pb 12/5/2019 02:17:02'!
name
	^ 'Image'! !

!SystemDictionary methodsFor: 'accessing' stamp: 'pb 12/11/2019 02:56:15'!
summary
	"Needed by CodeFileBrowser to use Smalltalk as the 'case' source"
	^ self name! !

!SystemDictionary methodsFor: 'private' stamp: 'pb 1/9/2020 15:28:55'!
baseLabel
	^ 'system'! !

!SystemDictionary methodsFor: 'testing' stamp: 'pb 12/5/2019 03:21:11'!
isLiveSmalltalkImage
	^ true! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'pb 12/12/2019 01:22:31'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"
	^ (self model baseCodeSource isLiveSmalltalkImage and: [ self model caseCodeSource isLiveSmalltalkImage not ])
		ifTrue: [ super optionalButtonTuples ]
		ifFalse: [ "For non-standard browser configurations assume most of the default buttons are invalid"
			#(
				#(10 'show...' #offerWhatToShowMenu 'menu of what to show in lower pane' )
			) ].! !


!CodeFile methodsFor: 'accessing' stamp: 'pb 12/11/2019 01:51:02'!
classDictionary
	^classes! !

!CodeFile methodsFor: 'accessing' stamp: 'pb 12/11/2019 23:14:13'!
classNamed: className
	^ classes at: className! !

!CodeFile methodsFor: 'accessing' stamp: 'pb 12/5/2019 00:22:02'!
organization
	^ SystemOrganizer defaultList: Array new.! !

!CodeFile methodsFor: 'testing' stamp: 'pb 12/5/2019 03:16:55'!
isLiveSmalltalkImage
	^ false! !

!CodeFile methodsFor: 'private' stamp: 'pb 1/9/2020 15:29:37'!
baseLabel
	^ 'base'! !


!CodeFile class methodsFor: 'instance creation' stamp: 'pb 12/5/2019 00:24:43'!
newFromFile: aFileEntry
	^ aFileEntry readStreamDo: [ :stream |
		self new
			fullName: aFileEntry pathName;
			buildFrom: stream ].! !


!PseudoClass methodsFor: 'accessing' stamp: 'pb 12/11/2019 23:20:55'!
theMetaClass
	^ metaClass ifNil: [ metaClass _ PseudoMetaclass new name: self name ].! !


!CodeFileBrowser methodsFor: 'accessing' stamp: 'pb 12/11/2019 01:13:28'!
acceptedStringOrText
	self updateInfoView.
	(editSelection == #newClass and: [ caseCodeSource notNil ])
		ifTrue: [ ^caseCodeSource description ].
	editSelection == #editClass
		ifTrue:[ ^self modifiedClassDefinition ].
	^super acceptedStringOrText! !

!CodeFileBrowser methodsFor: 'class list' stamp: 'pb 12/11/2019 02:39:26'!
classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	^(selectedSystemCategory isNil or: [ caseCodeSource isNil ])
		ifTrue: [ #() ]
		ifFalse: [ (caseCodeSource classes collect: [:ea| ea name]) sort ]! !

!CodeFileBrowser methodsFor: 'class list' stamp: 'pb 12/11/2019 01:53:26'!
renameClass
	| oldName newName |
	selectedClassName ifNil: [ ^self ].
	oldName _ self selectedClass name.
	newName _ (self request: 'Please type new class name'
						initialAnswer: oldName) asSymbol.
	(newName isEmpty or:[newName = oldName]) ifTrue: [^ self].
	(caseCodeSource classDictionary includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists in the CodeFile'].
	systemOrganizer classify: newName under: selectedSystemCategory.
	systemOrganizer removeElement: oldName.
	caseCodeSource renameClass: self selectedClass to: newName.
	self changed: #classList.
	self classListIndex: ((systemOrganizer listAtCategoryNamed: selectedSystemCategory) indexOf: newName).
! !

!CodeFileBrowser methodsFor: 'class list' stamp: 'pb 12/11/2019 23:14:26'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	^self selectedClassName ifNotNil: [ :scn |
		caseCodeSource classNamed: scn ]! !

!CodeFileBrowser methodsFor: 'diffs' stamp: 'pb 1/9/2020 15:22:58'!
methodDiffFor: aString class: aPseudoClass selector: selector meta: meta
	"Answer the diff between the current copy of the given class/selector/meta for the string provided"
	| theClass source |
	theClass _ self pvtBaseClassOrMetaclass.
	theClass ifNotNil: [
		meta ifTrue: [ theClass _ theClass class ].
		(theClass includesSelector: selector) ifTrue: [ source _ theClass sourceCodeAt: selector ]].
	^ DifferenceFinder
		displayPatchFrom: (source ifNil: [''])
		to: aString
		tryWords: self shouldDiffWords
		prettyPrintedIn:
			(self showingAnyKindOfPrettyDiffs ifTrue: [ theClass ]).! !

!CodeFileBrowser methodsFor: 'diffs' stamp: 'pb 12/11/2019 23:58:45'!
modifiedClassDefinition
	| pClass rClass old new |
	pClass := self selectedClassOrMetaClass.
	pClass ifNil: [^''].
	pClass hasDefinition ifFalse: [ ^pClass definition].
	rClass := [self baseCodeSource classNamed: self selectedClass name] on: Error do: ["Missing class"].
	rClass ifNil: [ ^pClass definition].
	self metaClassIndicated ifTrue:[ rClass := rClass class].
	old := rClass definition.
	new := pClass definition.
	^ DifferenceFinder displayPatchFrom: old to: new tryWords: true! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'pb 12/11/2019 01:18:26'!
fileIn
	caseCodeSource fileIn! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'pb 12/11/2019 01:14:09'!
fileOut

	caseCodeSource fileOut! !

!CodeFileBrowser methodsFor: 'infoView' stamp: 'pb 1/9/2020 15:56:50'!
infoViewContents
	| theClass useLabel |
	useLabel _ self baseCodeSource baseLabel.
	editSelection == #newClass ifTrue: [ ^ caseCodeSource
			ifNil: [ 'No file selected' ]
			ifNotNil: [ caseCodeSource summary ]].
	self selectedClass ifNil: [ ^ '' ].
	theClass _ self pvtBaseClassOrMetaclass.
	editSelection == #editClass ifTrue: [ ^ theClass
			ifNil: [ 'Class not in the ' , useLabel ]
			ifNotNil: [ 'Class exists already in the ' , useLabel ]].
	editSelection == #editMessage ifFalse: [ ^ '' ].
	(theClass notNil and: [ self metaClassIndicated ]) ifTrue: [ theClass _ theClass class ].
	^ (theClass notNil and: [ theClass includesSelector: self selectedMessageName ])
		ifTrue: [ 'Method already exists' , self extraInfo ]
		ifFalse: [ '**NEW** Method not in the ' , useLabel ].! !

!CodeFileBrowser methodsFor: 'metaclass' stamp: 'pb 12/11/2019 23:18:54'!
selectedClassOrMetaClass
	"Answer the selected class or metaclass."

	| cls |
	self metaClassIndicated
		ifTrue: [^ (cls _ self selectedClass) ifNotNil: [cls theMetaClass]]
		ifFalse: [^ self selectedClass]! !

!CodeFileBrowser methodsFor: 'metaclass' stamp: 'pb 12/11/2019 23:16:03'!
setClassOrganizer
	"Install whatever organization is appropriate"
	| theClass |
	classOrganizer _ nil.
	metaClassOrganizer _ nil.
	selectedClassName ifNil: [ ^self ].
	theClass _ self selectedClass.
	theClass ifNil: [classOrganizer := self baseCodeSource organization.
		metaClassOrganizer := self baseCodeSource organization] ifNotNil: [
	classOrganizer _ theClass organization.
	metaClassOrganizer _ theClass theMetaClass organization.
	
]! !

!CodeFileBrowser methodsFor: 'removing' stamp: 'pb 12/11/2019 01:16:45'!
removeClass
	| class |
	selectedClassName ifNil: [ ^self ].
	class _ self selectedClass.
	(self confirm:'Are you certain that you
want to delete the class ', class name, '?') ifFalse:[^self].
	caseCodeSource removeClass: class.
	self classListIndex: 0.
	self changed: #classList.! !

!CodeFileBrowser methodsFor: 'removing' stamp: 'pb 12/11/2019 23:18:54'!
removeUnmodifiedCategories
	| theClass |
	theClass _ self selectedClass.
	theClass ifNil: [^self].
	theClass removeUnmodifiedMethods: theClass selectors.
	theClass theMetaClass removeUnmodifiedMethods: theClass theMetaClass selectors.
	self messageCategoryListIndex: 0.
	self changed: #messageCategoryList! !

!CodeFileBrowser methodsFor: 'removing' stamp: 'pb 12/11/2019 02:41:02'!
removeUnmodifiedClasses
	caseCodeSource isLiveSmalltalkImage
		ifTrue: [ self error: 'Do not perform on a live image!!' ]
		ifFalse: [
			caseCodeSource classDictionary copy do: [ :theClass |
				theClass removeAllUnmodified.
				theClass hasChanges ifFalse: [ caseCodeSource removeClass: theClass ]].
			self classListIndex: 0.
			self changed: #classList ].! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 12/5/2019 00:48:37'!
serviceBrowseCode
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse code'
		selector: #browseCodeFileEntry:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code'
		icon: #editFindReplaceIcon)
			sortOrder: 10;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 12/5/2019 00:49:55'!
serviceBrowsePackage
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse package'
		selector: #browsePackageFileEntry:
		description: 'open a "package file browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'package browser'
		icon: #editFindReplaceIcon)
			sortOrder: 10;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'pb 12/11/2019 01:09:41'!
classListMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	itemColl _ OrderedCollection new.
	itemColl addAll:
		{
			{#label -> 'definition'. #object -> #model. #selector -> #editClass. #icon -> #editFindReplaceIcon} asDictionary. 
			{#label -> 'comment'. #object -> #model. #selector -> #editComment. #icon -> #editFindReplaceIcon} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl addAll:
			{
				{#label -> 'browse full (b)'. #selector -> #browseMethodFull. #icon -> #editFindReplaceIcon} asDictionary. 
				{#label -> 'class refs (N)'. #selector -> #browseClassRefs. #icon -> #classIcon} asDictionary. 
				nil. 
				{#label -> 'fileIn'. #object -> #model. #selector -> #fileInClass. #icon -> #updateIcon} asDictionary
			} ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #object -> #model. #selector -> #fileOutClass. #icon -> #fileOutIcon} asDictionary. 
			nil. 
			{#label -> 'rename...'. #object -> #model. #selector -> #renameClass. #icon -> #saveAsIcon} asDictionary. 
			{#label -> 'remove'. #object -> #model. #selector -> #removeClass. #icon -> #listRemoveIcon} asDictionary. 
			nil. 
			{#label -> 'remove existing'. #object -> #model. #selector -> #removeUnmodifiedCategories. #icon -> #deleteIcon} asDictionary
		}.
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'pb 12/11/2019 01:10:17'!
codeFileListMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Code File'.
	itemColl _ OrderedCollection new.
	itemColl addAll:
		{
			{#label -> 'find class... (f)'. #selector -> #findClass} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #object -> #model. #selector -> #fileIn} asDictionary ].
	itemColl add:
		{#label -> 'fileOut'. #object -> #model. #selector -> #fileOut} asDictionary.
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		itemColl add:
			{#label -> 'remove existing'. #object -> #model. #selector -> #removeUnmodifiedClasses} asDictionary ].
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'pb 12/11/2019 01:10:17'!
messageCategoryMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: model.
	"All the options are for the model."
	aMenu addTitle: 'Message Category'.
	itemColl _ OrderedCollection new.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #selector -> #fileInMessageCategories. #icon -> #updateIcon} asDictionary ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #selector -> #fileOutMessageCategories. #icon -> #fileOutIcon} asDictionary. 
			nil. 
			{#label -> 'reorganize'. #selector -> #editMessageCategories. #icon -> #sendReceiveIcon} asDictionary. 
			nil. 
			{#label -> 'add item...'. #selector -> #addCategory. #icon -> #newIcon} asDictionary. 
			nil. 
			{#label -> 'rename...'. #selector -> #renameCategory. #icon -> #saveAsIcon} asDictionary. 
			{#label -> 'remove'. #selector -> #removeMessageCategory. #icon -> #listRemoveIcon} asDictionary
		}.
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		itemColl addAll:
			{
				nil. 
				{#label -> 'remove existing'. #selector -> #removeUnmodifiedMethods. #icon -> #deleteIcon} asDictionary
			} ].
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'pb 12/11/2019 01:09:41'!
messageListMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message List'.
	itemColl _ OrderedCollection new.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #object -> #model. #selector -> #fileInMessage. #icon -> #updateIcon} asDictionary ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #object -> #model. #selector -> #fileOutMessage. #icon -> #fileOutIcon} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl addAll:
			{
				{#label -> 'senders (n)'. #selector -> #browseSenders. #icon -> #mailForwardIcon} asDictionary. 
				{#label -> 'implementors (m)'. #selector -> #browseImplementors. #icon -> #developmentIcon} asDictionary. 
				{#label -> 'method inheritance (h)'. #selector -> #methodHierarchy. #icon -> #goDownIcon} asDictionary. 
				{#label -> 'versions (v)'. #selector -> #browseVersions. #icon -> #clockIcon} asDictionary
			} ].
	itemColl addAll:
		{
			nil. 
			{#label -> 'remove method (x)'. #object -> #model. #selector -> #removeMessage. #icon -> #deleteIcon} asDictionary
		}.
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !

!CodeFileBrowserWindow methodsFor: 'commands' stamp: 'pb 12/11/2019 01:53:06'!
findClass
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	pattern _ (FillInTheBlankMorph request: 'Class Name?') asLowercase.
	pattern isEmpty ifTrue: [^ self].
	classNames := Set new.
	classNames addAll: model caseCodeSource classDictionary keys.
	classNames := classNames asArray select: 
		[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
	classNames isEmpty ifTrue: [^ self].
	index _ classNames size = 1
				ifTrue:	[1]
				ifFalse:	[(PopUpMenu labelArray: classNames lines: #()) startUpMenu].
	index = 0 ifTrue: [^ self].
	foundCodeFile := nil.
	foundClass := nil.
		(model caseCodeSource classDictionary includesKey: (classNames at: index)) ifTrue:[
			foundClass := model caseCodeSource classDictionary at: (classNames at: index).
			foundCodeFile := model caseCodeSource ].
	foundClass ifNotNil: [
	 	model systemCategoryListIndex: (model systemCategoryList indexOf: foundCodeFile name asSymbol).
		model classListIndex: (model classList indexOf: foundClass name) ]! !


!CodeFile methodsFor: 'accessing' stamp: 'pb 12/11/2019 02:38:00'!
classes
	^ self classDictionary values! !

!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'pb 12/11/2019 23:18:54'!
fileIn
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileInDoits].
	classOrder do:[:cls|
		cls fileInDefinition.
	].
	classes do:[:cls|
		Transcript newLine; show:'Filing in ', cls name.
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls theMetaClass fileInMethods].
	].
	doitsMark = 3 ifTrue: [ self fileInDoits ]! !

!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'pb 12/11/2019 23:18:54'!
fileOutOn: aStream
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileOutDoits: aStream].
	classOrder do:[:cls|
		cls fileOutDefinitionOn: aStream.
	].
	classes do:[:cls|
		cls fileOutMethodsOn: aStream.
		cls hasMetaclass ifTrue:[cls theMetaClass fileOutMethodsOn: aStream].
	].
	doitsMark = 3 ifTrue:[self fileOutDoits: aStream].! !

!CodeFile methodsFor: 'xtras' stamp: 'pb 12/11/2019 23:18:54'!
allMethodReferences
	"Create an answer a Set with method references for all methods in us"
	| answer className metaClass |
	answer _ Set new.
	
	classes do: [ :pseudoClass | 
		className _ pseudoClass name.
		pseudoClass selectors do: [ :selector |
			answer add: 
				(MethodReference new
					setClassSymbol: className
					classIsMeta: false
					methodSymbol: selector
					stringVersion: className, ' ' , selector) ].
		pseudoClass hasMetaclass ifTrue: [
			metaClass _ pseudoClass theMetaClass.
			metaClass selectors do: [ :selector |
				answer add: 
					(MethodReference new
						setClassSymbol: className
						classIsMeta: true
						methodSymbol: selector
						stringVersion: className, ' class ' , selector) ].
				]].
	^answer! !

!CodeFile methodsFor: 'private' stamp: 'pb 12/11/2019 23:18:54'!
metaClassDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	theClass := self getClass: (tokens at: 1).
	theClass theMetaClass definition: string.
	classOrder add: theClass theMetaClass.! !

!CodeFile methodsFor: 'private' stamp: 'pb 12/11/2019 23:18:54'!
msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) class == String]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) class == String]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass theMetaClass classComment: tokens last].
! !

!CodeFile methodsFor: 'private' stamp: 'pb 12/11/2019 23:18:54'!
removedMethod: string with: chgRec
	| class tokens firstToken secondToken thirdToken |
	tokens _ Scanner new scanTokens: string.
	tokens size >= 3 ifTrue: [
		firstToken _ tokens at: 1.
		secondToken _ tokens at: 2.
		thirdToken _ tokens at: 3.
		(tokens size = 3 and: [ secondToken == #removeSelector: or: [ secondToken == #removeSelectorIfInBaseSystem: ]]) ifTrue:[
			class _ self getClass: firstToken.
			^class perform: secondToken with: thirdToken.
		].
		(tokens size = 4 and: [ secondToken == #class and: [ thirdToken == #removeSelector: or: [ thirdToken == #removeSelectorIfInBaseSystem: ]]]) ifTrue:[
			class _ self getClass: firstToken.
			^class theMetaClass perform: thirdToken with: (tokens at: 4).
		].
	].
	doIts add: chgRec! !


!PseudoClass methodsFor: 'testing' stamp: 'pb 12/11/2019 23:18:54'!
needsInitialize
	^self hasMetaclass and:[
		self theMetaClass realClass includesSelector: #initialize]! !

!PseudoClass methodsFor: 'methods' stamp: 'pb 12/11/2019 23:18:54'!
methodChange: aChangeRecord
	aChangeRecord isMetaClassChange ifTrue:[
		^self theMetaClass addMethodChange: aChangeRecord.
	] ifFalse:[
		^self addMethodChange: aChangeRecord.
	].
! !

!PseudoClass methodsFor: 'testing method dictionary' stamp: 'pb 12/5/2019 03:20:39'!
includesSelector: aSymbol
	^ source keys includes: aSymbol.! !

!methodRemoval: PseudoClass #metaClass stamp: 'pb 1/9/2020 15:14:15'!
PseudoClass removeSelector: #metaClass!
!methodRemoval: CodeFile #classAt: stamp: 'pb 1/9/2020 15:14:15'!
CodeFile removeSelector: #classAt:!
!methodRemoval: CodeFileBrowserWindow class #browseFile: stamp: 'pb 1/9/2020 15:14:15'!
CodeFileBrowserWindow class removeSelector: #browseFile:!
!methodRemoval: CodeFileBrowserWindow class #browsePackageFile: stamp: 'pb 1/9/2020 15:14:15'!
CodeFileBrowserWindow class removeSelector: #browsePackageFile:!
!methodRemoval: CodeFileBrowserWindow #buildWindowMenu stamp: 'pb 1/9/2020 15:19:38'!
CodeFileBrowserWindow removeSelector: #buildWindowMenu!
!methodRemoval: CodeFileBrowser class #browseCode: stamp: 'pb 1/9/2020 15:14:15'!
CodeFileBrowser class removeSelector: #browseCode:!
!methodRemoval: CodeFileBrowser class #browsePackage: stamp: 'pb 1/9/2020 15:14:15'!
CodeFileBrowser class removeSelector: #browsePackage:!
!methodRemoval: CodeFileBrowser #shouldShowFalseColorDiffs stamp: 'pb 1/9/2020 15:18:31'!
CodeFileBrowser removeSelector: #shouldShowFalseColorDiffs!
!methodRemoval: CodeFileBrowser #toggleShowFalseColorDiffsLabel stamp: 'pb 1/9/2020 15:20:01'!
CodeFileBrowser removeSelector: #toggleShowFalseColorDiffsLabel!
!methodRemoval: CodeFileBrowser #codeFile: stamp: 'pb 1/9/2020 15:14:15'!
CodeFileBrowser removeSelector: #codeFile:!
!methodRemoval: CodeFileBrowser #toggleShowFalseColorDiffs stamp: 'pb 1/9/2020 15:20:12'!
CodeFileBrowser removeSelector: #toggleShowFalseColorDiffs!
!methodRemoval: CodeFileBrowser #initialize stamp: 'pb 1/9/2020 15:17:26'!
CodeFileBrowser removeSelector: #initialize!
!methodRemoval: CodeFileBrowser #selectedCodeFile stamp: 'pb 1/9/2020 15:14:16'!
CodeFileBrowser removeSelector: #selectedCodeFile!
!classDefinition: #CodeFileBrowser category: #'Tools-Code File Browser'!
Browser subclass: #CodeFileBrowser
	instanceVariableNames: 'baseCodeSource caseCodeSource'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Code File Browser'!

!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText baseCodeSource baseCodeSource: caseCodeSource caseCodeSource: contents:notifying: contentsSymbol)
('class list' classList renameClass selectedClass)
('diffs' methodDiffFor:class:selector:meta: modifiedClassDefinition)
('edit pane' selectedBytecodes selectedMessage)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('menus' contentsSymbolQuints)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' changeMessageCategories: didCodeChangeElsewhere labelString)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('shout styling' shouldStyle:with:)
('private' pvtBaseClassOrMetaclass)
!


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: printSubclassesOn:level: removeUninstantiatedSubclassesSilently subclasses subclassesDo:)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: addSelector:withMethod:notifying: allMethodCategoriesIntegratedThrough: induceMDFault methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: ultimateSourceCodeAt:ifAbsent:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: localBindingOf: moveInstVarNamed:to:after: noteCompilationOf:meta: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' doneCompiling superclass:methodDictionary:format: updateInstancesFrom:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: linesOfCode logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
!


!CodeFile reorganize!
('accessing' classDictionary classNamed: classes description fullName fullName: name organization removeClass: removeDoIts renameClass:to: sourceSystem summary)
('change record types' classComment: classDefinition: classRemoval: doIt: method: preamble:)
('fileIn/fileOut' askForDoits fileIn fileInDoits fileOut fileOutDoits: fileOutOn:)
('initialization' initialize)
('reading' buildFrom: buildFrom:informingTo:)
('testing' isLiveSmalltalkImage)
('xtras' allMethodReferences)
('private' baseLabel classDefinition:with: getClass: metaClassDefinition:with: msgClassComment:with: possibleSystemSource: removedMethod:with:)
!

