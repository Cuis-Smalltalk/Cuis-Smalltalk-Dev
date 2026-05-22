"
A ChangeList represents a list of changed methods that reside on a file in fileOut format.  The classes and methods in my list are not necessarily in this image!  Used as the model for both Version Lists and Changed Methods.

It holds three lists:
	changeList - a list of ChangeRecords
	list - a list of one-line printable headers
	listSelections - a list of Booleans (true = selected, false = not selected) multiple OK.
	listIndex 
Items that are removed (removeDoits, remove an item) are removed from all three lists.
Most recently clicked item is the one showing in the bottom pane.
"
Class {
	#name : 'ChangeList',
	#superclass : 'CodeProvider',
	#instVars : [
		'changeList',
		'list',
		'listIndex',
		'listSelections',
		'file'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'public access' }
ChangeList class >> browseCondensedChanges: aFileEntry [
	"Opens a changeList on a fileStream
	Condense older versions. Remove doits."
	| changeList |
	changeList := self new.
	aFileEntry readStreamDo: [ :stream |
		changeList scanFile: stream from: 0 to: stream size ].
	changeList
		removeOlderMethodVersions;
		removeDoIts.
	ChangeListWindow open: changeList label: 'Condensed changes from: ', aFileEntry name.
]

{ #category : 'public access' }
ChangeList class >> browseContents: aFileEntry [
	"Opens a changeList on a fileStream"
	| changeList fileSize charCount |
	aFileEntry readStreamDo: [ :stream |
		fileSize := stream size.
		charCount := fileSize.
		charCount > 1000000 ifTrue: [
			(self confirm: 'The file ', aFileEntry name , '
is really long (' , charCount printString , ' characters).
Would you prefer to view only the last million characters?')
				ifTrue: [ charCount := 1000000 ]].
		changeList := self new
			scanFile: stream from: fileSize-charCount to: fileSize.
	].
	ChangeListWindow open: changeList label: aFileEntry name
]

{ #category : 'public access' }
ChangeList class >> browseFrom: startPosition on: aChangesFileEntry labeled: aLabel [
	
	"
	ChangeList browseFrom: Smalltalk lastQuitLogPosition on: Smalltalk currentChangesName labeled: 'Lost changes'
	"
	
	| changeList end |

	aChangesFileEntry readStreamDo: [ :changesFile |
		end := changesFile size.
		changeList := self new
			scanFile: changesFile
			from: startPosition 
			to: end.
	].

	ChangeListWindow open: changeList label: aLabel
]

{ #category : 'public access' }
ChangeList class >> browsePackageContents: aFileEntry [
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	packageFile := CodePackageFile onFileEntry: aFileEntry.
	changeList := aFileEntry readStreamDo: [ :stream |
		self new scanFile: stream from: 0 to: stream size ].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new
				methodReference: methodReference;
				doItOnlyIfInBaseSystem: false)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new
				className: clsName;
				doItOnlyIfInBaseSystem: false)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name
]

{ #category : 'public access' }
ChangeList class >> browseRecent: charCount on: origChangesFileEntry  [
	"Opens a changeList on the end of the specified changes log file"
	
	| changeList end |
	origChangesFileEntry readStreamDo: [ :changesFile |
		end := changesFile size.
		changeList := self new
			scanFile: changesFile
			from: (0 max: end - charCount)
			to: end.
	].
	ChangeListWindow open: changeList label: 'Recent changes'
]

{ #category : 'public access' }
ChangeList class >> browseRecentLog [
	"ChangeList browseRecentLog"
	"Prompt with a menu of how far back to go to browse the current image's changes log file"
	^ self
		browseRecentLogOn: Smalltalk currentChangesName asFullFileEntry
		startingFrom: Smalltalk lastQuitLogPosition
]

{ #category : 'public access' }
ChangeList class >> browseRecentLogOn: origChangesFileEntry [
	"figure out where the last snapshot or quit was, then browse the recent entries."

	| end done block positions prevBlock |
	origChangesFileEntry readStreamDo: [ :changesFile |
		positions := SortedCollection new.
		end := changesFile size.
		prevBlock := end.
		block := end - 1024 max: 0.
		done := false.
		[ done or: [ positions notEmpty ]]
			whileFalse: [
				changesFile position: block.
				"ignore first fragment"
				changesFile nextChunk.
				[ changesFile position < prevBlock ]
					whileTrue: [
						| pos chunk |
						pos := changesFile position.
						chunk := changesFile nextChunk.
						((Smalltalk isQuitRecord: chunk) or: [ Smalltalk isSnapshotRecord: chunk ])
							ifTrue: [ positions add: pos ]].
				block = 0
					ifTrue: [done := true]
					ifFalse: [
						prevBlock := block.
						block := block - 1024 max: 0]].
	].
	self browseRecentLogOn: origChangesFileEntry startingFrom: (positions isEmpty ifTrue: [0] ifFalse: [positions last])
]

{ #category : 'public access' }
ChangeList class >> browseRecentLogOn: origChangesFileEntry startingFrom: initialPos  [
	"Prompt with a menu of how far back to go when browsing a changes file."

	| end banners positions pos chunk i |
	origChangesFileEntry readStreamDo: [ :changesFile |
		banners := OrderedCollection new.
		positions := OrderedCollection new.
		end := changesFile size.
		pos := initialPos.
		[pos = 0
			or: [banners size > 20]] 		"Go back at most 20 image exits"
			whileFalse: [
				changesFile position: pos.
				chunk := changesFile nextChunk.
				i := chunk indexOfSubCollection: 'priorSource: ' startingAt: 1.
				i > 0
					ifTrue: [
						positions addLast: pos.
						banners addLast: (chunk copyFrom: 5 to: i - 2).
						pos := Number readFrom: (chunk copyFrom: i + 13 to: chunk size)]
					ifFalse: [
						pos := 0]].
	].
	positions isEmpty
		ifTrue: [ pos := 0 ]
		ifFalse: [
			positions addLast: 0.
			banners addLast: 'Whole file'.
			pos := (SelectionMenu labelList: banners selections: positions)
						startUpWithCaption: 'Browse as far back as...'.
			pos
				ifNil: [^ self]].
	self browseRecent: end - pos on: origChangesFileEntry
]

{ #category : 'fileIn/Out' }
ChangeList class >> fileReaderServicesForFileEntry: filename suffix: suffix [
	"FileList buttons that open a ChangeListWindow on contents"

	| coda sampleCoda |
	(#('st' 'cs' 'cs.st') includes: suffix)
		ifTrue: [ ^ {self serviceContents}  ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].


	suffix = 'changes'
		ifTrue: [
			sampleCoda := '.user.159.changes'.
			filename name size > sampleCoda size ifTrue: [
				coda := filename name last: sampleCoda size.
				((coda beginsWith: '.user.') and: [ coda endsWith: '.changes' ]) ifTrue: [
					^ { self serviceContents. self serviceCondensedChanges} ]].
			^ {self serviceRecentChanges} ].

	^#()
]

{ #category : 'scanning' }
ChangeList class >> knownFileInPreambles [

	^ `{
	'classDefinition:'. 
	'classRemoval:'. 
	'methodRemoval:'. 
	'methodMoveToSomePackage:'. 
	'classMoveToSomePackage:'. 
	'provides:'. 
	'requires:'.
	'classRenamed:'. }`
]

{ #category : 'scanning' }
ChangeList class >> knownPreambles [

	^ { 'commentStamp:'. 'methodsFor:'. }, ChangeList knownFileInPreambles
]

{ #category : 'fileIn/Out' }
ChangeList class >> serviceCondensedChanges [
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'open condensed changes'
		selector: #browseCondensedChanges:
		description: 'open a changes tool on this file, including only the last version of methods and avoiding doits'
		buttonLabel: 'real changes'
		icon: #changesIcon)
			sortOrder: 20
]

{ #category : 'fileIn/Out' }
ChangeList class >> serviceContents [
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'open code changes'
		selector: #browseContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'all changes'
		icon: #changesIcon)
			sortOrder: 20
]

{ #category : 'fileIn/Out' }
ChangeList class >> servicePackageContents [
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'see package file contents'
		selector: #browsePackageContents:
		description: 'open a code file contents tool on this package file'
		buttonLabel: 'pck contents'
		icon: #changesIcon)
			sortOrder: 20
]

{ #category : 'fileIn/Out' }
ChangeList class >> serviceRecentChanges [
	"Answer a service for opening a changelist browser on the tail end of a .changes file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'see recent changes in file'
		selector: #browseRecentLogOn:
		description: 'open a changelist tool on recent changes in file'
		buttonLabel: 'recent changes'
		icon: #changesIcon
]

{ #category : 'viewing access' }
ChangeList >> acceptedStringOrText [
	"Answer the contents string, obeying diffing directives if needed"

	^ self showingAnyKindOfDiffs
		ifFalse: [
			self undiffedContents]
		ifTrue: [
			self showsVersions
				ifTrue: [
					self diffedVersionContents]
				ifFalse: [
					self contentsDiffedFromCurrent]]
]

{ #category : 'initialization-release' }
ChangeList >> addItem: item text: text [

	changeList addLast: item.
	list addLast: (text collect: [ :x | x isLineSeparator ifTrue: [$/] ifFalse: [x]])
]

{ #category : 'shout styling' }
ChangeList >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) changeType = #method ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				^true ]]].
	^false
]

{ #category : 'viewing access' }
ChangeList >> annotation [
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 |
	change := self currentChange.
	
	change isNil ifTrue: [
		count := listSelections size.
		selectedCount := listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change changeType == #classDefinition ifTrue: [
		ann1 := change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 := (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ ''].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			(self isNewMethod: change)
				ifTrue: [
					String streamContents: [ :strm | | sel |
						sel := change methodSelector.
						strm
							nextPutAll: change changeClassName;
							nextPutAll: ' >> ';
							nextPutAll: sel;
							nextPutAll: ' is not present in the system. It has '.
							count := Smalltalk numberOfImplementorsOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 implementor']
								ifFalse: [count printOn: strm. strm nextPutAll: ' implementors' ].
							strm nextPutAll: ' and '.
							count := Smalltalk numberOfSendersOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 sender.']
								ifFalse: [count printOn: strm. strm nextPutAll: ' senders.' ].
						]
					]
				ifFalse: [
					'current version: ', super annotation]]
]

{ #category : 'accessing' }
ChangeList >> changeList [
	^ changeList
]

{ #category : 'scanning' }
ChangeList >> classDefinitionRecordFrom: tokens [
	| classDefinition isMeta itemPosition className record fullClassName category stamp |
	
	itemPosition := file position.
	fullClassName := tokens second.
	isMeta := fullClassName includesSubString: ' class'.
	className := isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category := CompiledMethod classCategoryFrom: tokens.
	stamp := CompiledMethod stampFrom: tokens.
	classDefinition := file nextChunk.
	
	record := ChangeRecord new
		file: file
		position: itemPosition
		type: #classDefinition
		class: className asSymbol
		category: category
		meta: isMeta
		stamp: stamp.
	
	^record

]

{ #category : 'private' }
ChangeList >> clearSelections [
	listSelections := Array new: list size withAll: false
]

{ #category : 'menu actions' }
ChangeList >> compareToCurrentVersion [
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| base class baseString caseString differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	base := changeList at: listIndex.
	((class := base changeClass) notNil
			and: [class includesSelector: base methodSelector])
		ifTrue: [
			baseString := base string.
			caseString := (class sourceCodeAt: base methodSelector) asPlainString.
			baseString = caseString
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords := self shouldDiffWords.
			differDesc := diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: baseString to: caseString
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: (Preferences at: #standardCodeFont)))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]
]

{ #category : 'viewing access' }
ChangeList >> contentsDiffedFromCurrent [
	"Answer the contents diffed forward from current (in-memory) method version"

	|  aChange aClass  name aSelector |
	listIndex = 0
		ifTrue: [^ ''].
	aChange := changeList at: listIndex.
	((aChange changeType == #method 
			and: [(aClass := aChange changeClass) notNil]) 
			and: [aClass includesSelector: aChange methodSelector]) ifTrue: [
		aSelector := aChange methodSelector.
		(aClass notNil and: [aClass includesSelector: aSelector])
			ifFalse: [ ^aChange text copy ].
		^DifferenceFinder
				displayPatchFrom: (aClass sourceCodeAt: aSelector)
				to: aChange text
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [aClass]) ].
	(aChange changeType == #classDefinition and: [
			name := aChange changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		aClass := Smalltalk at: name.
		aChange isMetaClassChange ifTrue: [ aClass := aClass class ].
		^DifferenceFinder
				displayPatchFrom: aClass definition to: aChange text tryWords: true].
	(aChange changeType == #classComment
			and: [(aClass := aChange changeClass) notNil]) ifTrue: [
		^DifferenceFinder
				displayPatchFrom: aClass smalltalkComment to: aChange text tryWords: self shouldDiffWords].
	^(changeList at: listIndex) text
]

{ #category : 'accessing' }
ChangeList >> contentsSymbol [

	self currentChange ifNotNil: [ :change |
		(self isNewMethod: change) ifTrue: [ ^#source ]].
	^super contentsSymbol
]

{ #category : 'accessing' }
ChangeList >> currentChange [
	"return the current change being viewed, or nil if none"

	^ listIndex = 0
		ifFalse: [ changeList at: listIndex ]
]

{ #category : 'accessing' }
ChangeList >> currentCompiledMethod [
	"Actually, the CompiledMethod installed in the system that correspond to current selection"
	| selector |
	selector := self selectedMessageName.
	(selector isNil or: [ MethodSet isPseudoSelector: selector ]) ifTrue: [
		^nil ].
	^self selectedClassOrMetaClass compiledMethodAt: selector
]

{ #category : 'menu actions' }
ChangeList >> currentVersionsOfSelections [
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList := OrderedCollection new.
	1 to: changeList size do: [ :i |
		(listSelections at: i) ifTrue: [
			| aClass aChange |
			aChange := changeList at: i.
			(aChange changeType == #method
				and: [(aClass := aChange changeClass) notNil
				and: [aClass includesSelector: aChange methodSelector]])
					ifTrue: [
						aList add: (MethodReference
							class: aClass
							selector: aChange methodSelector)]]].
	^ aList
]

{ #category : 'menu actions' }
ChangeList >> deselectAll  [
	"Deselect all items in the list pane, and clear the code pane"

	listIndex := 0.
	listSelections atAllPut: false.
	self changed: #allSelections.
	self acceptedContentsChanged
]

{ #category : 'menu actions' }
ChangeList >> destroyCurrentCodeOfSelections [
	"Actually remove from the system any in-memory methods with class and selector identical to items current selected.  This may seem rather arcane but believe me it has its great uses, when trying to split out code.  To use effectively, first file out a change set that you wish to split off.  Then open a ChangeList browser on that fileout.  Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command.  For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!"

	|  aClass aChange aList |
	aList := OrderedCollection new.
	1 to: changeList size do:
		[:index |
			(listSelections at: index) ifTrue:
				[aChange := changeList at: index.
				(aChange changeType == #method
					and: [(aClass := aChange changeClass) notNil
					and: [aClass includesSelector: aChange methodSelector]])
						ifTrue:
							[aList add: {aClass. aChange methodSelector}]]].

	aList notEmpty ifTrue: [
		(self confirm: 'Warning! This will actually remove ', aList size printString,  ' method(s) from the system!') ifFalse: [^ self]].
	aList do: [ :aPair |
		Transcript newLine; show: 'Removed: ', aPair first printString, '.', aPair second.
		aPair first removeSelector: aPair second ]
]

{ #category : 'viewing access' }
ChangeList >> diffedVersionContents [
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].
	change := changeList at: listIndex.
	later := change text.
	class := change changeClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].

	earlier := (changeList at: listIndex + 1) text.

	^DifferenceFinder
		displayPatchFrom: earlier to: later
		tryWords: self shouldDiffWords
		prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class])
]

{ #category : 'accessing' }
ChangeList >> file [
	^file
]

{ #category : 'menu actions' }
ChangeList >> fileInSelections  [
	| any |
	any := false.
	listSelections with: changeList do: 
		[:selected :item | selected ifTrue: [any := true. item fileIn]].
	any ifFalse:
		[self inform: 'nothing selected, so nothing done']
]

{ #category : 'menu actions' }
ChangeList >> fileOutCurrentVersionsOfSelections [

	DirectoryEntry fileOutsDirQuery: 'CurrentVersions.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self currentVersionsOfSelections do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				moveSource: false
				toFile: 0 ]].
]

{ #category : 'menu actions' }
ChangeList >> fileOutSelections [

	DirectoryEntry fileOutsDirQuery: 'SelectedChanges.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		listSelections with: changeList do: [ :selected :item |
			selected ifTrue: [ item fileOutOn: fileStream ]]].
]

{ #category : 'initialization-release' }
ChangeList >> initialize [
	"Initialize a blank ChangeList.  Set the contentsSymbol to reflect whether diffs will initally be shown or not"

	contentsSymbol := (Preferences at: #diffsInChangeList)
		ifTrue:
			[self defaultDiffsSymbol]
		ifFalse:
			[#source].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.
	super initialize
]

{ #category : 'menu actions' }
ChangeList >> invertSelections [
	"Invert the selectedness of each item in the changelist"

	listSelections := listSelections collect: [ :ea | ea not].
	listIndex := 0.
	self changed: #allSelections.
	self acceptedContentsChanged
]

{ #category : 'testing' }
ChangeList >> isNewMethod: change [
	| class |
	^change changeType == #method and: [
		((class := change changeClass) isNil) or: [
			(class includesSelector: change methodSelector) not]]
]

{ #category : 'scanning' }
ChangeList >> itemIsRecognized: item [

	^ self class knownPreambles anySatisfy: [ :preamble | item includesSubString: preamble ] 
]

{ #category : 'viewing access' }
ChangeList >> list [
	^ list
]

{ #category : 'viewing access' }
ChangeList >> listIndex [
	^ listIndex
]

{ #category : 'viewing access' }
ChangeList >> listSelectionAt: index [
	^ listSelections at: index
]

{ #category : 'viewing access' }
ChangeList >> listSelectionAt: index put: value [

	^ listSelections at: index put: value
]

{ #category : 'menu actions' }
ChangeList >> removeDoIts [
	"Remove doits from the receiver, other than initializes. 1/26/96 sw"

	| newChangeList newList |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.

	changeList with: list do: [ :chRec :str |
		(chRec isDoIt not or: [str endsWith: 'initialize'])
			ifTrue: [
				newChangeList add: chRec.
				newList add: str]].
	newChangeList size < changeList size
		ifTrue: [
			changeList := newChangeList.
			list := newList.
			listIndex := 0.
			self clearSelections ].
	self changed: #list.

	
]

{ #category : 'menu actions' }
ChangeList >> removeEmptyClassComments [

	| newChangeList newList |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep |
		keep := true.
		(chRec changeType == #classComment and: [
				Smalltalk includesKey: chRec changeClassName]) ifTrue: [
			keep := chRec text notEmpty
		].
		keep ifTrue:[
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList := newChangeList.
		list := newList.
		listIndex := 0.
		self clearSelections ].
	self changed: #list
]

{ #category : 'menu actions' }
ChangeList >> removeNonSelections [
	"Remove the unselected items from the receiver."

	| newChangeList newList |

	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.

	1 to: changeList size do: [ :i | 
		(listSelections at: i) ifTrue: [
			newChangeList add: (changeList at: i).
			newList add: (list at: i)]].
	newChangeList isEmpty ifTrue: [
		^ self inform: 'That would remove everything.
Why would you want to do that?'].

	newChangeList size < changeList size
		ifTrue: [
			changeList := newChangeList.
			list := newList.
			listIndex := 0.
			self clearSelections ].
	self changed: #list

	
]

{ #category : 'menu actions' }
ChangeList >> removeOlderMethodVersions [
	"Remove older versions of entries from the receiver."

	| newChangeList newList found |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	found := Set new.
	changeList reverseWith: list do: [ :chRec :strNstamp | | str |
		str := strNstamp copyUpTo: $-.
		((str beginsWith: 'method: ') and: [found includes: str]) ifFalse: [
			found add: str.
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList := newChangeList reversed.
		list := newList reversed.
		listIndex := 0.
		self clearSelections ].
	self changed: #list.
]

{ #category : 'menu actions' }
ChangeList >> removeSelections [
	"Remove the selected items from the receiver.  9/18/96 sw"

	| newChangeList newList |

	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.

	1 to: changeList size do: [ :i |
		(listSelections at: i) ifFalse: [
			newChangeList add: (changeList at: i).
			newList add: (list at: i)]].
	newChangeList size < changeList size
		ifTrue: [
			changeList := newChangeList.
			list := newList.
			listIndex := 0.
			self clearSelections ].
	self changed: #list

	
]

{ #category : 'menu actions' }
ChangeList >> removeUpToDate [
	"Remove all up to date version of entries from the receiver"

	| newChangeList newList |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep cls name |
		keep := chRec isClassDeletion not or: [ chRec changeClass notNil ]. "If a class deletion, and class already gone, don't keep it"
		keep ifTrue: [
			(cls := chRec changeClass) ifNotNil: [ | sel str |
				str := chRec string.
				sel := chRec methodSelector.
				keep := chRec isMethodDeletion
					ifTrue: [cls includesSelector: sel]
					ifFalse: [(cls sourceCodeAt: sel ifAbsent: nil)  ~= str]]].
		(chRec changeType == #classComment and: [
				name := chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls := Smalltalk at: name.
			keep := cls organization classComment ~= chRec text ].
		(chRec changeType == #classDefinition and: [
				name := chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls := Smalltalk at: name.
			chRec isMetaClassChange ifTrue: [ cls := cls class ].
			keep := cls definition ~= chRec text ].
		keep ifTrue: [
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList := newChangeList.
		list := newList.
		listIndex := 0.
		self clearSelections ].
	self changed: #list
]

{ #category : 'scanning' }
ChangeList >> scanAndIgnore: item in: position [

	| record |
	
	record := ChangeRecord new 
		file: file 
		position: position 
		type: #preamble.
		
	self 
		addItem: record 
		text: ('preamble: ' , item contractTo: 160)

]

{ #category : 'scanning' }
ChangeList >> scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition libraryName: libNameOrNil [
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| itemPosition method |
	[
		itemPosition := file position.
		method := file nextChunk.
		method notEmpty ] whileTrue: [ "done when double terminators"
			self
				addItem: (ChangeRecord new
					file: file 
					position: itemPosition 
					type: #method
					class: class 
					category: category 
					meta: meta 
					stamp: stamp
					prior: priorVersionPosition
					libraryName: libNameOrNil)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (libNameOrNil ifNil: [''] ifNotNil: [' - library[', libNameOrNil, ']'])
					, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])].
]

{ #category : 'scanning' }
ChangeList >> scanClassComment: tokens [

	| className stamp record |
	
	className := tokens first.
	stamp := tokens third.
	record := ChangeRecord new 
			file: file 
			position: file position 
			type: #classComment 
			class: className 
			category: nil 
			meta: false 
			stamp: stamp.
			
	self 
		addItem: record
		text: 'class comment for ' , className, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
		
	file nextChunk.

]

{ #category : 'scanning' }
ChangeList >> scanClassDefinition: tokens [

	| record |
	
	record := self classDefinitionRecordFrom: tokens.
						
	self addItem: record text: 'classDefinition: ', record changeClassName.
	
]

{ #category : 'scanning' }
ChangeList >> scanClassRemoval: tokens [

	| doItOnlyIfInBaseSystem removeType classDefinition className description record stamp |
	
	removeType := tokens first.
	className := tokens second.
	doItOnlyIfInBaseSystem := removeType == #classMoveToSomePackage:.
	stamp := CompiledMethod stampFrom: tokens.
	classDefinition := file nextChunk.
	
	record := ClassDeletionChangeRecord 
			className: className
			definition: classDefinition 
			doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
			stamp: stamp.

	description := doItOnlyIfInBaseSystem 
			ifTrue: ['clase move to some package: '] 
			ifFalse: ['class removal: '].
			
	self 
		addItem: record
		text: description, className, '; ', stamp 
]

{ #category : 'scanning' }
ChangeList >> scanClassRenamed: tokens [

	| record stamp newName previousName preamble |

	preamble := tokens first.
	previousName := tokens second.
	newName := tokens fourth.
	stamp := CompiledMethod stampFrom: tokens.
	file nextChunk.
	
	record := ClassRenamedChangeRecord from: previousName to: newName stamp: stamp.
			
	self 
		addItem: record
		text: preamble, ' ', previousName, ' -> ', newName, '; ', stamp 
]

{ #category : 'scanning' }
ChangeList >> scanDoIt [
	
	| itemPosition command |

	itemPosition := file position.
	command := file nextChunk.
	
	command notEmpty ifTrue: [
		self
			addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
			text: 'do it: ' , (command contractTo: 160)]
]

{ #category : 'scanning' }
ChangeList >> scanFeatureProvision: tokens [

	| changeRecord feature |
	feature := Feature fromTokens: tokens.
	changeRecord := FeatureChangeRecord new
		type: #provides
		feature: feature.
	self addItem: changeRecord text: changeRecord string.
]

{ #category : 'scanning' }
ChangeList >> scanFeatureRequirement: tokens [

	| changeRecord requirement |
	requirement := FeatureRequirement fromTokens: tokens.
	changeRecord := FeatureChangeRecord new
		type: #requires
		feature: requirement.
	self addItem: changeRecord text: changeRecord string.
]

{ #category : 'scanning' }
ChangeList >> scanFile: aFile from: startPosition to: stopPosition [

	file := aFile.
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.
	file position: startPosition.

	'Scanning ', aFile localName
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [ :barBlock | self scanUpTo: stopPosition informing: barBlock ].
		
	self clearSelections
]

{ #category : 'scanning' }
ChangeList >> scanMethodDefinition: tokens [

	| stamp className priorVersionPosition libraryName |
	
	className := tokens first.
	stamp := CompiledMethod stampFrom: tokens.
	priorVersionPosition := CompiledMethod priorReferenceFrom: tokens.
	libraryName := CompiledMethod libraryNameFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorVersionPosition libraryName: libraryName ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorVersionPosition libraryName: libraryName ].
	
	self error: 'Unsupported method definition' 

]

{ #category : 'scanning' }
ChangeList >> scanMethodRemoval: tokens [

	| doItOnlyIfInBaseSystem removeType isMeta sourceCode methodReference className description record selector stamp |

	removeType := tokens first.
	className := tokens second.
	doItOnlyIfInBaseSystem := removeType == #methodMoveToSomePackage:.

	sourceCode := file nextChunk.
	isMeta := tokens third == #class.	
	selector := isMeta ifTrue: [ tokens fourth ] ifFalse: [ tokens third ].
	stamp := CompiledMethod stampFrom: tokens.

	methodReference := MethodReference new
		setClassSymbol: className
		classIsMeta: isMeta
		methodSymbol: selector
		stringVersion: className, (isMeta ifTrue: [' class '] ifFalse: [' ']), selector,'; ', stamp.
		
	record := MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
		source: sourceCode 
		stamp: stamp.
	
	description := doItOnlyIfInBaseSystem 
		ifTrue: ['method move to some package: '] 
		ifFalse: ['method removal: '].
		
	self
		addItem: record
		text: description, methodReference stringVersion
]

{ #category : 'scanning' }
ChangeList >> scanSpecificChangeRecordType [
	"Scan anything that involves more than one chunk"

	| itemPosition item tokens firstToken secondToken |

	itemPosition := file position.
	item := file nextChunk.

	(self itemIsRecognized: item) ifFalse: [
		"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
		^ self scanAndIgnore: item in: itemPosition ].

	tokens := Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		firstToken := tokens first.
		secondToken := tokens second.

		firstToken == #classDefinition:
			ifTrue: [ ^ self scanClassDefinition: tokens ].
		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ])
			ifTrue: [ ^ self scanClassRemoval: tokens ].
		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ])
			ifTrue: [ ^ self scanMethodRemoval: tokens ].
		(secondToken == #methodsFor: or: [ 
			tokens size > 2 and: [ tokens third == #methodsFor: ] ])
			ifTrue: [ ^ self scanMethodDefinition: tokens ].
		secondToken == #commentStamp:
			ifTrue: [ ^ self scanClassComment: tokens ].
		firstToken == #provides:
			ifTrue: [ ^ self scanFeatureProvision: tokens ].
		firstToken == #requires:
			ifTrue: [ ^ self scanFeatureRequirement: tokens ].
		firstToken == #classRenamed:
			ifTrue: [ ^ self scanClassRenamed: tokens ].
		]
]

{ #category : 'scanning' }
ChangeList >> scanUpTo: stopPosition informing: barBlock [
	
	[
		file skipSeparators.
		file position < stopPosition
	] whileTrue: [
		barBlock value: file position.
		"A line starting with $! means a specific ChangeRecord type"
		((file atStart or: [ file peekBack isLineSeparator]) and: [ file peekFor: $! ])
			ifTrue: [ self scanSpecificChangeRecordType]
			ifFalse: [ self scanDoIt ]]
]

{ #category : 'menu actions' }
ChangeList >> selectAll [
	listIndex := 0.
	listSelections atAllPut: true.
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'menu actions' }
ChangeList >> selectAllForAbsentClasses [
	"Selects all method definitions for which there is no counterpart method in the current image"

	| change |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		listSelections at: i put:
			((#(method classComment classDefinition) includes: change changeType)
				and: [ change changeClass isNil ])].
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'menu actions' }
ChangeList >> selectAllForThisClass [
	| name |
	self currentChange ifNil: [ ^self ].
	name := self currentChange changeClassName.
	name ifNil: [ ^self ].
	^self selectSuchThat: [ :change |
		change changeClassName = name ].
]

{ #category : 'menu actions' }
ChangeList >> selectContentsMatching [
	| pattern |
	pattern := self request: 'pattern to match' initialAnswer: ''.
	pattern isEmpty ifTrue: [^self].
	^self selectSuchThat: ((pattern includesAnyOf: '?*')
							ifTrue: [[ :change | pattern match: change string]]
							ifFalse: [[ :change | change string includesSubString: pattern]])
]

{ #category : 'menu actions' }
ChangeList >> selectEquivalentMethods [
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change := changeList at: i.
		listSelections at: i put:
			(((change changeType == #method and: [ change isMethodDeletion not ]) and: [
				(class := change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode := [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: SyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange |
							current := (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange := cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
								currentCmWithNode := [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current := currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'menu actions' }
ChangeList >> selectMethodsOlderThanCurrent [
	"Selects all method definitions for which there is some newer counterpart in the current image.
	This is, select methods that installing would be go back in time"

	| cm doSelect change class |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		doSelect := false.
		((change changeType == #method and: [ change isMethodDeletion not ] ) and: [
			((class := change changeClass) notNil)]) ifTrue: [ | current |
				cm := class compiledMethodAt: change methodSelector ifAbsent: nil.
				current := cm ifNotNil: [ cm dateAndTime ].
				current ifNotNil: [ 
					doSelect := change dateAndTime ifNil: [true] ifNotNil: [ :dateAndTime | dateAndTime < current ]]].
		listSelections at: i put: doSelect ].
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'menu actions' }
ChangeList >> selectNewMethods [
	"Selects all method definitions for which there is no counterpart method in the current image"

	| change |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		listSelections at: i put: (self isNewMethod: change)].
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'menu actions' }
ChangeList >> selectRemovalsOfSent [
	"Selects all method removal for sent methods"

	1 to: changeList size do: [ :i | | change isRemovalOfSent |
		change := changeList at: i.
		isRemovalOfSent := change isMethodDeletion and: [
			Smalltalk isThereAReferenceTo: change methodSelector ].
		isRemovalOfSent := isRemovalOfSent or:
			(change isDoIt and: [
				change string includesSubString: 'removeSelector: #' ] and: [
					Smalltalk isThereAReferenceTo: (change string copyAfterLast: $#) asSymbol ]).
		listSelections at: i put: isRemovalOfSent ].
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'menu actions' }
ChangeList >> selectSuchThat: aBlock [
	"select all changes for which block returns true"
	listSelections := changeList collect: aBlock.
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'menu actions' }
ChangeList >> selectUnchangedMethods [
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	| change class |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		listSelections at: i put:
			((change changeType == #method and:
				[(class := change changeClass) notNil]) and:
					[(class includesSelector: change methodSelector) and:
						[change string = (class sourceCodeAt: change methodSelector) asPlainString ]])].
	self changed: #allSelections.
	self changed: #annotation
]

{ #category : 'viewing access' }
ChangeList >> selectedClass [
	^(self selectedClassOrMetaClass ifNil: [ ^nil ]) theNonMetaClass 
]

{ #category : 'viewing access' }
ChangeList >> selectedClassOrMetaClass [
	| c |
	^ (c := self currentChange) ifNotNil: [c changeClass]
]

{ #category : 'viewing access' }
ChangeList >> selectedMessageName [
	| c |
	^ (c := self currentChange) ifNotNil: [c methodSelector]
]

{ #category : 'message list' }
ChangeList >> selector: oldSelector refactoredTo: newSelector  [
	
	
]

{ #category : 'accessing' }
ChangeList >> showsVersions [
	^ false
]

{ #category : 'viewing access' }
ChangeList >> toggleListIndex: newListIndex [

	listIndex ~= 0 ifTrue: [listSelections at: listIndex put: false].
	newListIndex ~= 0 ifTrue: [listSelections at: newListIndex put: true].
	listIndex := newListIndex.
	self changed: #listIndex.
	self acceptedContentsChanged
]

{ #category : 'viewing access' }
ChangeList >> undiffedContents [
	^ listIndex = 0
		ifTrue: ['']
		ifFalse: [(changeList at: listIndex) text]
]

{ #category : 'initialization-release' }
ChangeList >> wantsPrettyDiffOption [
	"Answer whether pretty-diffs are meaningful for this tool"

	^ true
]

{ #category : 'controls' }
ChangeList >> whatToShowSubmenuSpec [
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 			nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingLineDiffsString. 			nil. 	#codeProvider. 	#showLineDiffs. 			nil. 'the textual source lines diffed from its prior version'}.
		{3050.	self showingWordDiffsString. 			nil. 	#codeProvider. 	#showWordDiffs. 			nil. 'the textual source words diffed from its prior version'}.
		{3060.	self showingPrettyLineDiffsString. 		nil. 	#codeProvider. 	#showPrettyLineDiffs. 		nil. 'formatted source lines diffed from formatted prior version'}.
		{3070.	self showingPrettyWordDiffsString. 	nil. 	#codeProvider. 	#showPrettyWordDiffs. 	nil. 'formatted source words diffed from formatted prior version'}.
	}.

]

{ #category : 'menu actions' }
ChangeList >> xtraVersion [
	| change class |
	listIndex = 0
		ifTrue: [^ nil ].
	change := changeList at: listIndex.
	((class := change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifFalse: [ ^nil ].
	^change
]
