'From Cuis7.7 [latest update: #7890] on 30 April 2026 at 3:12:40 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 12:15:18'!
classDefinitionRecordFrom: tokens
	| classDefinition isMeta itemPosition className record fullClassName category stamp |

	itemPosition := file position.
	fullClassName := tokens second.
	isMeta := fullClassName includesSubString: ' class'.
	className := isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category := CompiledMethod classCategoryFrom: tokens.
	stamp := CompiledMethod stampFrom: tokens.
	classDefinition := file nextChunk.

	record := ClassDefinitionChange new
		file: file
		position: itemPosition
		class: className asSymbol
		category: category
		meta: isMeta
		stamp: stamp.

	^record! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 11:19:54'!
scanAndIgnore: item in: position

	| record |
	record := PreambleRecord new 
		file: file 
		position: position.
	self 
		addItem: record 
		text: ('preamble: ' , item contractTo: 160).! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 11:22:27'!
scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition libraryName: libNameOrNil
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| itemPosition method |
	[
		itemPosition := file position.
		method := file nextChunk.
		method notEmpty ] whileTrue: [ "done when double terminators"
			self
				addItem: (MethodDefinitionChange new
					file: file
					position: itemPosition
					class: class
					category: category
					meta: meta
					stamp: stamp
					prior: priorVersionPosition
					libraryName: libNameOrNil)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (libNameOrNil ifNil: [''] ifNotNil: [' - library[', libNameOrNil, ']'])
					, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])].! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 12:17:20'!
scanClassComment: tokens

	| className stamp record |

	className := tokens first.
	stamp := tokens third.
	record := ClassCommentChange new
			file: file
			position: file position
			class: className
			stamp: stamp.

	self 
		addItem: record
		text: 'class comment for ' , className, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).

	file nextChunk.! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 09:58:57'!
scanClassRemoval: tokens

	| doItOnlyIfInBaseSystem removeType classDefinition className description record stamp |
	
	removeType := tokens first.
	className := tokens second.
	doItOnlyIfInBaseSystem := removeType == #classMoveToSomePackage:.
	stamp := CompiledMethod stampFrom: tokens.
	classDefinition := file nextChunk.
	
	record := ClassDeletionChange
			className: className
			definition: classDefinition 
			doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
			stamp: stamp.

	description := doItOnlyIfInBaseSystem 
			ifTrue: ['clase move to some package: '] 
			ifFalse: ['class removal: '].
			
	self 
		addItem: record
		text: description, className, '; ', stamp ! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 10:00:08'!
scanClassRenamed: tokens

	| record stamp newName previousName preamble |

	preamble := tokens first.
	previousName := tokens second.
	newName := tokens fourth.
	stamp := CompiledMethod stampFrom: tokens.
	file nextChunk.
	
	record := ClassRenamedChange from: previousName to: newName stamp: stamp.
			
	self 
		addItem: record
		text: preamble, ' ', previousName, ' -> ', newName, '; ', stamp ! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 11:20:15'!
scanDoIt
	
	| itemPosition command |
	itemPosition := file position.
	command := file nextChunk.
	command notEmpty ifTrue: [
		self
			addItem: (UnspecificRecord new file: file position: itemPosition)
			text: 'do it: ' , (command contractTo: 160)].! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 11:37:39'!
scanFeatureProvision: tokens

	| providesRecord feature |
	feature := Feature fromTokens: tokens.
	providesRecord := FeatureProvidesRecord new feature: feature.
	self addItem: providesRecord text: providesRecord string.! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 11:37:53'!
scanFeatureRequirement: tokens

	| requiresRecord requirement |
	requirement := FeatureRequirement fromTokens: tokens.
	requiresRecord := FeatureRequiresRecord new feature: requirement.
	self addItem: requiresRecord text: requiresRecord string.! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 10:00:19'!
scanMethodRemoval: tokens

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
		
	record := MethodDeletionChange
		methodReference: methodReference
		doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
		source: sourceCode 
		stamp: stamp.
	
	description := doItOnlyIfInBaseSystem 
		ifTrue: ['method move to some package: '] 
		ifFalse: ['method removal: '].
		
	self
		addItem: record
		text: description, methodReference stringVersion! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 11:49:22'!
scanUpTo: stopPosition informing: barBlock
	
	[
		file skipSeparators.
		file position < stopPosition
	] whileTrue: [
		barBlock value: file position.
		"A line starting with $!! means a specific ChangeListElement type"
		((file atStart or: [ file peekBack isLineSeparator]) and: [ file peekFor: $!! ])
			ifTrue: [ self scanSpecificChangeType]
			ifFalse: [ self scanDoIt ]]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:40:52'!
currentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList := OrderedCollection new.
	1 to: changeList size do: [ :i |
		(listSelections at: i) ifTrue: [
			| aClass aChange |
			aChange := changeList at: i.
			(aChange isMethod
				and: [(aClass := aChange changeClass) notNil
				and: [aClass includesSelector: aChange methodSelector]])
					ifTrue: [
						aList add: (MethodReference
							class: aClass
							selector: aChange methodSelector)]]].
	^ aList! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:41:04'!
destroyCurrentCodeOfSelections
	"Actually remove from the system any in-memory methods with class and selector identical to items current selected.  This may seem rather arcane but believe me it has its great uses, when trying to split out code.  To use effectively, first file out a change set that you wish to split off.  Then open a ChangeList browser on that fileout.  Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command.  For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!!"

	|  aClass aChange aList |
	aList := OrderedCollection new.
	1 to: changeList size do:
		[:index |
			(listSelections at: index) ifTrue: [
				aChange := changeList at: index.
				(aChange isMethod
					and: [(aClass := aChange changeClass) notNil
					and: [aClass includesSelector: aChange methodSelector]])
						ifTrue:
							[aList add: {aClass. aChange methodSelector}]]].

	aList notEmpty ifTrue: [
		(self confirm: 'Warning!! This will actually remove ', aList size printString,  ' method(s) from the system!!') ifFalse: [^ self]].
	aList do: [ :aPair |
		Transcript newLine; show: 'Removed: ', aPair first printString, '.', aPair second.
		aPair first removeSelector: aPair second ]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:48:10'!
removeEmptyClassComments

	| newChangeList newList |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep |
		keep := true.
		(chRec isClassComment and: [
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
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:49:00'!
removeUpToDate
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
		(chRec isClassComment and: [
				name := chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls := Smalltalk at: name.
			keep := cls organization classComment ~= chRec text ].
		(chRec isClassDefinition and: [
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
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:50:01'!
selectAllForAbsentClasses
	"Selects all method definitions for which there is no counterpart method in the current image"

	| change |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		listSelections at: i put:
			(change isMethod | change isClassDefinition | change isClassComment
				and: [ change changeClass isNil ])].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:41:41'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change := changeList at: i.
		listSelections at: i put:
			((change isMethodDefinition and: [
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
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:42:10'!
selectMethodsOlderThanCurrent
	"Selects all method definitions for which there is some newer counterpart in the current image.
	This is, select methods that installing would be go back in time"

	| cm doSelect change class |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		doSelect := false.
		(change isMethodDefinition and: [
			((class := change changeClass) notNil)]) ifTrue: [ | current |
				cm := class compiledMethodAt: change methodSelector ifAbsent: nil.
				current := cm ifNotNil: [ cm dateAndTime ].
				current ifNotNil: [ 
					doSelect := change dateAndTime ifNil: [true] ifNotNil: [ :dateAndTime | dateAndTime < current ]]].
		listSelections at: i put: doSelect ].
	self changed: #allSelections.
	self changed: #annotation.! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 30/Apr/2026 10:42:40'!
selectUnchangedMethods
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	| change class |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		listSelections at: i put:
			((change isMethodDefinition and:
				[(class := change changeClass) notNil]) and:
					[(class includesSelector: change methodSelector) and:
						[change string = (class sourceCodeAt: change methodSelector) asPlainString ]])].
	self changed: #allSelections.
	self changed: #annotation.! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 30/Apr/2026 10:36:25'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 |
	change := self currentChange.
	
	change isNil ifTrue: [
		count := listSelections size.
		selectedCount := listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change isClassDefinition ifTrue: [
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
					'current version: ', super annotation] ]! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 30/Apr/2026 10:47:58'!
contentsDiffedFromCurrent
	"Answer the contents diffed forward from current (in-memory) method version"

	|  aChange aClass  name aSelector |
	listIndex = 0
		ifTrue: [^ ''].
	aChange := changeList at: listIndex.
	((aChange isMethodDefinition
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
	(aChange isClassDefinition and: [
			name := aChange changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		aClass := Smalltalk at: name.
		aChange isMetaClassChange ifTrue: [ aClass := aClass class ].
		^DifferenceFinder
				displayPatchFrom: aClass definition to: aChange text tryWords: true].
	(aChange isClassComment
			and: [(aClass := aChange changeClass) notNil]) ifTrue: [
		^DifferenceFinder
				displayPatchFrom: aClass smalltalkComment to: aChange text tryWords: self shouldDiffWords].
	^(changeList at: listIndex) text! !

!ChangeList methodsFor: 'shout styling' stamp: 'jmv 30/Apr/2026 10:45:41'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) isMethodDefinition ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				^true ]]].
	^false! !

!ChangeList methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:41:28'!
isNewMethod: change
	| class |
	^ change isMethod and: [
		((class := change changeClass) isNil) or: [
			(class includesSelector: change methodSelector) not ]]! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 30/Apr/2026 10:00:38'!
browsePackageContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	packageFile := CodePackageFile onFileEntry: aFileEntry.
	changeList := aFileEntry readStreamDo: [ :stream |
		self new scanFile: stream from: 0 to: stream size ].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChange new
				methodReference: methodReference;
				doItOnlyIfInBaseSystem: false)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChange new
				className: clsName;
				doItOnlyIfInBaseSystem: false)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 30/Apr/2026 11:52:06'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp priorMethodRef prevPos prevFileIndex libraryName preamble tokens sourceFilesCopy |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	addedChange ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex := 0.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFullFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble := file backChunk.
		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex := nil.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod stampFrom: tokens.
				priorMethodRef := CompiledMethod priorReferenceFrom: tokens.
				priorMethodRef ifNotNil: [
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ].
				libraryName := CompiledMethod libraryNameFrom: tokens ].
 		self addItem:
				(MethodDefinitionChange new
					file: file position: position
					class: class name category: category meta: meta stamp: stamp
					prior: priorMethodRef libraryName: libraryName)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 30/Apr/2026 12:17:29'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod := class.
	oldCommentRemoteStr := class  organization commentRemoteStr.
	currentCompiledMethod := oldCommentRemoteStr.
	selectorOfMethod := #Comment.
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy := SourceFiles collect: [ :x | x ifNotNil: [x name asFullFileEntry readStream]].
	position := oldCommentRemoteStr position.
	file := sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble := file backChunk.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'commentStamp:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod field: #commentStamp: from: tokens ifAbsentOrNil: [ '' ].
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorRef |
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorRef ]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp := '<historical>'].
 		self addItem:
				(ClassCommentChange new file: file position: position
						class: class name stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos := nil ].
		position := prevPos.
		prevPos notNil ifTrue: [ file := sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections.! !

!ClassCommentVersionsBrowser methodsFor: 'misc' stamp: 'jmv 30/Apr/2026 11:43:21'!
priorSourceOrNil
	"If the currently-selected method has a previous version, return its source, else return nil"
	| changes |
	self selectedClass ifNil: [^ nil].
	self selectedMessageName ifNil: [^ nil].
	changes :=  self class commentsOf: self selectedClass.
	(changes == nil or: [changes size <= 1]) ifTrue: [^ nil].
	^ (changes at: 2) string! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 30/Apr/2026 11:40:42'!
browseVersions

	| method class browser firstChange |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		model selectedClass ifNotNil: [ :selectedClass |
			^ VersionsBrowserWindow
				browseCommentOf: selectedClass ]].
	firstChange := model xtraVersion.
	firstChange ifNil: [ ^self ].
	class := method methodClass.
	browser := VersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector
		extraChange: firstChange.
	^browser! !


!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 30/Apr/2026 11:41:06'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer 
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: nil extraChange: nil! !


!ChangeListElement methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:05:44'!
performOn: aCodeFile

	self subclassResponsibility! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 30/Apr/2026 09:59:15'!
fileOutClassDeletionFrom: aClassChangeRecord doItOnlyIfInBaseSystem: aDoIt on: stream

	| record |
	
	record := ClassDeletionChange
		className: aClassChangeRecord thisName 
		definition: aClassChangeRecord priorDefinition 
		doItOnlyIfInBaseSystem: aDoIt 
		stamp: aClassChangeRecord stamp.
		
	record fileOutOn: stream
	
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 30/Apr/2026 10:00:26'!
fileOutMethodRemovalOf: selector for: class movedToOtherPackage: moved on: stream stamp: stamp 

	| methodReference changeRecord |
	
	methodReference := MethodReference class: class selector: selector.
	
	changeRecord := MethodDeletionChange
		methodReference: methodReference
		doItOnlyIfInBaseSystem: moved
		source: (methodReference sourceCodeIfAbsent: [ 'Missing' ]) 
		stamp: stamp.
		
	changeRecord fileOutOn: stream ! !

