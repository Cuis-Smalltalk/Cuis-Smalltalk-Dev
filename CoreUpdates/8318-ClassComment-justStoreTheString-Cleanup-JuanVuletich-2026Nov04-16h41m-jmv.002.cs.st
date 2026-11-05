'From Cuis7.9 [latest update: #8315] on 4 November 2026 at 5:06:30 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 7:57:37 pm (UTC)'!
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| header |

	(aString isEmpty) & (self organization hasNoComment) ifTrue: [
		self organization classComment: nil.
		^self ].
		"never had a class comment, no need to write empty string out"
	
	SourceFiles ifNotNil: [ | file |
		(file := SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!!.	"directly"
			header := String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm ].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.

	Smalltalk userChangesAppendDo: [ :stream |
		stream newLine; nextPut: $!!.	"directly"
		header := String streamContents: [:strm | strm nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm ].
		stream nextChunkPut: header.
		stream newLine; nextChunkPut: aString ].

	SystemChangeNotifier uniqueInstance classCommented: self.! !


!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 4/Nov/2026, 5:53:24 pm (UTC)'!
classComment
	^classComment
		ifNil: [ '' ]
		ifNotNil: [ classComment string ]! !

!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 4/Nov/2026, 6:08:30 pm (UTC)'!
classComment: aString 
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	classComment := aString.! !

!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 4/Nov/2026, 6:05:26 pm (UTC)'!
classComment: aString  stamp: aStamp
	"Store the comment, aString, associated with the object that refers to the receiver."

	commentStamp := aStamp.
	classComment := aString.! !

!BasicClassOrganizer methodsFor: 'private' stamp: 'jmv 4/Nov/2026, 7:59:02 pm (UTC)'!
hasClassComment

	^classComment notNil and: [^classComment string notNil]! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 4/Nov/2026, 7:59:51 pm (UTC)'!
getSourceFromFile
	"Read the source code from file, determining source file index and
	file position from the last 3 bytes of this method."
	| position |
	(position := self filePosition) = 0 ifTrue: [^ nil].
	^ (RemoteString newFileNumber: self fileIndex position: position) string! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 4/Nov/2026, 8:00:34 pm (UTC)'!
removeEmptyClassComments

	| newChangeList newList |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep |
		keep := true.
		(chRec isClassComment and: [
				Smalltalk includesKey: chRec changeClassName]) ifTrue: [
			keep := chRec string notEmpty
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

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 4/Nov/2026, 8:00:48 pm (UTC)'!
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
					ifFalse: [
						((cls sourceCodeAt: sel ifAbsent: nil)  = str and: [
							chRec isMethodDefinition and: [
							(cls includesSelector: sel) and: [
							chRec properties isNil or: chRec properties = (cls compiledMethodAt: sel) propertiesAsArray ]]])
						not ]]].
		(chRec isClassComment and: [
				name := chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls := Smalltalk at: name.
			keep := cls organization classComment ~= chRec string ].
		(chRec isClassDefinition and: [
				name := chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls := Smalltalk at: name.
			chRec isMetaClassChange ifTrue: [ cls := cls class ].
			keep := cls definition ~= chRec string ].
		keep ifTrue: [
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList := newChangeList.
		list := newList.
		listIndex := 0.
		self clearSelections ].
	self changed: #list! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 4/Nov/2026, 8:00:29 pm (UTC)'!
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
			ifFalse: [ ^aChange string copy ].
		^DifferenceFinder
				displayPatchFrom: (aClass sourceCodeAt: aSelector)
				to: aChange string
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [aClass]) ].
	(aChange isClassDefinition and: [
			name := aChange changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		aClass := Smalltalk at: name.
		aChange isMetaClassChange ifTrue: [ aClass := aClass class ].
		^DifferenceFinder
				displayPatchFrom: aClass definition to: aChange string tryWords: true].
	(aChange isClassComment
			and: [(aClass := aChange changeClass) notNil]) ifTrue: [
		^DifferenceFinder
				displayPatchFrom: aClass smalltalkComment to: aChange string tryWords: self shouldDiffWords].
	^(changeList at: listIndex) string! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 4/Nov/2026, 8:00:52 pm (UTC)'!
undiffedContents
	^ listIndex = 0
		ifTrue: ['']
		ifFalse: [(changeList at: listIndex) string]! !


!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 4/Nov/2026, 8:00:57 pm (UTC)'!
diffedVersionContents
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].
	change := changeList at: listIndex.
	later := change string.
	class := change changeClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].

	earlier := (changeList at: listIndex + 1) string.

	^DifferenceFinder
		displayPatchFrom: earlier to: later
		tryWords: self shouldDiffWords
		prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class])! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 4/Nov/2026, 8:01:00 pm (UTC)'!
undiffedContents
	^ listIndex = 0
		ifTrue: ['']
		ifFalse: [(changeList at: listIndex) string]! !


!ClassCommentChange methodsFor: 'initialization' stamp: 'jmv 4/Nov/2026, 8:01:23 pm (UTC)'!
fileIn
	"File the receiver in.  Apply the change.  Update the class comment."

	(Smalltalk at: class)
		comment: self string
		stamp: stamp.! !


!MethodDefinitionChange methodsFor: 'initialization' stamp: 'jmv 4/Nov/2026, 8:01:39 pm (UTC)'!
fileIn
	"File the receiver in.  Apply the change.  Define the method."

	self changeClass ifNotNil: [ :methodClass |
		methodClass
			compile: self string
			classified: category
			withStamp: stamp
			methodProperties: properties
			notifying: nil ].! !

!methodRemoval: ChangeListElement #text stamp: 'jmv 4/Nov/2026, 8:02:59 pm (UTC)'!
ChangeListElement removeSelector: #text!
!methodRemoval: RemoteString #isRemote stamp: 'jmv 4/Nov/2026, 8:04:44 pm (UTC)'!
RemoteString removeSelector: #isRemote!
!methodRemoval: RemoteString #text stamp: 'jmv 4/Nov/2026, 8:03:03 pm (UTC)'!
RemoteString removeSelector: #text!
!methodRemoval: ProcessBrowser #text stamp: 'jmv 4/Nov/2026, 8:03:56 pm (UTC)'!
ProcessBrowser removeSelector: #text!
!methodRemoval: Text #isRemote stamp: 'jmv 4/Nov/2026, 8:04:45 pm (UTC)'!
Text removeSelector: #isRemote!
!methodRemoval: CharacterSequence #isRemote stamp: 'jmv 4/Nov/2026, 8:04:47 pm (UTC)'!
CharacterSequence removeSelector: #isRemote!
!methodRemoval: BasicClassOrganizer #postCopy stamp: 'jmv 4/Nov/2026, 7:48:44 pm (UTC)'!
BasicClassOrganizer removeSelector: #postCopy!

!RemoteString reorganize!
('accessing' compilerClass historyPointer last position string)
('private' fileNumber:position: string:onFileNumber: string:onFileNumber:toFile:)
!

