"
A class-comment-versions-browser tool
"
Class {
	#name : 'ClassCommentVersionsBrowser',
	#superclass : 'VersionsBrowser',
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'utilities' }
ClassCommentVersionsBrowser class >> commentRecordsOf: aClass [
	"Return a list of ChangeRecords for all versions of the method at selector. Source code can be retrieved by sending string to any one.  Return nil if the method is absent."

	| aList |
	aList := self new
			scanVersionsOf: aClass.
	^ aList ifNotNil: [aList changeList]
]

{ #category : 'utilities' }
ClassCommentVersionsBrowser class >> timeStampFor: aSelector class: aClass reverseOrdinal: anInteger [
	"Answer the time stamp corresponding to some version of the given method, nil if none.  The reverseOrdinal parameter is interpreted as:  1 = current version; 2 = last-but-one version, etc."
	
	| aChangeList |
	aChangeList :=  self new scanVersionsOf: aClass.
	^ aChangeList ifNil: [nil] ifNotNil:
		[aChangeList list size >= anInteger
			ifTrue:
				[(aChangeList changeList at: anInteger) stamp]
			ifFalse:
				[nil]]
]

{ #category : 'misc' }
ClassCommentVersionsBrowser >> classCommentIndicated [
	"Answer whether the receiver is pointed at a class comment"

	^ true
]

{ #category : 'menu' }
ClassCommentVersionsBrowser >> compareToCurrentVersion [
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| change s1 s2 differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	change := changeList at: listIndex.
	s1 := classOfMethod organization classComment.
	s2 := change string.
	s1 = s2
		ifTrue: [^ self inform: 'Exact Match'].
	diffWords := self shouldDiffWords.
	differDesc := diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].
	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: s1 to: s2
			tryWords: diffWords))
				openLabel: 'Comparison to Current Version: ', differDesc
]

{ #category : 'basic function' }
ClassCommentVersionsBrowser >> diffedVersionContents [
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].
	change := changeList at: listIndex.
	later := change text.
	class := self selectedClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].

	earlier := (changeList at: listIndex + 1) text.

	^DifferenceFinder
		displayPatchFrom: earlier to: later
		tryWords: self shouldDiffWords
]

{ #category : 'menu' }
ClassCommentVersionsBrowser >> offerVersionsHelp [
	(TextModel new contents: self versionsHelpString)
		openLabel: 'Class Comment Versions Browsers'
]

{ #category : 'misc' }
ClassCommentVersionsBrowser >> priorSourceOrNil [
	"If the currently-selected method has a previous version, return its source, else return nil"
	| changeRecords |
	self selectedClass ifNil: [^ nil].
	self selectedMessageName ifNil: [^ nil].
	changeRecords :=  self class commentRecordsOf: self selectedClass.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [^ nil].
	^ (changeRecords at: 2) string 

]

{ #category : 'basic function' }
ClassCommentVersionsBrowser >> reformulateList [

     classOfMethod organization classComment ifNil: [^ self].

	self scanVersionsOf: classOfMethod.
	self changed: #list. "for benefit of mvc"
	listIndex := 1.
	self changed: #listIndex.
	self acceptedContentsChanged
]

{ #category : 'basic function' }
ClassCommentVersionsBrowser >> scanVersionsOf: class [
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
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos := nil ].
		position := prevPos.
		prevPos notNil ifTrue: [ file := sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections
]

{ #category : 'misc' }
ClassCommentVersionsBrowser >> selectedClass [
	"Answer the class currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod
]

{ #category : 'basic function' }
ClassCommentVersionsBrowser >> updateIfNeeded [
	| aComment |
	aComment := classOfMethod organization commentRemoteStr.
	aComment == currentCompiledMethod
		ifFalse:
			["Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aComment last ~= 0 ifTrue: [self reformulateList]].
	^ true

]

{ #category : 'misc' }
ClassCommentVersionsBrowser >> wantsPrettyDiffOption [
	"Answer whether pretty-diffs are meaningful for this tool"

	^ false
]

{ #category : 'controls' }
ClassCommentVersionsBrowser >> whatToShowSubmenuSpec [
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 	nil. 	#codeProvider. 	#showSource. 		nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingLineDiffsString. 	nil. 	#codeProvider. 	#showLineDiffs. 	nil. 'the textual source lines diffed from its prior version'}.
	}.

]
