'From Cuis7.1 [latest update: #6829] on 21 November 2024 at 12:43:07 pm'!

!FileList methodsFor: 'file list' stamp: 'jmv 11/21/2024 12:33:58'!
fileList
	"Answer the list of display strings for the files in the current volume."

	| font namePad sizePad sizeWithCommasPad |
	font := Preferences at: #standardListFont.
	namePad := list inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad := (list inject: 0 into: [ :mx :entry | mx max: entry fileSize]) printString size.
	sizeWithCommasPad := (list inject: 0 into: [ :mx :entry | mx max: entry fileSize]) printStringWithCommas size.
	^list collect: [ :e |
		self fileNameFormattedFrom: e
			namePad: namePad
			sizePad: sizePad
			sizeWithCommasPad: sizeWithCommasPad ].! !

!FileList methodsFor: 'file list' stamp: 'jmv 11/21/2024 12:23:23'!
fileListIndex: anInteger
	"Select the file name having the given index, and display its contents."

	listIndex := anInteger.
	fileName := listIndex = 0
		"open the file selected"
		ifFalse: [
			(list at: anInteger) name ].

	brevityState := #needToGetBrief.
	self changed: #fileListIndex.
	self acceptedContentsChanged.
	self triggerEvent: #updateButtonRow! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 11/21/2024 12:17:04'!
addNewFile

	self 
		addNew: 'file'
		byEvaluating: [ :newName | | index |
			(directory // newName) assureExistence.
			self updateFileList.
			index := (1 to: list size) detect: [ :i |
				(list at: i) name = newName ] ifNone: [ 0 ].
			self fileListIndex: index ].! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 11/21/2024 12:23:36'!
renameFile
	"Rename the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	self request: 'New file name?'
 		initialAnswer: fileName
		verifying: [:response| response notEmpty and: [response asFileName ~= fileName]]
		do: [:response|
			| newName |
			newName := response asFileName.
			directory // fileName rename: newName.
			self updateFileList.
			listIndex := list findFirst: [:item | item name = newName].
			listIndex > 0 ifTrue: [fileName := newName].
			self changed: #fileListIndex.
			self triggerEvent: #updateButtonRow]! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 11/21/2024 12:33:37'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| selected |
	directory ifNil: [^#()].
	directory invalidateChildren.
	selected := Set new.
	patternsThatSelect do: [ :selPat |
		directory childrenDo: [ :entry |
			(entry isDirectoryEntry
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ 
					(self doesPattern: selPat allow: entry) and: [
							patternsThatReject noneSatisfy: [ :rejPat | rejPat match: entry name ]]])
						ifTrue: [ selected add: entry ]]].
		
	^selected asArray sort: self sortBlock.! !

!FileList methodsFor: 'private' stamp: 'jmv 11/21/2024 12:19:36'!
defaultContents
	| n |
	acceptedContentsCache := list
			ifNil: [String new]
			ifNotNil: [
				"In the contents pane, list only up to a certain number of files."
				n := list size min: 100.
				String streamContents: [ :s | 
					s nextPutAll: 'NO FILE SELECTED'; newLine.
					s nextPutAll: '  -- Folder Summary --'; newLine.
					n < list size ifTrue: [
						s nextPutAll: 'MANY FILES...'; newLine  ].
					1 to: n do: [ :i | s nextPutAll: (list at: i) name; newLine].
					n < list size ifTrue: [
						s nextPutAll: '  -- End of this Folder Summary --'; newLine.
						s nextPutAll: '  -- Additional files not listed here --'; newLine ]]].
	brevityState := #FileList.
	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'jmv 11/21/2024 12:24:06'!
resort: newMode
	"Re-sort the list of files."

	| name |
	listIndex > 0
		ifTrue: [name := (list at: listIndex) name ].
	sortAscending := sortMode = newMode
		ifTrue: [ sortAscending not ]
		ifFalse: [ newMode = #name].
	sortMode := newMode.
	self pattern: pattern.
	name ifNotNil: [
		fileName := name.
		listIndex := list findFirst: [:item | item name = name. ].
		self changed: #fileListIndex].
	listIndex = 0 ifTrue: [
		self acceptedContentsChanged ].
	self triggerEvent: #updateButtonRow
! !

!methodRemoval: FileList #fileNameFromFormattedItem: stamp: 'jmv 11/21/2024 12:36:36'!
FileList removeSelector: #fileNameFromFormattedItem:!
