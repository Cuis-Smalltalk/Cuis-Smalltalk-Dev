"
I am model that can be used to navigate the host file system. By omitting the volume list, file list, and template panes from the view, I can also be used as the model for an editor on an individual file.

The FileList provides a dynamic extension mechanism.  To extend FileList functionality, tools should implement the following class-side method (look for implementors in the image):

#fileReaderServicesForFile:suffix: (appropriate services for given file, takes a file name and a lowercased suffix)

This method returns a collection of SimpleServiceEntry instances.  These contain a class, a menu label and a method selector having one argument.  They may also provide separate button labels and description.

The argument to the specified method will be a string representing the full name of a file when one is selected or the file list itself when there is no selected file.
"
Class {
	#name : 'FileList',
	#superclass : 'TextProvider',
	#instVars : [
		'acceptedContentsCache',
		'fileName',
		'directory',
		'list',
		'listIndex',
		'pattern',
		'sortMode',
		'brevityState',
		'sortAscending',
		'showDirsInFileList',
		'currentDirectorySelected',
		'showHidden',
		'topDirectories'
	],
	#category : 'Tools-FileList',
	#package : 'Tools',
	#tag : 'FileList'
}

{ #category : 'file reader registration' }
FileList class >> itemsForFileEntry: aFileEntry [
	"Answer a list of services appropriate for a file of the given name"
	| services suffix classList |
	"
	suffix := (FileIOAccessor default extensionFor: filename) asLowercase.
	"
	suffix := aFileEntry extension asLowercase.
	"Build the list dynamically for all implementers of the appropriate class method... registration no longer required"
	classList := (Smalltalk allClassesImplementing: #fileReaderServicesForFileEntry:suffix:)
		collect: [ :item | item theNonMetaClass ].
	services := OrderedCollection new.
	classList do: [ :reader |
		services addAll:
			(reader
				fileReaderServicesForFileEntry: aFileEntry 
				suffix: suffix) ].
	services sort: [ :a :b | a sortOrder < b sortOrder ].
	^ services.
]

{ #category : 'ui' }
FileList class >> open [
	"
	FileList open
	"
	self open: DirectoryEntry userBaseDirectory
]

{ #category : 'ui' }
FileList class >> open: aDirectory [
	"
	FileList open: '/' asDirectoryEntry
	"
	| win |
	win := FileListWindow open: (self new directory: aDirectory) label: nil.
	win model when: #updateButtonRow send: #updateButtonRow to: win.
	^ win
]

{ #category : 'file reader registration' }
FileList class >> registerFileReader: aProviderClass [
	"For compatibility... no longer necessary"
]

{ #category : 'file reader registration' }
FileList class >> unregisterFileReader: aProviderClass [
	"For compatibility... no longer necessary"
]

{ #category : 'private' }
FileList >> acceptedStringOrText [
	"Answer the contents of the file, reading it first if needed."
	"Possible brevityState values:
		FileList,
		fullFile, briefFile, needToGetFull, needToGetBrief,
		fullHex, briefHex, needToGetFullHex, needToGetBriefHex"

	(listIndex = 0) | (brevityState == #FileList) ifTrue: [ ^ self defaultContents ].  "no file selected"
	brevityState == #fullFile ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #fullHex ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #briefFile ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #briefHex ifTrue: [ ^ acceptedContentsCache ].

	brevityState == #needToGetFullHex ifTrue: [^ self readContentsHex: false ].
	brevityState == #needToGetBriefHex ifTrue: [^ self readContentsHex: true ].

	brevityState == #needToGetFull ifTrue:  [^ self readContentsBrief: false ].
	brevityState == #needToGetBrief ifTrue: [ ^ self readContentsBrief: true ].  "default"

	^''.
]

{ #category : 'file menu action' }
FileList >> addNew: aString byEvaluating: aBlock [
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	self request: ('New {1} name?' format: {aString})
		initialAnswer: ('{1}Name' format: {aString})
		verifying: [ :response | response notEmpty ]
		do: [ :response | aBlock value: response asFileName ].
]

{ #category : 'file menu action' }
FileList >> addNewDirectory [

	self 
		addNew: 'directory'
		byEvaluating: [ :newName | | newDirectory |
			newDirectory := (directory / newName) assureExistence.
			self changed: #topDirectoryWrappers.
			self directory: newDirectory.
			self updateDirectory ].
]

{ #category : 'file menu action' }
FileList >> addNewFile [

	self 
		addNew: 'file'
		byEvaluating: [ :newName | | index |
			(directory // newName) assureExistence.
			self updateFileList.
			index := (1 to: list size) detect: [ :i |
				(list at: i) name = newName ] ifNone: [ 0 ].
			self fileListIndex: index ].
]

{ #category : 'user interface support' }
FileList >> autoCompleterClassFor: textGetter [
	^SmalltalkCompleter
]

{ #category : 'accessing' }
FileList >> brevityState [
	^brevityState
]

{ #category : 'initialization' }
FileList >> buttonSelectorsToSuppress [
	"Answer a list of action selectors whose corresponding services we would prefer *not* to have appear in the filelist's button pane; this can be hand-jimmied to suit personal taste."

	^ #(addFileToNewZip: compressFile:)
]

{ #category : 'menu messages' }
FileList >> copyName [

	listIndex = 0 ifTrue: [^ self].
	Clipboard storeObject: self fullName
]

{ #category : 'private' }
FileList >> currentDirectorySelected [
	^ currentDirectorySelected
]

{ #category : 'private' }
FileList >> defaultContents [
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
	^ acceptedContentsCache
]

{ #category : 'volume list and pattern' }
FileList >> deleteDirectory [
	"Remove the currently selected directory"

	| newSelection |
	directory children isEmpty ifFalse: [ ^self inform:'Directory must be empty' ].
	(self confirm: 'Really delete ' , directory name , '?') ifFalse: [ ^ self ].
	newSelection := directory parent.
	directory delete.
	self directory: newSelection.
	self updateFileList.
	self updateDirectory.
	self changed: #topDirectoryWrappers
]

{ #category : 'file menu action' }
FileList >> deleteFile [
	"Delete the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	(self confirm: ('Really delete {1}?' format:{fileName})) ifFalse: [^ self].
	(directory // fileName) delete.
	self updateFileList.
	brevityState := #FileList.
	self get
]

{ #category : 'volume list and pattern' }
FileList >> directory [

	^ directory
]

{ #category : 'initialization' }
FileList >> directory: aDirectoryEntry [
	"Set the path of the volume to be displayed."

	directory := aDirectoryEntry.

	sortMode ifNil: [
		sortMode := #date.
		sortAscending := false ].
	self changed: #relabel.
	self pattern: pattern
]

{ #category : 'user interface support' }
FileList >> doItContext [
	^ nil
]

{ #category : 'user interface support' }
FileList >> doItReceiver [
	^ nil
]

{ #category : 'volume list and pattern' }
FileList >> doesPattern: aPattern allow: entry  [

	(aPattern = '*' or: [ aPattern match: entry name ]) ifFalse: [ ^false ].

	(showHidden not and: [ entry name first == $. ]) ifTrue: [ ^false ].

	"Hide Mac resurce forks and folder service stores"
	(entry name = '.DS_Store') ifTrue: [ ^false ].
	('._*' match: entry name) ifTrue: [ ^false ].
	^true
]

{ #category : 'initialization' }
FileList >> dynamicButtonServices [
	"Answer services for buttons that may come and go in the button pane, depending on selection"

	^ fileName isEmptyOrNil
		ifTrue:
			[#()]
		ifFalse:
			[ | toReject |
				toReject := self buttonSelectorsToSuppress.
				(self itemsForFileEntry: self selectedFileEntry) reject:
					[:svc | toReject includes: svc selector]]
]

{ #category : 'user interface support' }
FileList >> editorClassFor: textGetter [
	"Accept both on Contents and on Pattern"
	^SmalltalkEditor
]

{ #category : 'file list' }
FileList >> fileList [
	"Answer the list of display strings for the files in the current volume."

	| font namePad sizePad sizeWithCommasPad |
	font := Preferences at: #standardListFont.
	namePad := list inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad := (list inject: 0 into: [ :mx :entry | entry fileSize ifNil: [mx] ifNotNil: [:fs| mx max: fs]]) printString size.
	sizeWithCommasPad := (list inject: 0 into: [ :mx :entry | mx max: (entry fileSize ifNil: [0])]) printStringWithCommas size.
	^list collect: [ :e |
		self fileNameFormattedFrom: e
			namePad: namePad
			sizePad: sizePad
			sizeWithCommasPad: sizeWithCommasPad ].
]

{ #category : 'file list' }
FileList >> fileListIndex [
	"Answer the index of the currently selected file."

	^ listIndex
]

{ #category : 'file list' }
FileList >> fileListIndex: anInteger [
	"Select the file name having the given index, and display its contents."

	listIndex := anInteger.
	fileName := listIndex = 0
		"open the file selected"
		ifFalse: [
			(list at: anInteger) name ].

	brevityState := #needToGetBrief.
	self changed: #fileListIndex.
	self acceptedContentsChanged.
	self triggerEvent: #updateButtonRow
]

{ #category : 'file name list' }
FileList >> fileName [
	^ fileName
]

{ #category : 'as yet unclassified' }
FileList >> fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad [
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font := Preferences at: #standardListFont.
	spaceWidth := font widthOf: $ .
	nameStr := entry isDirectoryEntry
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd := namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr := nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr := (entry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			entry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).
	sizeDigits := entry fileSize printString size.
	sizeStr := entry fileSize printStringWithCommas.
	sizeDigitsAndCommas := sizeStr size.
	spacesToAdd := sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd := spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr := (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces := String new: 6 withAll: $ .
	"
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].
	"
	^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' .
]

{ #category : 'private' }
FileList >> folderString [
	^ ' [...]'
]

{ #category : 'private' }
FileList >> fullName [
	"Answer the full name for the currently selected file; answer nil if no file is selected."

	^ fileName ifNotNil: [ (directory // fileName) pathName ]
]

{ #category : 'file menu action' }
FileList >> get [
	"Get contents of file again, it may have changed. Do this by making the cancel string be the contents, and doing a cancel."

	brevityState == #briefHex
		ifTrue: [brevityState := #needToGetFullHex]
		ifFalse: [brevityState := #needToGetFull].
	self acceptedContentsChanged
]

{ #category : 'file menu action' }
FileList >> getHex [
	"Get contents of file again, and display in Hex. Do this by making the cancel string be the contents, and doing a cancel."

	brevityState := #needToGetBriefHex.
	self acceptedContentsChanged
]

{ #category : 'initialization' }
FileList >> initialize [
	listIndex := 0.
	showDirsInFileList := false.
	showHidden := false.
]

{ #category : 'private' }
FileList >> isFileSmalltalkCode [
	^fileName
		ifNil: [ false ]
		ifNotNil: [ fileName endsWith: '.st' ]
]

{ #category : 'file list menu' }
FileList >> itemsForAnyFile1 [
	"Answer a list of universal services that could apply to any file"
	
	| services |
	services := OrderedCollection new.
	(#(briefHex briefFile needToGetBriefHex needToGetBrief) includes: brevityState) ifTrue: [
		services add: self serviceGet ].
	services add: self serviceRenameFile. 
	services add: self serviceDeleteFile.
	^ services
]

{ #category : 'file list menu' }
FileList >> itemsForAnyFile2 [
	"Answer a list of universal services that could apply to any file"
	
	| services |
	services := OrderedCollection new.
	(#(fullHex briefHex needToGetFullHex needToGetBriefHex) includes: brevityState) ifFalse: [
		services add: self serviceGetHex ].
	services add: self serviceCopyName.
	services add: self serviceViewContentsInWorkspace.
	services add: self serviceViewContentsInTextEditor.
	^ services
]

{ #category : 'file list menu' }
FileList >> itemsForFileEntry: aFileEntry [
	"Answer a list of services appropriate for a file of the given name"
	^ self class itemsForFileEntry: aFileEntry
]

{ #category : 'file list menu' }
FileList >> itemsForNoFile [

	| services |
	services := OrderedCollection new.
	services add: self serviceSortByName.
	services add: self serviceSortBySize.
	services add: (self serviceSortByDate useLineAfter: true).
	services add: self serviceAddNewFile.
	services add: self serviceAddNewDirectory.
	^ services

		
]

{ #category : 'initialization' }
FileList >> labelString [

	^ (directory ifNil: [^'[]']) pathName "contractTo: 50"
]

{ #category : 'volume list and pattern' }
FileList >> listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject [
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
		
	^selected asArray sort: self sortBlock.
]

{ #category : 'volume list and pattern' }
FileList >> pattern [

	^ pattern ifNil: ['*']

]

{ #category : 'volume list and pattern' }
FileList >> pattern: textOrStringOrNil [

	textOrStringOrNil
		ifNil: [pattern := '*']
		ifNotNil: [pattern := textOrStringOrNil asPlainString].
	self updateFileList.
	^ true

]

{ #category : 'private' }
FileList >> postOpen [
	| pathRoot pathElements |
	directory ifNotNil: [
		pathRoot := topDirectories detect: [ :any | any isInPathTo: directory ].
		pathElements := pathRoot pathComponents size = 0
			ifTrue: [ {directory drive}, directory pathComponents ]
			ifFalse: [ (directory pathComponents
					copyFrom: (pathRoot pathComponents size)
					to: directory pathComponents size) ].
		self changed: #(openPath), pathElements ]
]

{ #category : 'private' }
FileList >> put: aText [
	"Private - put the supplied text onto the file"

	| type |
	brevityState == #fullFile ifTrue: [
		acceptedContentsCache := aText.
		directory // fileName writeStreamDo: [ :fileStream |
			fileStream ifNotNil: [
				fileStream nextPutAll: acceptedContentsCache asPlainString ]].
		directory invalidateChildren.
		self updateFileList.
		^ true  "accepted"].
	listIndex = 0 ifTrue: [
		self inform: 'No fileName is selected'.
		^ false  "failed"].
	type := 'These'.
	brevityState = #briefFile ifTrue: [type := 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type := 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type := 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type := 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"
]

{ #category : 'private' }
FileList >> readContentsBrief: brevityFlag [
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first part characters. Don't create a file if it doesn't already exist."
	| fileSize firstPart firstPartSize |
firstPartSize := 1e6.
directory // fileName readStreamDo: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= (1.2*firstPartSize) ]) ifTrue: [
		acceptedContentsCache := f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	firstPart := f next: 1e6.
].
	acceptedContentsCache :=
'File ''{1}'' is {2} characters long.
You may use the ''get'' command to read the entire file.

Here are the first {3} characters...
------------------------------------------
{4}
------------------------------------------
... end of the first {3} characters.' format: {fileName. fileSize. firstPartSize. firstPart}.
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache
]

{ #category : 'private' }
FileList >> readContentsHex: brevity [
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData firstPartSize |
	firstPartSize := 1e3.
	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		stream useBytes.
		((size := stream size)) > (10*firstPartSize) & brevity
			ifTrue: [ data := stream next: firstPartSize. brevityState := #briefHex ]
			ifFalse: [ data := stream next: size. brevityState := #fullHex ]].

	hexData := String streamContents: [ :s |
		0 to: data size-1 by: 16 do: [ :loc |
			loc printOn: s base: 16 length: 8 padded: true.
			s
				space;
				nextPut: $(.
			loc printOn: s base: 10 length: 10 padded: true.
			s
				nextPut: $);
				space;
				tab.
			loc+1 to: (loc+16 min: data size) do: [ :i | s nextPutAll: (Character codePoint: (data at: i)) hex; space ].
			s newLine ]].

	^ acceptedContentsCache := ((size > (10*firstPartSize)) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first {3} characters...
------------------------------------------
{4}
------------------------------------------
... end of the first {3} characters.' format: {fileName. size. firstPartSize. hexData}]
		ifFalse: [hexData])
]

{ #category : 'volume list and pattern' }
FileList >> renameDirectory [
	"Rename the currently selected directory"

	self request: 'New directory name?'
	initialAnswer: directory 	name
	verifying: [:newDirectoryName | newDirectoryName notEmpty and: [newDirectoryName ~= directory name]]
	do: [:newDirectoryName |
			[directory rename: newDirectoryName]
			on: Error
			do: [:anError | PopUpMenu inform: self renameErrorMsg].
			self changed: #topDirectoryWrappers
		].
]

{ #category : 'private' }
FileList >> renameErrorMsg [
	^'An error occurred while renaming the directory.
Perhaps a directory with that name already exists in this path?'
]

{ #category : 'file menu action' }
FileList >> renameFile [
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
			self triggerEvent: #updateButtonRow]
]

{ #category : 'private' }
FileList >> resort: newMode [
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

]

{ #category : 'file list' }
FileList >> selectedFileEntry [
	"Answer the selected file, in the form of a FileEntry. For the various stream-reading services."

	^ directory ifNotNil: [ :dir | dir // fileName ]
]

{ #category : 'own services' }
FileList >> serviceAddNewDirectory [
	"Answer a service entry characterizing the 'add new directory' command"

	^ (SimpleServiceEntry
		provider: self
		label: 'add new directory (N)'
		selector: #addNewDirectory
		description: 'adds a new, empty directory (folder)'
		icon: #listAddIcon)
			keyboardShortcutCharacter: $N
]

{ #category : 'own services' }
FileList >> serviceAddNewFile [
	"Answer a service entry characterizing the 'add new file' command"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'add new file (n)' 
		selector: #addNewFile 
		description: 'create a new,. empty file, and add it to the current directory.'
		icon: #newIcon)
			keyboardShortcutCharacter: $n
]

{ #category : 'own services' }
FileList >> serviceCopyName [

	^ SimpleServiceEntry 
		provider: self 
		label: 'copy name to clipboard' 
		selector: #copyName 
		description:'copy name to clipboard' 
		icon: #copyIcon
]

{ #category : 'own services' }
FileList >> serviceDeleteFile [

	^ (SimpleServiceEntry
		provider: self
		label: 'delete (x)'
		selector: #deleteFile
		description: 'delete the seleted item'
		icon: #deleteIcon)
			keyboardShortcutCharacter: $x
]

{ #category : 'own services' }
FileList >> serviceGet [
	"Answer a service for getting the entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'get entire file' 
		selector: #get
		description: 'if the file has only been partially read in, because it is very large, read the entire file in at this time.'
		icon: #textEditorIcon
]

{ #category : 'own services' }
FileList >> serviceGetHex [

	^ SimpleServiceEntry 
		provider: self 
		label: 'view as hex' 
		selector: #getHex
		description: 'view as hex'
		icon: #preferencesDesktopFontIcon
]

{ #category : 'own services' }
FileList >> serviceRenameFile [

	^ (SimpleServiceEntry
		provider: self
		label: 'rename (R)'
		selector: #renameFile
		description: 'rename file'
		icon: #saveAsIcon)
			keyboardShortcutCharacter: $R
]

{ #category : 'own services' }
FileList >> serviceSortByDate [
	"Answer a service for sorting by date"
	| buttonLabel |
	buttonLabel := sortMode = #date
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - date' ]
				ifFalse: [ '[v] - date' ]]
		ifFalse: [ 'date' ].
	^  (SimpleServiceEntry 
			provider: self 
			label: 'by date' 
			selector: #sortByDate 
			description: 'sort entries by date'
			icon: #sendReceiveIcon)
		extraSelector: #sortingByDate;
		buttonLabel: buttonLabel
]

{ #category : 'own services' }
FileList >> serviceSortByName [
	"Answer a service for soring by name"
	| buttonLabel |
	buttonLabel := sortMode = #name
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - name' ]
				ifFalse: [ '[v] - name' ]]
		ifFalse: [ 'name' ].
	^ (SimpleServiceEntry 
		provider: self 
		label: 'by name' 
		selector: #sortByName 
		description: 'sort entries by name'
		icon: #sendReceiveIcon)
			extraSelector: #sortingByName;
			buttonLabel: buttonLabel
]

{ #category : 'own services' }
FileList >> serviceSortBySize [
	"Answer a service for sorting by size"
	| buttonLabel |
	buttonLabel := sortMode = #size
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - size' ]
				ifFalse: [ '[v] - size' ]]
		ifFalse: [ 'size' ].
	^ (SimpleServiceEntry
		provider: self
		label: 'by size'
		selector: #sortBySize
		description: 'sort entries by size'
		icon: #sendReceiveIcon)
			extraSelector: #sortingBySize;
			buttonLabel: buttonLabel.
]

{ #category : 'own services' }
FileList >> serviceViewContentsInTextEditor [
	"Answer a service for viewing the contents of a file in a TextEditor"
	
	^ SimpleServiceEntry 
		provider: self 
		label: 'Text Editor with contents' 
		selector: #viewContentsInTextEditor
		description: 'open a new TextEditor whose contents are set to the contents of this file'
		icon: #textEditorIcon
]

{ #category : 'own services' }
FileList >> serviceViewContentsInWorkspace [
	"Answer a service for viewing the contents of a file in a workspace"
	
	^ SimpleServiceEntry 
		provider: self 
		label: 'workspace with contents' 
		selector: #viewContentsInWorkspace
		description: 'open a new Workspace whose contents are set to the contents of this file'
		icon: #terminalIcon
]

{ #category : 'private' }
FileList >> setSelectedDirectoryTo: aFileDirectoryWrapper [
	currentDirectorySelected := aFileDirectoryWrapper.
	self directory: aFileDirectoryWrapper withoutListWrapper.
	brevityState := #FileList.
	self changed: #fileList.
	self acceptedContentsChanged.
	self changed: #currentDirectorySelected
]

{ #category : 'accessing' }
FileList >> showHidden [
	^showHidden
]

{ #category : 'private' }
FileList >> sortBlock [
	"Answer block to decide what order to display the directory entries."

	^ [ :xx :yy | | x y |
		(xx isDirectoryEntry == yy isDirectoryEntry) 
			ifTrue: [  
				sortAscending
					ifTrue: [ x := xx. y := yy ]
					ifFalse: [ x := yy. y := xx ].
				"sort by user-specified criterion"
				sortMode = #name 
					ifTrue: [x name <= y name]
					ifFalse: [ sortMode = #date
						ifTrue: [ x modificationTime = y modificationTime
								ifTrue: [ x name <= y name ]
								ifFalse: [ x modificationTime <= y modificationTime ] ]
						ifFalse: [ "size"
							x fileSize = y fileSize 
								ifTrue: [ x name <= y name ]
								ifFalse: [ x fileSize <= y fileSize ] ] ] ]
			ifFalse: [
				"directories always precede files"
				xx isDirectoryEntry ] ]
]

{ #category : 'file menu action' }
FileList >> sortByDate [
	self resort: #date
]

{ #category : 'file menu action' }
FileList >> sortByName [
	self resort: #name
]

{ #category : 'file menu action' }
FileList >> sortBySize [
	self resort: #size
]

{ #category : 'private' }
FileList >> sortingByDate [
	^ sortMode == #date
]

{ #category : 'private' }
FileList >> sortingByName [
	^ sortMode == #name
]

{ #category : 'private' }
FileList >> sortingBySize [
	^ sortMode == #size
]

{ #category : 'stepping' }
FileList >> stepAt: millisecondSinceLast [

	"Do not auto-update the list if there are too many files. It could take too long."
	list size <= 1000 ifTrue: [
		self updateFileList ].
]

{ #category : 'user interface' }
FileList >> toggleShowHidden [
	showHidden := showHidden not.
	self changed: #topDirectoryWrappers.
	self updateFileList.
]

{ #category : 'initialization' }
FileList >> topDirectoryWrappers [

	topDirectories := (Preferences at: #initialFileListDirectories) 
		caseOf: {
			[ #cuis ]  	-> 	[ | topDirectoryToUse |
				topDirectoryToUse := directory. "must be inside the tree of some of the included"
				(topDirectoryToUse commonPathWith: DirectoryEntry projectBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry userBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry cuisBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry currentDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				{ topDirectoryToUse} ].
			[ #roots ] 	-> [ DirectoryEntry roots ].
			}.
	^topDirectories collect: [ :dir |
		FileDirectoryWrapper with: dir name: (dir name ifNil: [ '/' ]) model: self ]
]

{ #category : 'initialization' }
FileList >> universalButtonServices [
	"Answer a list of services underlying the universal buttons in their initial inception.  For the moment, only the sorting buttons are shown."

	^ {self serviceSortByName. self serviceSortByDate. self serviceSortBySize}
]

{ #category : 'initialization' }
FileList >> updateDirectory [
	"directory has been changed externally, by calling directory:.
	Now change the view to reflect the change."
	self changed: #currentDirectorySelected.
	self postOpen
]

{ #category : 'private' }
FileList >> updateFileList [
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns patternsThatReject patternsThatSelect newList |
	patterns := OrderedCollection new.
	(pattern findTokens: (String with: Character cr with: Character lf with: $;))
		do: [ :each |
			(each includes: $*) | (each includes: $?)
				ifTrue: [ patterns add: (each copyReplaceAll: '?' with: '#')]
				ifFalse: [
					each isEmpty
						ifTrue: [ patterns add: '*']
						ifFalse: [ patterns add: '*' , each , '*']]].
	"A pattern that starts with $/ is used to reject entries
	Rejecting patterns are applied after selecting patterns."
	patternsThatSelect := patterns reject: [ :any | any first == $/ ].
	patternsThatSelect isEmpty ifTrue: [ patternsThatSelect add: '*' ]. 
	patternsThatReject := patterns select: [ :any | any first == $/ ] thenCollect: [ :each | each copyFrom: 2 to: each size ].
	newList := self listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject.

	newList = list ifFalse: [
		"If we had a selection, ensure it is valid, or discard it."
		listIndex = 0 ifFalse: [
			(listIndex > newList size or: [(list at: listIndex) ~= (newList at: listIndex)]) ifTrue: [
				listIndex := newList indexOf: (list at: listIndex) ]].
		list := newList.
		listIndex > 0 ifFalse: [ "Had no selection or it was discarded"
			fileName := nil.
			acceptedContentsCache := ''.
			self acceptedContentsChanged.
			self changed: #fileListIndex ].
		self changed: #fileList.
		self triggerEvent: #updateButtonRow ].
]

{ #category : 'own services' }
FileList >> viewContentsInTextEditor [
	"View the contents of my selected file in a new text editor"

	| fileEntry fileContents textEditor|
	fileEntry := directory // fileName.
	fileContents := fileEntry textContents.
	
	textEditor := TextEditor open.
	textEditor model actualContents:fileContents.
	textEditor forceSaveContentsTo: fileEntry.
]

{ #category : 'own services' }
FileList >> viewContentsInWorkspace [
	"View the contents of my selected file in a new workspace"

	| fileEntry fileContents |
	fileEntry := directory // fileName.
	fileContents := fileEntry textContents.

	((Workspace new contents: fileContents) 
		openLabel: 'Workspace from ', fileName)
			setProperty: #lastFilenameUsedToSave
			toValue: fileEntry pathName
]

{ #category : 'stepping' }
FileList >> wantsSteps [
	^ true
]
