'From Cuis 4.2 of 25 July 2013 [latest update: #2539] on 20 October 2015 at 12:53:48.487111 pm'!

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 10/20/2015 12:26'!
fileReaderServicesForFile: filename suffix: suffix
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(#('st' 'cs' 'cs.st') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowsePackage } ].

	^#()! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 10/20/2015 12:26'!
fileReaderServicesForFile: filename suffix: suffix
	"FileList buttons that open a ChangeListWindow on contents"

	(#('st' 'cs' 'cs.st') includes: suffix)
		ifTrue: [ ^ {self serviceContents}  ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].

	suffix = 'changes'
		ifTrue: [ ^ {self serviceRecentChanges} ].

	^#()! !


!FileList methodsFor: 'file list' stamp: 'jmv 10/20/2015 12:48'!
selectedFileEntry
	"Answer the selected file, in the form of a FileEntry. For the various stream-reading services."

	^ directory ifNotNil: [ :dir | dir / fileName ]! !

!FileList methodsFor: 'file list menu' stamp: 'jmv 10/20/2015 12:26'!
itemsForFile: filename
	"Answer a list of services appropriate for a file of the given name"
	^ self class itemsForFile: filename! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/20/2015 12:44'!
addNewDirectory

	self 
		addNew: 'Directory'
		byEvaluating: [ :newName | (directory / newName) asDirectoryEntry assureExistence ].
	self updateDirectory.
	self changed: #initialDirectoryList! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/20/2015 12:34'!
addNewFile

	self 
		addNew: 'File'
		byEvaluating: [ :newName | (directory / newName) assureExistence ]
! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/20/2015 12:35'!
deleteFile
	"Delete the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	(self confirm: ('Really delete {1}?' format:{fileName})) ifFalse: [^ self].
	(directory / fileName) delete.
	self updateFileList.
	brevityState _ #FileList.
	self get! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/20/2015 12:48'!
renameFile
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	(response _ FillInTheBlankMorph request: 'NewFileName?'
 					initialAnswer: fileName)
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	newName = fileName ifTrue: [^ self].
	directory / fileName rename: newName.
	self updateFileList.
	listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = newName].
	listIndex > 0 ifTrue: [fileName _ newName].
	self changed: #fileListIndex.
	self changed: #updateButtonRow
! !

!FileList methodsFor: 'initialization' stamp: 'jmv 10/20/2015 12:14'!
directory: anFmDirectoryEntry
	"Set the path of the volume to be displayed."

	directory _ anFmDirectoryEntry.

	sortMode ifNil: [
		sortMode _ #date.
		sortAscending _ false ].
	self changed: #relabel.
	self pattern: pattern! !

!FileList methodsFor: 'initialization' stamp: 'jmv 10/20/2015 12:27'!
dynamicButtonServices
	"Answer services for buttons that may come and go in the button pane, depending on selection"

	^ fileName isEmptyOrNil
		ifTrue:
			[#()]
		ifFalse:
			[ | toReject |
				toReject _ self buttonSelectorsToSuppress.
				(self itemsForFile: self fileName) reject:
					[:svc | toReject includes: svc selector]]! !

!FileList methodsFor: 'initialization' stamp: 'jmv 10/20/2015 12:14'!
initialDirectoryList

	| dirList |
	dirList _ FmDirectoryEntry roots collect: [ :each |
		FileDirectoryWrapper with: each name: each name model: self].
	dirList isEmpty ifTrue: [
		dirList _ Array with: (FileDirectoryWrapper 
			with: directory
			name: directory localName 
			model: self)].
	^dirList! !

!FileList methodsFor: 'initialization' stamp: 'jmv 10/20/2015 12:15'!
labelString

	^ (directory ifNil: [^'[]']) pathName "contractTo: 50"! !

!FileList methodsFor: 'own services' stamp: 'jmv 10/20/2015 12:50'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"
	
	| aString aName |


	directory / fileName readStream: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		aString _ stream next: stream size.
		aName _ stream localName ].

	(Workspace new contents: aString) openLabel: 'Workspace from ', aName! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 10/20/2015 12:43'!
deleteDirectory
	"Remove the currently selected directory"

	| newSelection |
	directory children isEmpty ifFalse: [ ^self inform:'Directory must be empty' ].
	(self confirm: 'Really delete ' , directory name , '?') ifFalse: [ ^ self ].
	newSelection _ directory parent.
	directory delete.
	self directory: newSelection.
	self updateFileList.
	self updateDirectory.
	self changed: #initialDirectoryList! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 10/20/2015 12:15'!
directory

	^ directory! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 10/20/2015 12:14'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	selected _ Set new.
	patternsThatSelect do: [ :pat |
		directory childrenDo: [ :entry |
			(entry isDirectory
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ self doesPattern: pat allow: entry])
					ifTrue: [ selected add: entry ]]].
	newList _ selected copy.
	patternsThatReject do: [ :pat |
		selected do: [ :entry |
			(entry isDirectory not and: [ pat match: entry name]) ifTrue: [
				newList remove: entry ]]].
		
	newList _ newList asArray sort: self sortBlock.
	font _ Preferences standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList _ newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !

!FileList methodsFor: 'private' stamp: 'jmv 10/20/2015 12:16'!
fullName
	"Answer the full name for the currently selected file; answer nil if no file is selected."

	^ fileName ifNotNil: [ (directory / fileName) pathName ]! !

!FileList methodsFor: 'private' stamp: 'jmv 10/20/2015 12:24'!
isFileSmalltalkCode
	^fileName
		ifNil: [ false ]
		ifNotNil: [ fileName endsWith: '.st' ]! !

!FileList methodsFor: 'private' stamp: 'jmv 10/20/2015 12:15'!
postOpen

	directory ifNotNil: [
		self changed: #(openPath), {directory drive}, directory pathComponents ]! !

!FileList methodsFor: 'private' stamp: 'jmv 10/20/2015 12:21'!
put: aText
	"Private - put the supplied text onto the file"

	| nameUsed type |
	brevityState == #fullFile ifTrue: [
		directory / fileName writeStream: [ :stream |
			nameUsed _ stream name.
			stream nextPutAll: aText asString ].
		fileName = nameUsed
			ifTrue: [ acceptedContentsCache _ aText asString]
			ifFalse: [self updateFileList].		"user chose a different name (instead of overwriting)"
		^ true  "accepted"].

	listIndex = 0 ifTrue: [
		self inform: 'No fileName is selected'.
		^ false  "failed"].
	type _ 'These'.
	brevityState = #briefFile ifTrue: [type _ 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type _ 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type _ 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type _ 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"
! !

!FileList methodsFor: 'private' stamp: 'jmv 10/20/2015 12:16'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first5000 |

directory / fileName readStream: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= 2000000]) ifTrue: [
		acceptedContentsCache _ self withUnderscoreGlyphsIfNotSmalltalkCode: f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	first5000 := f next: 5000.
].
	acceptedContentsCache _ self withUnderscoreGlyphsIfNotSmalltalkCode:
('File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. fileSize. first5000}).
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'jmv 10/20/2015 12:47'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData |

	directory / fileName readStream: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		((size _ stream size)) > 2000000 & brevity
			ifTrue: [ data _ stream next: 10000. brevityState := #briefHex ]
			ifFalse: [ data _ stream next: size. brevityState := #fullHex ]].

	hexData _ String streamContents: [ :s |
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
			loc+1 to: (loc+16 min: data size) do: [ :i | s nextPutAll: (data at: i) hex; space ].
			s newLine ]].

	^ acceptedContentsCache _ ((size > 2000000) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. size. hexData}]
		ifFalse: [hexData])! !


!FileList class methodsFor: 'file reader registration' stamp: 'jmv 10/20/2015 12:26'!
itemsForFile: filename
	"Answer a list of services appropriate for a file of the given name"

	| services suffix |
	suffix _ (FmFileIOAccessor default extensionFor: filename) asLowercase.
	services _ OrderedCollection new.
	FileReaderRegistry do: [ :reader |
		reader ifNotNil: [
			services addAll: (reader fileReaderServicesForFile: filename suffix: suffix)]].
	^ services! !


!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 10/20/2015 12:26'!
fileReaderServicesForFile: filename suffix: suffix
	"These would better be done by ChangeSorter!!"

	(#('cs' 'cs.st') includes: suffix) ifTrue: [
		^{self serviceInstall} ].

	(#('st') includes: suffix) ifTrue: [
		^{self serviceFileIn} ].

	^#()! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 10/20/2015 12:26'!
fileReaderServicesForFile: filename suffix: suffix
	"FileList buttons that are serviced by us."

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ { self serviceInstallPackage } ].
	^#()! !


!FmDirectoryEntry methodsFor: 'accessing' stamp: 'jmv 10/20/2015 12:23'!
children
"Es que puede cambiar en cualquier momento!!!!!!!!!!!!"
children _ nil.
	children ifNil: [self initChildren].
	^children! !


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/19/2015 16:56'!
contents

	^((model directoryNamesFor: item) asArray sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: (item / n) asDirectoryEntry name: n model: self ]! !

!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/19/2015 16:36'!
hasContents
	"Return whether this directory has subfolders. The value is cached to 
	avoid a performance penalty.	Also for performance reasons, the code 
	below will just assume that the directory does indeed have contents in a 
	few of cases:  
	1. If the item is not a FileDirectory (thus avoiding the cost 
	of refreshing directories that are not local) 
	2. If it's the root directory of a given volume 
	3. If there is an error computing the FileDirectory's contents
	"
	hasContents
		ifNil: [hasContents := true. "default"
			["Best test I could think of for determining if this is a local directory "
			((item is: #FileDirectory)
					and: ["test to see that it's not the root directory"
						"there has to be a better way of doing this test -tpr"
						item pathComponents size > 1])
				ifTrue: [hasContents := self contents notEmpty]]
				on: Error
				do: [hasContents := true]].
	^ hasContents! !

!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/19/2015 16:40'!
setItem: anObject name: aString model: aModel

	item _ anObject.
	model _ aModel.
	itemName _ aString.
aString isNil ifTrue: [ self halt ].
	hasContents _ nil.
! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 10/20/2015 12:26'!
fileSelectedMenu

	| itemsPart1 itemsPart2 itemsPart3 itemsPart4 n1 n2 n3 services aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	itemsPart1 _ model itemsForAnyFile1.
	itemsPart2 _ model itemsForFile: model fileName.
	itemsPart3 _ model itemsForAnyFile2.
	itemsPart4 _ model itemsForNoFile.
	n1 _ itemsPart1 size.
	n2 _ n1 + itemsPart2 size.
	n3 _ n2 + itemsPart3 size.
	services _ itemsPart1, itemsPart2, itemsPart3, itemsPart4.
	services do: [ :svc | svc addDependent: model ].
	^ aMenu 
		addServices: services 
		for: model
		extraLines:{ n1 . n2 . n3 }
! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'jmv 10/19/2015 16:32'!
openFileList
	"
	FileListWindow openFileList
	"
	FileListWindow open: (FileList new directory: FmDirectoryEntry currentDirectory) label: nil! !


!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 10/19/2015 16:36'!
menuLinesArray: aDirectory
"Answer a menu lines object corresponding to aDirectory"

	| typeCount nameCnt dirDepth|
	typeCount _ canTypeFileName 
		ifTrue: [1] 
		ifFalse: [0].
	nameCnt _ aDirectory directoryNames size.
	dirDepth _ aDirectory pathComponents size.
	^Array streamContents: [:s |
		canTypeFileName ifTrue: [s nextPut: 1].
		s nextPut: dirDepth + typeCount + 1.
		s nextPut: dirDepth + nameCnt + typeCount + 1]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 10/19/2015 16:36'!
menuSelectionsArray: aDirectory
"Answer a menu selections object corresponding to aDirectory.  The object is an array corresponding to each item, each element itself constituting a two-element array, the first element of which contains a selector to operate on and the second element of which contains the parameters for that selector."

	|dirSize|
	dirSize _ aDirectory pathComponents size.
	^Array streamContents: [:s |
		canTypeFileName ifTrue:
			[s nextPut: (StandardFileMenuResult
				directory: aDirectory
				name: nil)].
		s nextPut: (StandardFileMenuResult
			directory: (FileDirectory root)
			name: '').
		aDirectory pathComponents doWithIndex: 
			[:d :i | s nextPut: (StandardFileMenuResult
					directory: (self 
						advance: dirSize - i
						containingDirectoriesFrom: aDirectory)
					name: '')].
		aDirectory directoryNames do: 
			[:dn |  s nextPut: (StandardFileMenuResult
						directory: (FileDirectory on: (aDirectory fullNameFor: dn))
						name: '')].
		aDirectory fileNames do: 
			[:fn | (self patternMatches: fn) ifTrue: [
					s nextPut: (StandardFileMenuResult
						directory: aDirectory
						name: fn)]]]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 10/19/2015 16:36'!
pathPartsString: aDirectory
"Answer a string concatenating the path parts strings in aDirectory, each string followed by a newLine."

	^String streamContents:
		[:s | 
			s nextPutAll: '[]'; newLine.
			aDirectory pathComponents asArray doWithIndex: 
				[:part :i |
					s next: i put: $ .
					s nextPutAll: part withBlanksTrimmed; newLine]]! !

!methodRemoval: FileList #directory:directoryEntry:!
FileList removeSelector: #directory:directoryEntry:!
