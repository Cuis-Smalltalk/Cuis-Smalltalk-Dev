'From Cuis 4.2 of 25 July 2013 [latest update: #2547] on 21 October 2015 at 12:31:56.197976 pm'!

!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 12:02'!
// pathString
"todo esto deberia bajar a la subclase... aca arriba no tiene sentido!!!!!!!!!!!!"
	"Answer an instance of FmFileEntry.
	If you want an instance of FmDirectoryEntry, please call #/"
	^self concatPathComponentsAsFile: pathString asString asPathComponents! !

!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 11:49'!
concatPathComponentsAsDirectory: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := FmDirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	^entry! !

!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 11:58'!
concatPathComponentsAsFile: components

	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components allButLast do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := FmDirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	entryComponents := entryComponents copyWith: components last.
	entry := FmFileEntry pathComponents: entryComponents drive: self drive.
	parentEntry ifNotNil: [
		entry setParent: parentEntry ].

	^entry! !


!FmDirectoryEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:04'!
assureExistenceDirNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceDirNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName! !


!FileList methodsFor: 'file list' stamp: 'jmv 10/21/2015 12:03'!
selectedFileEntry
	"Answer the selected file, in the form of a FileEntry. For the various stream-reading services."

	^ directory ifNotNil: [ :dir | dir // fileName ]! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/21/2015 12:25'!
addNewDirectory

	self 
		addNew: 'Directory'
		byEvaluating: [ :newName | (directory / newName) assureExistence ].
	self updateDirectory.
	self changed: #initialDirectoryList! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/21/2015 12:02'!
addNewFile

	self 
		addNew: 'File'
		byEvaluating: [ :newName | (directory // newName) assureExistence ]
! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/21/2015 12:02'!
deleteFile
	"Delete the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	(self confirm: ('Really delete {1}?' format:{fileName})) ifFalse: [^ self].
	(directory // fileName) delete.
	self updateFileList.
	brevityState _ #FileList.
	self get! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 10/21/2015 12:03'!
renameFile
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	(response _ FillInTheBlankMorph request: 'NewFileName?'
 					initialAnswer: fileName)
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	newName = fileName ifTrue: [^ self].
	directory // fileName rename: newName.
	self updateFileList.
	listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = newName].
	listIndex > 0 ifTrue: [fileName _ newName].
	self changed: #fileListIndex.
	self changed: #updateButtonRow
! !

!FileList methodsFor: 'own services' stamp: 'jmv 10/21/2015 12:03'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"
	
	| aString aName |


	directory // fileName readStream: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		aString _ stream next: stream size.
		aName _ stream localName ].

	(Workspace new contents: aString) openLabel: 'Workspace from ', aName! !

!FileList methodsFor: 'private' stamp: 'jmv 10/21/2015 12:12'!
fullName
	"Answer the full name for the currently selected file; answer nil if no file is selected."

	^ fileName ifNotNil: [ (directory // fileName) pathName ]! !

!FileList methodsFor: 'private' stamp: 'jmv 10/21/2015 12:03'!
put: aText
	"Private - put the supplied text onto the file"

	| nameUsed type |
	brevityState == #fullFile ifTrue: [
		directory // fileName writeStream: [ :stream |
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

!FileList methodsFor: 'private' stamp: 'jmv 10/21/2015 12:03'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first5000 |

directory // fileName readStream: [ :f |
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

!FileList methodsFor: 'private' stamp: 'jmv 10/21/2015 12:03'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData |

	directory // fileName readStream: [ :stream |
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


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/21/2015 12:10'!
fileOutCategory: catName
	"FileOut the named category"

	FmDirectoryEntry smalltalkImageDirectory // (self name , '-' , catName , '.st') writeStream: [ :fileStream |
		fileStream timeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/21/2015 12:31'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	nameBody _ self name , '-' , (selector copyReplaceAll: ':' with: '').
	FmDirectoryEntry smalltalkImageDirectory // (nameBody asFileName, '.st') writeStream: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ]! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/21/2015 12:14'!
fileOutCategory: category
	"FileOut all the classes in the named system category."

	FmDirectoryEntry smalltalkImageDirectory // (category asFileName , '.st') writeStream: [ :fileStream |
		self fileOutCategory: category on: fileStream initializing: true ]! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 10/21/2015 12:23'!
installNewUpdates
 	
	| updatesFileDirectory |

	updatesFileDirectory _ FmDirectoryEntry smalltalkImageDirectory / 'CoreUpdates'.

	updatesFileDirectory exists
		ifFalse: [ self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ] 
		ifTrue: [ ChangeSet installNewUpdates: updatesFileDirectory ]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 10/21/2015 12:10'!
save

	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asDirectoryEntry exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName _
			(FmDirectoryEntry smalltalkImageDirectory // self packageFileName) pathName ].

	fullFileName asFileEntry forceWriteStream: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 10/21/2015 12:24'!
path: aString 
	
	| contentPacks directory |

	self flag: #todo. "Consider renaming this method. --cbr"

	directory _ FmDirectoryEntry smalltalkImageDirectory / aString.

	(self supportedFilesIn: directory) do: [ :i |
		| filename |
		filename _ directory pathName , '/', (i at: 1).
			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: i name 
			put: (self import: [ Form fromFileNamed: filename ]) "This may yet be a cross-cutting concern, and need to be refactored when other media types become present. --cbr"
	].

	contentPacks _ directory directoryNames collect: [ :i |	
		i ->  (ContentPack new path: (directory / i) pathName)
	].
	
	^ self union: (contentPacks as: Dictionary)! !

!ContentPack methodsFor: 'exporting' stamp: 'jmv 10/21/2015 12:24'!
exportInto: aDirectory

	aDirectory assureExistence.

	self associations do: [ :assoc |
		| klass thisDirectory fullPath | 
			klass _ assoc value class.
			thisDirectory _ aDirectory / assoc key.
			fullPath _  aDirectory // assoc key.

		self flag: #note. "Add support for new file export type mappings here. --cbr"
		klass = ContentPack
			ifTrue: [ assoc value exportInto: thisDirectory ].
		
		klass = ColorForm
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ].
			
		klass = Form
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ]
	]! !

!ContentPack methodsFor: 'private' stamp: 'jmv 10/21/2015 12:24'!
exportDirectory
	
	^ FmDirectoryEntry smalltalkImageDirectory / self class defaultContentDirectory / 'Exported'! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 10/21/2015 12:10'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	FmDirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStream: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class | 
					bar value: (classCount _ classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition _ f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old'.
	FmDirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: (FileStream oldFileNamed: oldChangesPathName).

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 10/21/2015 12:13'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount newVersionString oldChangesName newChangesName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	newSourcesName asFileEntry writeStream: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class |
					bar value: (classCount _ classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChangesName _ self currentChangesName.
	self closeSourceFiles.
	oldChangesName ifNotNil: [
		FmDirectoryEntry smalltalkImageDirectory // oldChangesName rename: oldChangesName, '.old' ].
	newChangesName _ self defaultChangesName.
	newChangesName asFileEntry writeStream: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/21/2015 12:14'!
fullNameForChangesNamed: aName
	"
	Smalltalk fullNameForChangesNamed: 'newChanges'
	"
	| newName |
	newName _ FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.changes'! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/21/2015 12:14'!
fullNameForImageNamed: aName
	"
	Smalltalk fullNameForImageNamed: 'newImage'
	"
	| newName |
	newName _ FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.image'! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/21/2015 12:14'!
logError: errMsg inContext: aContext to: localFileName
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	[
		FmDirectoryEntry smalltalkImageDirectory // localFileName forceWriteStream: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: Error do: [] "avoid recursive errors"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/21/2015 12:14'!
saveAsNewVersion
	"Save the image/changes using the next available version number."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	self okayToSave ifFalse: [ ^ self ].
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate ].
	newName _ fileName, '.image'.
	(FmDirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ FmDirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 10/21/2015 12:11'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName entry |
	pathName ifNotNil: [
		pathName asFileEntry exists ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory |
		entry _ directory // packageFileName.
		entry exists ifTrue: [
			pathName _ entry pathName.
			^ true ]].
	^ false! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 10/21/2015 12:24'!
inPackagesSubtreeOf: anFmDirectoryEntry do: aBlock

	| pckDir compatPckDir |

	"Look in the requested directory"
	aBlock value: anFmDirectoryEntry.

	"Look in the usual Packages subfolders"
	pckDir _ anFmDirectoryEntry / 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	compatPckDir _ anFmDirectoryEntry / 'CompatibilityPackages'.
	compatPckDir exists ifTrue: [
		aBlock value: compatPckDir ].

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	anFmDirectoryEntry children do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			aBlock value: entry.
			pckDir _ entry / 'Packages'.
			pckDir exists ifTrue: [
				aBlock value: pckDir ].
			compatPckDir _ entry / 'CompatibilityPackages'.
			compatPckDir exists ifTrue: [
				aBlock value: compatPckDir ]]]! !


!FileDirectory methodsFor: 'file operations' stamp: 'jmv 10/21/2015 12:25'!
recursiveDelete
	"Delete the this directory, recursing down its tree."
	self directoryNames
		do: [ :dn | (self / dn) recursiveDelete].
	self deleteLocalFiles.
	"should really be some exception handling for directory deletion, but no 
	support for it yet"
	self containingDirectory deleteDirectory: self localName! !


!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/21/2015 12:01'!
/ pathString
"todo esto deberia bajar a la subclase... aca arriba no tiene sentido!!!!!!!!!!!!"
	"Answer an instance of FmDirectoryEntry.
	If you want an instance of FmFileEntry, please call #//"
	^self concatPathComponentsAsDirectory: pathString asString asPathComponents! !

!FmFileEntry methodsFor: 'comparing' stamp: 'jmv 10/21/2015 11:30'!
= aFileEntry
	self class = aFileEntry class ifFalse: [^false].
	^self pathComponents = aFileEntry pathComponents and: [ self drive = aFileEntry drive ]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 10/21/2015 12:22'!
copyTo: filename 
	| targetEntry |
	
	targetEntry := filename asFileEntry.
	targetEntry isDirectory
		ifTrue: [ targetEntry := targetEntry // self name ].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 10/21/2015 12:06'!
rename: newName
	
	self fileAccessor rename: self pathName to: (self parent // newName) pathName.
	self name: newName! !


!FmFileEntry class methodsFor: 'examples' stamp: 'jmv 10/21/2015 12:07'!
example2
	"
	FmFileEntry example2
	"

	"FileMan's path representation is portable"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') at: 'test1' put: 'Hello2!!'.! !


!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 10/21/2015 12:04'!
assureExistence
	self isRoot ifTrue: [^self].
	self parent assureExistenceDirNamed: self name.
	^self! !

!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 10/21/2015 12:06'!
copyTo: filename 
	
	| toDir |
	filename asFileEntry exists ifTrue: [^self error: 'Cannot copy directory to file'].
	
	toDir := filename asDirectoryEntry assureExistence.

	self
		filesDo: [:file | file copyTo: (toDir / file name) pathName].
	
	self
		directoriesDo: [ :dir |
			dir copyTo:  dir pathName ]! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/21/2015 12:05'!
at: localFileName

	^(self // localFileName) textContents! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/21/2015 12:05'!
at: localFileName put: contents

	(self // localFileName) forceWriteStream: [ :stream |
		self setContentsOf: stream to: contents ].
	self refreshChildren.
	^contents! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/21/2015 12:05'!
binaryAt: localFileName 

	^ (self // localFileName) binaryContents! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/21/2015 12:06'!
removeKey: localFileName ifAbsent: failBlock
	self fileAccessor deleteFile: (self // localFileName) pathName ifAbsent: [^failBlock value].
	self refreshChildren.! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 10/21/2015 12:13'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	FmDirectoryEntry smalltalkImageDirectory // (class name , '.bytes') writeStream: [ :file |
		class selectorsDo: [ :sel | 
			file newLine; nextPutAll: sel; newLine.
			(self on: (class compiledMethodAt: sel)) printInstructionsOn: file ]].

	"
	InstructionPrinter printClass: Parser.
	"! !


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/21/2015 12:25'!
contents

	^((model directoryNamesFor: item) asArray sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: item / n name: n model: self ]! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/21/2015 12:13'!
fileOut

	FmDirectoryEntry smalltalkImageDirectory // (self name, '.st') writeStream: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/21/2015 12:13'!
fileOutCategory: categoryName

	FmDirectoryEntry smalltalkImageDirectory // (self name,'-',categoryName,'.st') writeStream: [ :f |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: f ]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/21/2015 12:31'!
fileOutMethod: selector

	FmDirectoryEntry smalltalkImageDirectory // (name,'-', selector asFileName, '.st') writeStream: [ :f |
		self fileOutMethods: (Array with: selector) on: f ]! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/21/2015 12:13'!
printSpaceAnalysis	
	"
	SpaceTally new printSpaceAnalysis
	"

	FmDirectoryEntry smalltalkImageDirectory // 'STspace.text' writeStream: [ :stream |
		self printSpaceAnalysis: 1 on: stream ]! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/21/2015 12:13'!
saveTo: aFileName
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	FmDirectoryEntry smalltalkImageDirectory // aFileName forceWriteStream: [ :stream |
		results do: [ :each |
				stream nextPutAll: each analyzedClassName asString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]! !

!methodRemoval: FmDirectoryEntry #assureExistenceFileNamed:!
FmDirectoryEntry removeSelector: #assureExistenceFileNamed:!
!methodRemoval: FmDirectoryEntry #directoryNamed:!
FmDirectoryEntry removeSelector: #directoryNamed:!
!methodRemoval: FmDirectoryEntry #ensureAsFile!
FmDirectoryEntry removeSelector: #ensureAsFile!

!FmDirectoryEntry reorganize!
('actions-path')
('enumeration' allChildrenDo: allDirectoriesDo: allFilesDo: allFilesDo:matches: childrenDo: childrenDo:filesMatches:directoryMatches: directoriesDo: directoriesMatches: filesDo: filesMatches: latestFileMatches: oldestFileMatches:)
('actions-directory' assureExistence copyTo: delete recursiveDelete rename:)
('private' assureExistenceDirNamed: basicRecursiveDelete initChildren)
('dictionary-like' at: at:ifAbsent: at:put: binaryAt: binaryAt:ifAbsent: binaryAt:put: includesKey: keys removeKey: removeKey:ifAbsent:)
('accessing' children directories directoryNames fileNames files)
('testing' exists isDirectory)
('initialize-release' refresh refreshChildren)
('cuis specific' fileNamesMatching: nextNameFor:coda:extension: nextNameFor:extension:)
!

!methodRemoval: FmFileEntry #,!
FmFileEntry removeSelector: #,!
!methodRemoval: FmFileEntry #concat:!
FmFileEntry removeSelector: #concat:!
!methodRemoval: FmFileEntry #concatPathComponents:!
FmFileEntry removeSelector: #concatPathComponents:!
!methodRemoval: FmFileEntry #concatPathComponents:defaultToFile:!
FmFileEntry removeSelector: #concatPathComponents:defaultToFile:!
!methodRemoval: FmFileEntry #ensureAsFile!
FmFileEntry removeSelector: #ensureAsFile!

!FmFileEntry reorganize!
('actions-path' / // concatPathComponentsAsDirectory: concatPathComponentsAsFile:)
('actions-rio' < <<)
('comparing' = hash)
('enumeration' allChildrenDo: allDirectoriesDo: allFilesDo: allFilesDo:matches: assureExistence childrenDo: childrenDo:filesMatches:directoryMatches: directoriesDo: directoriesMatches: filesDo: filesMatches: latestFileMatches: oldestFileMatches:)
('actions-file' appendContents: binaryContents binaryContents: copyTo: delete fileContents fileContents: rename: textContents textContents:)
('accessing-stream' appendStream: forceWriteStream: readStream: writeStream:)
('dictionary-like' at: at:ifAbsent: at:put: binaryAt: binaryAt:ifAbsent: includesKey: keys removeKey: removeKey:ifAbsent:)
('accessing-file name' baseName extension nameVersionExtension nameWithoutExtension version)
('accessing' basicPathComponents: children creationTime creationTime: directories drive drive: fileSize fileSize: files modificationTime modificationTime: name name: options options: parent parent: parents pathComponents pathName)
('private' ensureParent initValuesFrom: initValuesFromParent privateAppendStream privateForceWriteStream privateReadStream privateWriteStream setContentsOf:to: setParent:)
('factory' fileAccessor)
('testing' exists isDirectory isDriveSupported isFile isRoot onMac onUnix onWindows)
('initialize-release' pathComponents: pathComponents:detectDrive: pathName: refresh)
('actions-pipe' pipe:to: pipeRepeat:to: pipeRepeat:while:to:)
('printing' printOn: printPathOn:)
('cuis extensions' form)
!

