"
I represent a single file directory.
I implement various directory specific behaviors.

You can write data by #at:put: , and read the data by #at:.

---
mu 11/6/2006 20:21

--------------
Some examples:
	DirectoryEntry default
	DirectoryEntry root
	DirectoryEntry roots
	
See FileEntry
"
Class {
	#name : 'DirectoryEntry',
	#superclass : 'FileSystemEntry',
	#instVars : [
		'children',
		'lastChildrenSync'
	],
	#classVars : [
		'CuisBaseDirectory',
		'CurrentDirectory',
		'ImageDirectory',
		'ProjectBaseDirectory',
		'UserBaseDirectory',
		'VMDirectory'
	],
	#category : 'System-File System',
	#package : 'System',
	#tag : 'File System'
}

{ #category : 'default directories' }
DirectoryEntry class >> coreUpdatesDirectory [
	^ DirectoryEntry cuisBaseDirectory / 'CoreUpdates'
]

{ #category : 'base directories' }
DirectoryEntry class >> cuisBaseDirectory [
	"The main folder with the directory structure where the files provided with Cuis reside.
	Usually will not contain user files, that belong to the project using Cuis and not to Cuis itself.
	Could be made read-only.
		cuisBaseDirectory/CoreUpdates: 								Default folder to look for updates to Cuis
		cuisBaseDirectory/Packages: 								Folder for code packages included with Cuis
		cuisBaseDirectory/TrueTypeFonts: 								Default folder to look for TrueType Fonts
	DirectoryEntry cuisBaseDirectory
	See #currentDirectory. See #userBaseDirectory."

	| imageDirectory |
	CuisBaseDirectory notNil ifTrue: [
		^ CuisBaseDirectory ].

	imageDirectory := self withAbsolutePathName: Smalltalk imagePath.

	"If the directory containing the image has siblings named CoreUpdates or Packages,
	it means that the parent is the Cuis Base Directory. This is the case for the directory
	structure provided in the Cuis repo."
	imageDirectory parent ifNotNil: [ :parentDirectory |
		(parentDirectory / 'CoreUpdates') exists | (parentDirectory / 'Packages') exists ifTrue: [
			^ CuisBaseDirectory := parentDirectory ]].

	"Otherwise, hopefully the required folders shold be inside the image folder.
	But if not, at least fail while looking for them in a reasonable place."
	^ CuisBaseDirectory := imageDirectory.
]

{ #category : 'base directories' }
DirectoryEntry class >> currentDirectory [
	"The current directory.
	
	It is used, for unqualified names: `'stuff.txt' asFileEntry`, `'subfldr' asDirectoryEntry`. It is also used for relative paths if the full path is not specified: `'../sibling/data.txt'`.

	In Linux and macOS, it is the current directory in the OS shell that started us.

	Same in Windows if Cuis is started from a command line. But, for instance, if the image is dropped on the VM in a Windows explorer, and no OS current directory is available, it defaults to #userBaseDirectory.

	DirectoryEntry currentDirectory
	See #cuisBaseDirectory. See #userBaseDirectory.
	See #getCurrentWorkingDirectory"

	CurrentDirectory notNil ifTrue: [
		^ CurrentDirectory ].


	self workingDirectory ifNotNil: [ :wd |
		"Don't use / as the UserBaseDirectory, as most likely we don't have rights to write to it.
		This could happen if a proper #workingDirectory is not set by the host os, for instance because
		Cuis was started from drag and drop in an os file explorer tool."
		wd isRoot ifFalse: [
			CurrentDirectory := wd ]].
	
	"Don't use / as the CurrentDirectory, as most likely we don't have rights to write to it.
	This could happen if a proper #workingDirectory is not set by the host os, for instance because
	Cuis was started from drag and drop in an os file explorer tool."
	CurrentDirectory isNil ifTrue: [
		CurrentDirectory := DirectoryEntry smalltalkImageDirectory ].
	
	^CurrentDirectory
]

{ #category : 'user default directories' }
DirectoryEntry class >> fileOutsDirQuery: suggestedFileName writeStreamDo: writeBlock [

	self userDirectory: 'FileOuts' queryFileName: suggestedFileName writeStreamDo: writeBlock
]

{ #category : 'user default directories' }
DirectoryEntry class >> fileOutsDirectory [

	^ self userDirectory: 'FileOuts'
]

{ #category : 'user default directories' }
DirectoryEntry class >> logsDirectory [
	^ (DirectoryEntry userBaseDirectory / 'Logs') assureExistence
]

{ #category : 'user default directories' }
DirectoryEntry class >> newPackagesDirectory [
	^ (DirectoryEntry userBaseDirectory / 'NewPackages') assureExistence
]

{ #category : 'default directories' }
DirectoryEntry class >> packagesDirectory [
	^ DirectoryEntry cuisBaseDirectory / 'Packages'
]

{ #category : 'base directories' }
DirectoryEntry class >> projectBaseDirectory [
	"The main directory that contains the #cuisBaseDirectory, but also other directories with project files.
	See #cuisBaseDirectory.
	DirectoryEntry projectBaseDirectory
	"
	ProjectBaseDirectory notNil ifTrue: [
		^ ProjectBaseDirectory ].

	^ ProjectBaseDirectory := self cuisBaseDirectory parent.
]

{ #category : 'cached state access' }
DirectoryEntry class >> releaseClassCachedState [

	ImageDirectory := nil.
	VMDirectory := nil.
	CurrentDirectory := nil.
	CuisBaseDirectory := nil.
	ProjectBaseDirectory := nil.
	UserBaseDirectory := nil.
]

{ #category : 'system directories' }
DirectoryEntry class >> roots [
	"Appropriate for all systems, including those with several roots, each being a logical 'drive' (Like Windows),
	and for systems having a single root with file systems (i.e. 'drives') mounted anywhere in the tree (Unix, MacOS)
	"
	"
	DirectoryEntry roots
	"
	^ FileIOAccessor default drives
		ifEmpty: [
			"On Linux and macOS"
			{ '/' asFullDirectoryEntry } ]
]

{ #category : 'user default directories' }
DirectoryEntry class >> savedChangeSetsDirectory [
	^ (DirectoryEntry userBaseDirectory / 'ChangeSets') assureExistence
]

{ #category : 'initialization' }
DirectoryEntry class >> setUserBaseDirectory: aDirectoryEntry [
	UserBaseDirectory := aDirectoryEntry
]

{ #category : 'initialization' }
DirectoryEntry class >> setUserBaseDirectoryAsCuisBase [
	UserBaseDirectory := self cuisBaseDirectory
]

{ #category : 'system directories' }
DirectoryEntry class >> smalltalkImageDirectory [
	"Answer the directory on which this Smalltalk image was started (or last saved)

	DirectoryEntry smalltalkImageDirectory
	"

	ImageDirectory ifNil: [
		ImageDirectory := self withAbsolutePathName: Smalltalk imagePath ].
	^ ImageDirectory
]

{ #category : 'default directories' }
DirectoryEntry class >> trueTypeFontsDirectory [
	^ DirectoryEntry cuisBaseDirectory / 'TrueTypeFonts'
]

{ #category : 'base directories' }
DirectoryEntry class >> userBaseDirectory [
	"The top directory for files generated by the system on behalf of the user
		userBaseDirectory/SavedChangeSets 						Folder for saving new ChangeSets
		userBaseDirectory/Packages: 								Folder for new code packages
		userBaseDirectory/UserChanges: 							Folder to store user changes files
		userBaseDirectory/FileOuts: 								Folder for filed out code
		userBaseDirectory/Logs: 									Folder for error logs and other logs
	DirectoryEntry userBaseDirectory
	See #cuisBaseDirectory. See #currentDirectory."

	UserBaseDirectory notNil ifTrue: [
		^ UserBaseDirectory ].

	self workingDirectory ifNotNil: [ :wd |
		"Don't use / as the UserBaseDirectory, as most likely we don't have rights to write to it.
		This could happen if a proper #workingDirectory is not set by the host os, for instance because
		Cuis was started from drag and drop in an os file explorer tool."
		wd isRoot ifFalse: [
			UserBaseDirectory := wd ]].

	"If no OS current directory, use a sibling of #cuisBaseDirectory
	If OS current directory is same as #cuisBaseDirectory, take a sibling too"
	(UserBaseDirectory isNil or: [ UserBaseDirectory = self cuisBaseDirectory ]) ifTrue: [
		UserBaseDirectory := self cuisBaseDirectory parent / (self cuisBaseDirectory name, '-UserFiles').
		UserBaseDirectory assureExistence ].

	^ UserBaseDirectory
]

{ #category : 'user default directories' }
DirectoryEntry class >> userChangesDirectory [
	^ (DirectoryEntry userBaseDirectory / 'UserChanges') assureExistence
]

{ #category : 'user default directories' }
DirectoryEntry class >> userDirectory: userDirectoryDefaultName [
	| preferenceKey |
	preferenceKey := self userDirectoryKeyFor: userDirectoryDefaultName.
	^(Preferences includesKey: preferenceKey)
		ifTrue: [ DirectoryEntry withPathName: (Preferences at: preferenceKey) ]
		ifFalse: [ DirectoryEntry userBaseDirectory / userDirectoryDefaultName ].
]

{ #category : 'user default directories' }
DirectoryEntry class >> userDirectory: userDirectoryDefaultName fileName: aFileName writeStreamDo: writeBlock ifExists: appendOverwriteSymbol [
	"Don't query the user at all. Use the saved preference for the folder, if it exists.
	If file doesn't exist, just create it.
	If it exists, act on appendOverwriteSymbol
		#append: append new contents at the end
		#overwrite: just delete any previous contents of the file.
	See #userDirectory:queryFileName:writeStreamDo:
	"
	| file |
	file := (self userDirectory: userDirectoryDefaultName) // aFileName.
	file writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			writeBlock value: fileStream ]]	.
	
	appendOverwriteSymbol
		caseOf: {
			[#append] 		-> 	[ file appendStreamDo: [ :fileStream | writeBlock value: fileStream ]].
			[#overwrite] 	-> 	[ file forceWriteStreamDo: [ :fileStream | writeBlock value: fileStream ]] }
		otherwise: 				[ file writeStreamDo: [ :fileStream | fileStream ifNotNil: [ writeBlock value: fileStream ]]].
]

{ #category : 'user default directories' }
DirectoryEntry class >> userDirectory: userDirectoryDefaultName put: pathName [
	| preferenceKey |
	preferenceKey := self userDirectoryKeyFor: userDirectoryDefaultName.
	Preferences name: preferenceKey description: 'User Directory ', userDirectoryDefaultName
		category: #directories type: CharacterSequence value: pathName.
	Preferences saveToDisk: preferenceKey.
]

{ #category : 'user default directories' }
DirectoryEntry class >> userDirectory: userDirectoryDefaultName queryFileName: suggestedFileName writeStreamDo: writeBlock [
	"Query the user for a fully qualified file path name. 
	If the file already exists, ask for confirmation to overwrite.
	If the user modifies the suggested directory, keep it as a saved preference for next time.
	If user accepts, evaluate writeBlock on the file write stream.
	See #userDirectory:fileName:writeStreamDo:ifExists:
	"
	| initialDirectoryChoice file |
	initialDirectoryChoice := self userDirectory: userDirectoryDefaultName.
	file := initialDirectoryChoice // suggestedFileName.
	self
		request: 'Confirm or enter path and file name'
		initialAnswer: file pathName
		verifying: [ :userInput |
			(FileEntry withPathName: userInput) exists
				ifTrue: [ 
					(PopUpMenu confirm: 'File already exists. Do you want to overwrite it?' )
						ifTrue: [ true ]
						ifFalse: [ 'Please pick another file name' ] ]
				ifFalse: [ true "Validation passed" ]]
		do: [ :userInput |
			file := FileEntry withPathName: userInput.
			file parent = initialDirectoryChoice ifFalse: [
				self userDirectory: userDirectoryDefaultName put: file parent pathName ].
			file forceWriteStreamDo: [ :fileStream |
				fileStream ifNotNil: [
					writeBlock value: fileStream ]]	].
]

{ #category : 'user default directories' }
DirectoryEntry class >> userDirectoryKeyFor: userDirectoryDefaultName [

	^('user', userDirectoryDefaultName, 'Directory') asSymbol.
]

{ #category : 'system directories' }
DirectoryEntry class >> vmDirectory [
	"Answer the directory containing the VM that runs us.

	DirectoryEntry vmDirectory
	"

	VMDirectory ifNil: [
		VMDirectory := self withAbsolutePathName: Smalltalk vmPath ].
	^ VMDirectory
]

{ #category : 'system directories' }
DirectoryEntry class >> workingDirectory [
	"Answer the name of the VM process's working/current directory.
		DirectoryEntry workingDirectory
	"
	| string |
	string := FileIOAccessor default primWorkingDirectory.

	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	(String isAscii: string) ifFalse: [
		ByteArray adoptInstance: string.
		string := UnicodeString fromUtf8Bytes: string ].

	^ self withAbsolutePathName: string
]

{ #category : 'system directories' }
DirectoryEntry class >> workingDirectory: pathName [
	"Set the VM process's working working/current to be the argument."

	FileIOAccessor default primWorkingDirectory: pathName asUtf8Bytes.
]

{ #category : 'actions-path' }
DirectoryEntry >> / pathString [
	"Answer an instance of DirectoryEntry.
	If you want an instance of FileEntry, please call #//"
	^self concatPathComponentsAsDirectory: pathString asPathComponents
]

{ #category : 'actions-path' }
DirectoryEntry >> // pathString [
	"Answer an instance of FileEntry.
	If you want an instance of DirectoryEntry, please call #/"
	^self concatPathComponentsAsFile: pathString asPathComponents
]

{ #category : 'enumeration' }
DirectoryEntry >> allChildrenDo: aBlock [
	self childrenDo: [ :child | 
		aBlock value: child ].
	self allDirectoriesDo: [ :child |
		child allChildrenDo: aBlock]
]

{ #category : 'enumeration' }
DirectoryEntry >> allDirectoriesDo: aBlock [
	self directoriesDo: 
		[:child | 
		aBlock value: child.
		child allDirectoriesDo: aBlock]
]

{ #category : 'enumeration' }
DirectoryEntry >> allFilesDo: aBlock [
	self childrenDo: 
		[:child | 
		child isFileEntry ifTrue: [aBlock value: child] ifFalse: [child allFilesDo: aBlock]]
]

{ #category : 'enumeration' }
DirectoryEntry >> allFilesDo: aBlock matches: selectionBlock [
	self childrenDo: 
		[:child | 
		child isFileEntry
			ifTrue: [(selectionBlock value: child) ifTrue: [aBlock value: child]]
			ifFalse: [child allFilesDo: aBlock matches: selectionBlock]]
]

{ #category : 'enumeration' }
DirectoryEntry >> allRegularDirectoriesDo: aBlock [
	self regularDirectoriesDo: [ :child |
		aBlock value: child.
		child allRegularDirectoriesDo: aBlock]
]

{ #category : 'convenience' }
DirectoryEntry >> asDirectoryEntry [
	"For convenience."
	^self
]

{ #category : 'actions-directory' }
DirectoryEntry >> assureExistence [
	self isRoot ifTrue: [^self].
	self parent assureExistenceDirNamed: self name.
	^self
]

{ #category : 'private' }
DirectoryEntry >> assureExistenceDirNamed: localName [
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceDirNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName.
	self invalidateChildren
]

{ #category : 'dictionary-like' }
DirectoryEntry >> at: localFileName [

	^(self // localFileName) textContents
]

{ #category : 'dictionary-like' }
DirectoryEntry >> at: localFileName ifAbsent: block [

	^ [self at: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]
]

{ #category : 'dictionary-like' }
DirectoryEntry >> at: localFileName put: contents [

	(self // localFileName) forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: contents ].
	self invalidateChildren.
	^contents
]

{ #category : 'private' }
DirectoryEntry >> basicRecursiveDelete [
	self invalidateChildren.
	self directoriesDo: [:dir | dir basicRecursiveDelete].
	self filesDo: [:file | file delete].
	self delete
]

{ #category : 'dictionary-like' }
DirectoryEntry >> binaryAt: localFileName  [

	^ (self // localFileName) binaryContents
]

{ #category : 'dictionary-like' }
DirectoryEntry >> binaryAt: localFileName ifAbsent: block [

	^ [self binaryAt: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]
]

{ #category : 'dictionary-like' }
DirectoryEntry >> binaryAt: localFileName put: contents  [
	^self at: localFileName put: contents asByteArray
]

{ #category : 'accessing' }
DirectoryEntry >> children [
	(lastChildrenSync isNil or: [
		"This message may be sent many times. Don't call the primitives too often.
		Any time we invalidate children ourselves (by adding / removing / renaming entries) we send #invalidateChildren.
		Still, if directory contents are modified elsewhere (and especially outside Cuis), there will be a delay until we see those changes."
		(DateAndTime now - lastChildrenSync) totalSeconds > 2]) ifTrue: [
			children := nil ].
	children ifNil: [self initChildren].
	^children
]

{ #category : 'enumeration' }
DirectoryEntry >> childrenDo: aBlock [
	^self children do: aBlock
]

{ #category : 'actions-path' }
DirectoryEntry >> concatPathComponentsAsDirectory: components [
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry withPathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	^entry
]

{ #category : 'actions-path' }
DirectoryEntry >> concatPathComponentsAsFile: components [

	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components allButLast do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry withPathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	entryComponents := entryComponents copyWith: components last.
	entry := FileEntry withPathComponents: entryComponents drive: self drive.
	parentEntry ifNotNil: [
		entry setParent: parentEntry ].

	^entry
]

{ #category : 'actions-directory' }
DirectoryEntry >> copyTo: filename  [
	
	| toDir |
	filename asFileEntry exists ifTrue: [^self error: 'Cannot copy directory to file'].
	
	toDir := filename asDirectoryEntry assureExistence.

	self
		filesDo: [:file | file copyTo: (toDir / file name) pathName].
	
	self
		directoriesDo: [ :dir |
			dir copyTo:  dir pathName ]
]

{ #category : 'actions-directory' }
DirectoryEntry >> delete [
	self fileAccessor deleteDirectory: self pathName.
	self invalidateChildren.
	parent ifNotNil: [ parent invalidateChildren ].
]

{ #category : 'accessing' }
DirectoryEntry >> directories [
	^self children select: [:each | each isDirectoryEntry]
]

{ #category : 'enumeration' }
DirectoryEntry >> directoriesDo: aBlock [
	self childrenDo: [ :each |
		each isDirectoryEntry ifTrue: [
			aBlock value: each ]]
]

{ #category : 'enumeration' }
DirectoryEntry >> directoriesMatches: selectionBlock [
	^self directories select: selectionBlock
]

{ #category : 'enumeration' }
DirectoryEntry >> directoryMatching: pattern [
"
	DirectoryEntry smalltalkImageDirectory directoryMatching: 'C*Pack*'.
	DirectoryEntry smalltalkImageDirectory directoryMatching: 'xC*Pack*'.
"
	self directoriesDo: [ :directory |
		(pattern match: directory name)
			ifTrue: [ ^ directory ]].
	^ nil
]

{ #category : 'accessing' }
DirectoryEntry >> directoryNames [
	^self directories collect: [:each | each name]
]

{ #category : 'enumeration' }
DirectoryEntry >> directoryNamesMatching: pat [
	"
	DirectoryEntry currentDirectory directoryNamesMatching: '*'
	"

	^ self directoryNames select: [ :each | pat match: each ]
]

{ #category : 'accessing' }
DirectoryEntry >> driveName: driveString [
	"For instance, 'C:' or 'C:\'"

	name := driveString.
	drive := driveString.
	pathComponents := #().
	self invalidate
]

{ #category : 'enumeration' }
DirectoryEntry >> fileMatching: pattern [
"
	DirectoryEntry smalltalkImageDirectory fileMatching: '*.image'.
	DirectoryEntry smalltalkImageDirectory fileMatching: 'x*.image'.
"
	self filesDo: [ :file |
		(pattern match: file name)
			ifTrue: [ ^ file ]].
	^ nil
]

{ #category : 'accessing' }
DirectoryEntry >> fileNames [
	^self files collect: [:each | each name]
]

{ #category : 'enumeration' }
DirectoryEntry >> fileNamesMatching: pat [
	"
	DirectoryEntry currentDirectory fileNamesMatching: '*'
	"

	^ self fileNames select: [ :each | pat match: each ]
]

{ #category : 'accessing' }
DirectoryEntry >> files [
	^self children select: [:each | each isFileEntry]
]

{ #category : 'enumeration' }
DirectoryEntry >> filesDo: aBlock [
	self childrenDo: [ :each |
		each isFileEntry ifTrue: [
			aBlock value: each ]]
]

{ #category : 'enumeration' }
DirectoryEntry >> filesMatches: selectionBlock [
	^self files select: selectionBlock
]

{ #category : 'dictionary-like' }
DirectoryEntry >> includesKey: fileName [
	^self fileNames includes: fileName
]

{ #category : 'private' }
DirectoryEntry >> initChildren [

	self exists ifFalse: [ ^children := #()] .
	children := self fileAccessor entriesIn: self.
	lastChildrenSync := DateAndTime now.
	^children
]

{ #category : 'initialize-release' }
DirectoryEntry >> invalidateChildren [
	
	children := nil
]

{ #category : 'testing' }
DirectoryEntry >> isDirectoryEntry [
	^true
]

{ #category : 'accessing' }
DirectoryEntry >> isInPathTo: aFileSystemEntry [
	| theirPathComponents size |
	size := pathComponents size.
	theirPathComponents := aFileSystemEntry pathComponents.
	theirPathComponents size >= size ifFalse: [
		^false ].
	1 to: size do: [ :index |
		(pathComponents at: index) = (theirPathComponents at: index) ifFalse: [^ false]].
	^ true
]

{ #category : 'testing' }
DirectoryEntry >> isRegularDirectory [
	"hidden convention in Unix"
	name first == $. ifTrue: [ ^false ].
	"in MacOS, applications are actually directories, but are usually not treated as such"
	self extension = 'app' ifTrue: [ ^false ].
	"in MacOS, .bundle directories, are resource packages"
	self extension = 'bundle' ifTrue: [ ^false ].
	^true
]

{ #category : 'dictionary-like' }
DirectoryEntry >> keys [
	^self files
]

{ #category : 'enumeration' }
DirectoryEntry >> latestFileMatches: selectionBlock [
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) first
]

{ #category : 'services' }
DirectoryEntry >> nextNameFor: baseFileName coda: fileNameCoda extension: extension [
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version candidate |
	files := self fileNamesMatching: (baseFileName,'*.', extension).
	splits := files collect: [ :file | self fileAccessor splitNameVersionExtensionFor: file ].
	splits := splits asArray sort: [ :a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [ version := 1 ]
			ifFalse: [ version := (splits last at: 2) + 1 ].
	candidate := (baseFileName, fileNameCoda, '.', (String streamContents: [ :strm | version printOn: strm length: 3 zeroPadded: true ]), '.', extension) asFileName.
	^ candidate
]

{ #category : 'services' }
DirectoryEntry >> nextNameFor: baseFileName extension: extension [
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	^self nextNameFor: baseFileName coda: '' extension: extension
]

{ #category : 'enumeration' }
DirectoryEntry >> oldestFileMatches: selectionBlock [
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) last
]

{ #category : 'actions-directory' }
DirectoryEntry >> recursiveDelete [
	self exists
		ifTrue: [self basicRecursiveDelete]
]

{ #category : 'enumeration' }
DirectoryEntry >> regularDirectoriesDo: aBlock [
	self childrenDo: [ :each |
		each isDirectoryEntry ifTrue: [
			each isRegularDirectory ifTrue: [
				aBlock value: each ]]]
]

{ #category : 'dictionary-like' }
DirectoryEntry >> removeKey: localFileName  [
	self removeKey: localFileName ifAbsent: nil
]

{ #category : 'dictionary-like' }
DirectoryEntry >> removeKey: localFileName ifAbsent: failBlock [
	self fileAccessor deleteFile: (self // localFileName) pathName ifAbsent: [^failBlock value].
	self invalidateChildren.
]

{ #category : 'actions-directory' }
DirectoryEntry >> rename: newName [

	| fileEntry directoryEntry |
	fileEntry := self parent // newName.
	fileEntry exists
		ifTrue: [ Error signal: 'There already exists a file named: ', fileEntry printString ].
	directoryEntry := self parent / newName.
	directoryEntry exists
		ifTrue: [ Error signal: 'There already exists a directory named: ', fileEntry printString ].
	self fileAccessor renameDirectory: self pathName to: directoryEntry pathName.
	self name: newName.
	self parent 	invalidateChildren.
]

{ #category : 'testing' }
DirectoryEntry >> updateExists [
	
	| pathName |
	(self fileAccessor isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^ exists := self fileAccessor drives includes: self ].

	self isRoot ifTrue: [ ^ exists := true ].
	
	pathName := self pathName.
	pathName = self fileAccessor slash ifTrue: [ ^ exists := true ].

	exists := self fileAccessor basicDirectoryExists: pathName
]

{ #category : 'private' }
DirectoryEntry >> updateFrom: primitiveArray entryInParent: index [
	super updateFrom: primitiveArray entryInParent: index.
	children := nil.	"lazy initialization"
]
