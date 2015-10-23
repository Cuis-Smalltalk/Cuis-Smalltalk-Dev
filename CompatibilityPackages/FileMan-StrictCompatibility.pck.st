'From Cuis 4.2 of 25 July 2013 [latest update: #2555] on 23 October 2015 at 3:33:41.426292 pm'!
'Description Please enter a description for this package'!
!provides: 'FileMan-StrictCompatibility' 1 7!
!requires: 'Tests' 1 nil nil!

!String methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:38'!
fileContents: aString by: encoderName
	self asFileEntry textContents: aString by: encoderName.
	^aString! !

!String methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:38'!
fileContentsBy: encoderName
	^self asFileEntry textContentsBy: encoderName! !

!String methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 14:22'!
upToLastPathSeparator
	| index |
	self ifEmpty: [^self].
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MasashiUmezawa 7/8/2012 21:55'!
allChildrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	self childrenDo: 
		[:child | 
		(child isFile and: [selectionBlock value: child]) 
			ifTrue: [aBlock value: child]
			ifFalse: [(child isDirectory and: [dirSelectionBlock value: child]) ifTrue: [aBlock value: child]].
		child allChildrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock]! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/21/2006 10:21'!
asDirectoryEntry
	^self! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/28/2013 00:25'!
asFileEntry
	"For converting to FileEntry, use #ensureAsFile"
	^super asFileEntry! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MU 3/26/2014 12:38'!
assureExistenceFileNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self pathName) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceFileNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MasashiUmezawa 7/8/2012 21:45'!
childrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	self children do: [:each | 
		(each isFile and: [selectionBlock value: each]) 
			ifTrue: [aBlock value: each]
			ifFalse: [(each isDirectory and: [dirSelectionBlock value: each]) ifTrue: [aBlock value: each]]
	]! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:31'!
directory
	^self directories ifEmpty: [] ifNotEmpty: [:ds | ds first]! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/28/2013 00:23'!
ensureAsDirectory
	^self! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/12/2015 10:11'!
ensureAsFile
	^self as: FmFileEntry! !

!FmDirectoryEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 13:14'!
file
	^self files ifEmpty: [] ifNotEmpty: [:fs | fs first]! !

!FmDirectoryEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/5/2015 12:24'!
default 
	"Compatibility with other implementations of FileMan.
	For Cuis specific code, it is advisable to call #currentDirectory, #smalltalkImageDirectory or #vmDirectory
	FmDirectoryEntry default
	"
	^self currentDirectory! !

!FmDirectoryEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/14/2007 19:48'!
pathName: fullPathName

	| inst |
	inst := self new.
	inst pathName: fullPathName.
	^ inst! !

!FmDirectoryEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/3/2007 17:13'!
root 
	"FmDirectoryEntry root"
	^'/' asDirectoryEntry! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/13/2006 19:47'!
allChildrenDo: aBlock
	"do nothing"! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MasashiUmezawa 7/8/2012 21:37'!
allChildrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	"do nothing"! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/13/2006 19:50'!
allDirectoriesDo: aBlock
	"do nothing"! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/13/2006 19:51'!
allFilesDo: aBlock
	"do nothing"! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MasashiUmezawa 7/8/2012 17:18'!
allFilesDo: aBlock matches: selectionBlock
	"do nothing"! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:26'!
appendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamContents: that closes the file for you."

	^ self privateAppendStream! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:09'!
appendStreamContents: blockWithArg 
	| stream |
	stream := self appendStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/27/2007 15:30'!
as: aCustomFileEntryClass
	"Usable when adding new behaviors to the existing FileEntry"
	^aCustomFileEntryClass pathComponents: self pathComponents drive: self drive! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/12/2015 10:13'!
asDirectoryEntry
	^ self as: FmDirectoryEntry! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 5/4/2007 22:38'!
asDirectoryEntry: directoryEntryClass
	^self asDirectoryEntry as: directoryEntryClass! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/21/2006 10:20'!
asFileEntry
	^self! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 5/4/2007 22:38'!
asFileEntry: fileEntryClass
	^self asFileEntry as: fileEntryClass! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 7/30/2006 22:45'!
at: localFileName 
	^self asDirectoryEntry at: localFileName asString ifAbsent:[]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 7/27/2006 17:33'!
at: localFileName ifAbsent: block
	^self asDirectoryEntry at: localFileName asString ifAbsent: block! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 7/19/2006 16:59'!
at: localFileName put: contents
	^self asDirectoryEntry at: localFileName asString put: contents! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 11/30/2006 17:58'!
baseName
	^self nameVersionExtension first! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 17:49'!
basicPathComponents: aCollection
	pathComponents := aCollection! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/10/2006 18:47'!
binaryAt: localFileName 
	^self asDirectoryEntry binaryAt: localFileName asString ifAbsent:[]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/10/2006 18:47'!
binaryAt: localFileName ifAbsent: block
	^self asDirectoryEntry binaryAt: localFileName asString ifAbsent: block! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/1/2006 19:24'!
children
	^self asDirectoryEntry children! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/13/2006 19:57'!
childrenDo: aBlock
	^self asDirectoryEntry childrenDo: aBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MasashiUmezawa 7/8/2012 21:31'!
childrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	"do nothing"! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 18:19'!
creationTime
	creationTime ifNil: [self initValuesFromParent].
	^creationTime! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 14:03'!
creationTime: value
	creationTime := value! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/14/2007 19:30'!
currentDriveName
	^self fileAccessor currentDriveName! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/1/2006 19:21'!
directories
	^self asDirectoryEntry directories! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/13/2006 19:57'!
directoriesDo: aBlock
	^self asDirectoryEntry directoriesDo: aBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/9/2006 13:55'!
directoriesMatches: selectionBlock
	^self asDirectoryEntry directoriesMatches: selectionBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:31'!
directory
	^self asDirectoryEntry directory! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/3/2007 22:46'!
drive
	self onUnix ifTrue: [^ drive := nil].
	drive
		ifNil: [drive := self currentDriveName].
	^ drive! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 13:26'!
drive: aString
	drive := aString! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/2/2007 21:04'!
drives
	^self class drives! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/28/2013 00:23'!
ensureAsDirectory
	^self asDirectoryEntry! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/28/2013 00:22'!
ensureAsFile
	^self! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/12/2015 10:13'!
ensureParent
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent := FmDirectoryEntry
				pathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/19/2006 23:27'!
extension
	^self nameVersionExtension last! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 13:14'!
file
	^self asDirectoryEntry file! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/28/2007 00:01'!
fileAccessor
	"override"
	^FmFileIOAccessor default! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:36'!
fileContentsBy: encoderName
	^self asFileEntry textContentsBy: encoderName.
! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/1/2006 19:22'!
files
	^self asDirectoryEntry files! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/13/2006 19:57'!
filesDo: aBlock
	^self asDirectoryEntry filesDo: aBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/9/2006 13:54'!
filesMatches: selectionBlock
	^self asDirectoryEntry filesMatches: selectionBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 11/6/2006 20:38'!
includesKey: fileName
	^self asDirectoryEntry includesKey: fileName! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 18:18'!
initValuesFromParent
	| targets target |
	self ensureParent.
	self parent ifNil: [^self].

	targets := self isDirectory ifTrue: [self parent directories] ifFalse: [self parent files].
	target := targets detect: [:each | each = self] ifNone:[].

	self initValuesFrom: target.
	! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 21:39'!
isDriveSupported
	^self fileAccessor isDriveSupported! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 17:27'!
isFileEntry
	^true! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/15/2007 13:08'!
isRoot
	^self parent isNil! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 7/27/2006 17:35'!
keys
	^self asDirectoryEntry keys! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/9/2006 13:55'!
latestFileMatches: selectionBlock
	^self asDirectoryEntry latestFileMatches: selectionBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 18:19'!
modificationTime
	modificationTime ifNil: [self initValuesFromParent].
	^modificationTime! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 14:03'!
modificationTime: value
	modificationTime := value! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 13:35'!
name
	^name! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/18/2006 22:05'!
name: aString 
	name := aString.
	self pathComponents
		ifNotEmpty: [self pathComponents at: self pathComponents size put: name]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/14/2007 18:03'!
nameVersionExtension
	^self fileAccessor splitNameVersionExtensionFor: self name! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 10/25/2010 22:24'!
nameWithoutExtension
	| nm index |
	nm := self name.
	index := nm lastIndexOf: $..
	index = 0 ifTrue: [^nm].
	nm first = $. ifTrue: [^nm].
	
	^self name copyFrom: 1 to: index - 1! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 8/9/2006 13:55'!
oldestFileMatches: selectionBlock
	^self asDirectoryEntry oldestFileMatches: selectionBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 21:38'!
onMac
	^self fileAccessor onMac! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 21:38'!
onUnix
	^self fileAccessor onUnix! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 21:38'!
onWindows
	^self fileAccessor onWindows! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 17:41'!
parent
	parent ifNil: [parent := self ensureParent].
	^parent! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 21:43'!
parent: aFmDirectory
	parent := aFmDirectory.

	self pathName: aFmDirectory pathName, self slash, self name! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 1/24/2012 15:44'!
parents
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 13:16'!
pathComponents
	pathComponents ifNil: [pathComponents := OrderedCollection new].
	^pathComponents! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 10/26/2006 19:20'!
pathComponents: aCollection
	self pathComponents: aCollection detectDrive: true! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/3/2007 23:34'!
pathComponents: aCollection detectDrive: detectDrive
	| tokens firstToken  |
	aCollection isEmptyOrNil ifTrue: [^pathComponents := nil].
	tokens := aCollection.
	(detectDrive and: [(firstToken := aCollection first) isDriveName])
		ifTrue: [self drive: firstToken.
			self
				basicPathComponents: (tokens copyFrom: 2 to: tokens size)]
		ifFalse: [self basicPathComponents: tokens].

	pathComponents ifNotEmpty: [self name: pathComponents last]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:43'!
pathName

	^ String streamContents: [ :stream |
		self printPathOn: stream ]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 6/6/2015 21:42'!
pathName: aString 
	| path tokens guessedDriveName |
	path := aString asAbsolutePathName.
	tokens := path findTokens: self slash.
	tokens ifEmpty: [^ nil].
	self isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName isDriveName
				ifTrue: [
					self drive: guessedDriveName.
					tokens := tokens copyFrom: 2 to: tokens size.]].
	self pathComponents: tokens! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 2/28/2007 00:06'!
printOn: aStream 
	self printPathOn: aStream! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MU 3/26/2014 12:43'!
printPathOn: aStream 
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPutAll: self slash.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPutAll: self slash]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:53'!
readStream

	^ [ self fileAccessor privateReadOnlyFile: self ]
		on: FileDoesNotExistException
		do: [ :ex |
			self assureExistence.
			ex retry ]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:11'!
readStreamContents: blockWithArg 
	| stream conts |
	stream := self readStream.
	[conts := blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]].
	^ conts! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:53'!
readStreamIfError: errorHandlingBlock
	"Note: You need to eventually close the stream."

	^ [self fileAccessor privateReadOnlyFile: self ]
		on: FileDoesNotExistException
		do: [:ex | errorHandlingBlock value: ex]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 7/19/2006 17:00'!
removeKey: localFileName 
	^self asDirectoryEntry removeKey: localFileName asString! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 9/20/2006 18:10'!
removeKey: localFileName ifAbsent: failBlock
	^self asDirectoryEntry removeKey: localFileName asString ifAbsent: failBlock! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/12/2015 10:13'!
root
	^FmDirectoryEntry root! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/28/2015 11:54'!
setContentsOf: aStream to: aStringOrBytes

	aStringOrBytes isString
		ifFalse: [ aStream binary].
	aStream nextPutAll: aStringOrBytes.
	self refresh! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 6/6/2015 21:09'!
setParent: anFmFileEntry
	parent := anFmFileEntry! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 14:45'!
shortName
	^self name readStream upTo: $.! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 21:42'!
slash
	^self fileAccessor slash! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:53'!
tryReadStream
	"Raise FileDoesNotExistException if not found."
	"Note: You need to eventually close the stream."

	^ self fileAccessor privateReadOnlyFile: self! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:51'!
tryWriteStream
	"If the file already exists, raise FileExistsException, to give the chance to pick another name or overwrite it."
	"Note: You need to eventually close the stream.
	Fail if the directory to create the file in doesn't exist."

	self refresh.
	^self fileAccessor privateNewFile: self! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/19/2006 23:28'!
version
	^self nameVersionExtension second! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:57'!
writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamContents: that closes the file for you.
	Creates the directory if it doesn't exist."

	^self fileAccessor privateForceNewFile: self! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:51'!
writeStreamConfirming
	"Note: You need to eventually close the stream."

	self refresh.
	^self fileAccessor privateNewFile: self! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MU 3/26/2014 12:41'!
writeStreamContents: blockWithArg 
	| stream |
	stream := self writeStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:51'!
writeStreamIfError: errorHandlingBlock
	"Note: You need to eventually close the stream."

	self refresh.
	^[ self fileAccessor privateNewFile: self ]
		on: FileExistsException
		do: [ :ex | errorHandlingBlock value: ex ]! !

!FmFileEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/15/2007 10:24'!
drives
	"FmFileEntry drives"
	^FmFileIOAccessor default drives! !

!FmFileEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 13:14'!
example4
	"FmFileEntry example4"

	"Collect sm directory package names"
	| packageNames |
	packageNames := './sm/cache/packages' asDirectoryEntry directories collect: [:each | each directory file name].
	packageNames inspect! !

!FmFileEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 10/26/2006 19:21'!
pathComponents: comps drive: driveString
	| inst |
	inst := self new.
	inst pathComponents: comps detectDrive: false.
	inst drive: driveString.
	^inst

	! !

!FmFileEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 6/11/2006 22:21'!
pathName: aString
	| inst |
	inst := self new.
	inst pathName: aString.
	^inst

	! !

!FmFileEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/15/2015 13:05'!
releaseClassCachedState

	self allSubInstancesDo: [:each | each refresh]! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/5/2015 08:47'!
currentDriveName
	"
	self default currentDriveName
	"
	^Smalltalk imageName asDriveName! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/26/2007 12:01'!
directoryExists: fullPathName

	fullPathName = self slash ifTrue: [^true].

	^self basicDirectoryExists: fullPathName
	! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/14/2007 19:15'!
directoryExists: localName in: directoryFullPath

	| dirNames |
	dirNames := self directoryNamesIn: directoryFullPath.

	^self isCaseSensitive 
		ifTrue:[dirNames includes: localName]
		ifFalse:[dirNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/23/2015 15:24'!
entriesIn: directoryFullPath from: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Warning: If parentEntryOrNil is nil, answered instances are broken, 
	and essentially only useful for asking back the attributes we have just set.
	Not pretty.

		FmFileIOAccessor default entriesIn: '' from: nil
	
		FmFileIOAccessor default entriesIn: '.' from: '.' asFileEntry
	"
	| entries index done entryArray entry isDirectory |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self lookupEntryIn: directoryFullPath index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory
					ifTrue: [FmDirectoryEntry new]
					ifFalse: [FmFileEntry new].
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				entry fileSize: (entryArray at: 5).
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/16/2015 16:36'!
extensionDelimiter
	"Return the character used to delimit filename extensions on this platform. Most platforms use the period (.) character.
	This method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $. "

	^ $.! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 5/7/2007 15:56'!
fileExists: fullPathName

	| pathAndLocalName |
	pathAndLocalName := fullPathName pathAndLocalName.
	^self fileExists: pathAndLocalName last in: pathAndLocalName first! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/14/2007 22:46'!
fileExists: localName in: directoryFullPath

	| fileNames |
	fileNames := self fileNamesIn: directoryFullPath.
	^self isCaseSensitive 
		ifTrue:[fileNames includes: localName]
		ifFalse:[fileNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/27/2007 23:58'!
fileNamed: fullPathName
	"Open the file with the given name in this directory for writing."

	^ self concreteStreamClass fileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 12:09'!
forceNewFileNamed: fullPathName
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	^ self concreteStreamClass forceNewFileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/12/2015 20:44'!
lookupEntryIn: fullPathName index: index
	
	^self primLookupEntryIn: fullPathName index: index! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/27/2007 23:58'!
newFileNamed: fullPathName
	"Create a new file with the given name in this directory."

	^ self concreteStreamClass newFileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/14/2007 20:08'!
onMac
	^self pathNameDelimiter = $:! !

!FmFileIOAccessor methodsFor: '*FileMan-StrictCompatibility' stamp: 'mu 3/13/2007 12:05'!
readOnlyFileNamed: fullPathName
	"Open the existing file with the given name in this directory for read-only access."

	^ self concreteStreamClass readOnlyFileNamed: fullPathName
! !

!FmFileManTest methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/2/2015 17:09'!
testTryReadStream
	"FmFileManTest debug: #testTryReadStream" 
	| file1 readStream |
	file1 := self randomFileName asFileEntry.
	self deny: (file1 exists).

	self should: [readStream := file1 tryReadStream] raise: FileDoesNotExistException.
	self deny: (file1 exists).
	self assert: (readStream isNil).
	
	readStream := file1 readStreamIfError: [:ex | 'NotExist'].
	
	self assert: (readStream = 'NotExist').
	self deny: (file1 exists).

	file1 assureExistence.
	self assert: (file1 exists).
	
	readStream := file1 readStreamIfError: [:ex | 'NotExist'].
	self deny: (readStream = 'NotExist').
	
	self assert: (readStream contents isEmpty).
	readStream close.
	
	file1 delete.
	self deny: (file1 exists).! !

!FmFileManTest methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/2/2015 17:09'!
testTryWriteStream
	"FmFileManTest debug: #testTryWriteStream" 
	| file1 writeStream |
	file1 := self randomFileName asFileEntry assureExistence.
	self assert: (file1 exists).

	self should: [writeStream := file1 tryWriteStream] raise: FileExistsException.
	self assert: (file1 exists).
	self assert: (writeStream isNil).
	
	writeStream := file1 writeStreamIfError: [:ex | 'Exists'].
	
	self assert: (writeStream = 'Exists').
	self assert: (file1 exists).
	
	file1 delete.
	self deny: (file1 exists).
	
	writeStream := file1 writeStreamIfError: [:ex | 'Exists'].
	self deny: (writeStream = 'Exists').
	writeStream nextPutAll: 'Test'; close.
	
	self assert: (file1 textContents = 'Test').
	
	file1 delete.
	self deny: (file1 exists).! !
