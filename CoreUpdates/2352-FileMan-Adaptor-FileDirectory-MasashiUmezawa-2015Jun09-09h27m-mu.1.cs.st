'From Cuis 4.2 of 25 July 2013 [latest update: #2350] on 9 June 2015 at 9:28:25.394503 am'!
!classDefinition: #FmFileDirectoryFileIOAccessorCuis category: #'FileMan-Adaptor-FileDirectory'!
FmFileIOAccessor subclass: #FmFileDirectoryFileIOAccessorCuis
	instanceVariableNames: 'defaultDirectoryPath'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Adaptor-FileDirectory'!

!FileDirectory methodsFor: '*FileMan-Adaptor-FileDirectory' stamp: 'mu 5/16/2007 21:01'!
asDirectoryEntry
	^self fullName asDirectoryEntry! !


!FmDirectoryEntry methodsFor: '*FileMan-Adaptor-FileDirectory' stamp: 'mu 3/14/2007 19:06'!
asFileDirectory
	"return lagacy FileDirectory"

	^FileDirectory on: self pathName! !


!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 4/6/2008 14:10'!
asAbsolutePathName: aString
	"Not complete, but in most cases it is OK"

	| tokens curDir childPath |

	aString isRelativePathName ifFalse: [^aString].

	aString = '.' ifTrue: [^self defaultDirectoryPath].
	aString = '..' ifTrue: [^self containingDirectoryPathOf: self defaultDirectoryPath].

	tokens := aString asPathTokens.

	curDir := self defaultDirectoryPath.
	tokens reverseDo: [:each |
		each = '..' ifTrue: [curDir := self containingDirectoryPathOf: curDir]
	].

	tokens removeAllSuchThat: [:each | #('.' '..') includes: each ].
	childPath := WriteStream on: String new.
	tokens do: [:each | childPath nextPutAll: each]
					separatedBy: [childPath nextPutAll: self slash].
		
	^curDir, self slash, childPath contents! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'MU 3/26/2014 12:34'!
copy: fromFileFullName to: toFileFullName 
	| readStr writeStr |
	[readStr := (self readOnlyFileNamed: fromFileFullName) binary.
	writeStr := (self forceNewFileNamed: toFileFullName) binary.
	self copyFile: readStr toFile: writeStr]
		ensure: [readStr
				ifNotNil: [:r | r close].
			writeStr
				ifNotNil: [:w | w close]]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'MU 3/26/2014 12:36'!
createDirectory: fullPathName
	^self primCreateDirectory: fullPathName! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'MU 3/26/2014 12:36'!
deleteDirectory: fullPathName
	^self primDeleteDirectory: fullPathName! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 3/14/2007 23:12'!
deleteFile: fullPathName
	^self deleteFile: fullPathName ifAbsent: []! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'MU 3/26/2014 12:36'!
deleteFile: fullPathName ifAbsent: failBlock 
	^(self
			try: [self primDeleteFileNamed: fullPathName]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 3/26/2007 12:01'!
directoryExists: fullPathName

	fullPathName = self slash ifTrue: [^true].

	^self basicDirectoryExists: fullPathName
	! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 3/14/2007 19:15'!
directoryExists: localName in: directoryFullPath

	| dirNames |
	dirNames := self directoryNamesIn: directoryFullPath.

	^self isCaseSensitive 
		ifTrue:[dirNames includes: localName]
		ifFalse:[dirNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 5/7/2007 15:56'!
fileExists: fullPathName

	| pathAndLocalName |
	pathAndLocalName := fullPathName pathAndLocalName.
	^self fileExists: pathAndLocalName last in: pathAndLocalName first! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 3/14/2007 22:46'!
fileExists: localName in: directoryFullPath

	| fileNames |
	fileNames := self fileNamesIn: directoryFullPath.
	^self isCaseSensitive 
		ifTrue:[fileNames includes: localName]
		ifFalse:[fileNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 3/14/2007 22:44'!
fileOrDirectoryExists: localName in: directoryFullPath

	| entryNames |
	entryNames := self entryNamesIn: directoryFullPath.

	^self isCaseSensitive 
		ifTrue:[entryNames includes: localName]
		ifFalse:[entryNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'mu 3/14/2007 20:09'!
pathNameDelimiter
	^self primPathNameDelimiter! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'MU 3/26/2014 12:36'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	(self fileExists: oldFileFullName) ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(self fileExists: newFileFullName)
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'MU 3/26/2014 12:37'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	(self directoryExists: oldFileFullName) ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(self directoryExists: newFileFullName)
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'MU 3/26/2014 12:30'!
bareEntriesIn: fullPath
	"FmFileDirectoryFileIOAccessor default bareEntriesIn: ''"
	| entries index done entryArray |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self lookupEntryIn: fullPath index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [entries addLast: (self bareEntryFromArray: entryArray in: fullPath)].
		index := index + 1].

	^ entries asArray! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'MU 3/26/2014 12:33'!
bareEntryFromArray: entryArray in: pathString
	^DirectoryEntry fromArray: entryArray directory: (FileDirectory on: pathString)! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'mu 4/2/2008 17:14'!
basicDirectoryExists: fullPathName

	| result |
	result := self lookupEntryIn: fullPathName index: 1.
 	^result ~= #badDirectoryPath! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'mu 4/5/2008 13:32'!
containingDirectoryPathOf: pathName 
	| lastIndex |
	lastIndex := 0.
	(((pathName isNil
			or: [pathName isEmpty])
			or: [pathName isPathSeparator])
			or: [pathName isDriveName])
		ifTrue: [^ nil].
	'\:/' do: [:each | lastIndex := ( pathName lastIndexOf: each) max: lastIndex].
	^ pathName copyFrom: 1 to: lastIndex - 1! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'mu 7/14/2007 20:11'!
imageDirectoryPath
	"Note that pathname is not encoded"
	| imagePath lastIndex |
	imagePath := self primImagePath.
	lastIndex := imagePath lastIndexOf: self pathNameDelimiter.
	^ imagePath copyFrom: 1 to: lastIndex! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'MU 3/26/2014 12:37'!
lookupEntryIn: fullPathName index: index
	
	^self primLookupEntryIn: fullPathName index: index! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'mu 3/14/2007 23:45'!
try: execBlock forFileNamed: fullName

	"If fail, return nil"

	^ (self concreteStreamClass retryWithGC: execBlock until: [:result | result notNil] forFileNamed: fullName) notNil! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'mu 3/19/2007 17:56'!
baseNameFor: localName 
	| delim index |
	delim := self extensionDelimiter.
	index := localName
				findLast: [:c | c = delim].
	^ index = 0
		ifTrue: [localName]
		ifFalse: [localName copyFrom: 1 to: index - 1]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'mu 4/2/2008 17:15'!
copyFile: fileStream1 toFile: fileStream2
	| buffer |
	buffer := String new: 50000.
	[fileStream1 atEnd] whileFalse:
		[fileStream2 nextPutAll: (fileStream1 nextInto: buffer)].
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'mu 3/14/2007 19:20'!
directoryNamesIn: fullPath
	"FmFileDirectoryFileIOAccessor default directoryNamesIn: 'D:\lang\Smalltalk\squeak\000Scallion'"
	
	^((self bareEntriesIn: fullPath) select: [:each | each isDirectory]) collect: [:each | each name]
	! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'MU 3/26/2014 12:37'!
encode: encoderName on: externalStream 
	encoderName ifNil: [^self]. "UTF-8 will be used"
	"externalStream
		converter: (TextConverter newForEncoding: encoderName)"! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'mu 3/14/2007 22:43'!
entryNamesIn: fullPath
	"FmFileDirectoryFileIOAccessor default entryNamesIn: 'D:\lang\Smalltalk\squeak\000Scallion'"
	
	^(self bareEntriesIn: fullPath) collect: [:each | each name]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'mu 3/19/2007 17:57'!
extensionFor: localName 
	| delim index |
	delim := self extensionDelimiter.
	index := localName
				findLast: [:c | c = delim].
	^ index = 0
		ifTrue: ['']
		ifFalse: [localName copyFrom: index + 1 to: localName size]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'mu 3/14/2007 22:46'!
fileNamesIn: fullPath
	"FmFileAccessor default fileNamesIn: 'D:\lang\Smalltalk\squeak\000Scallion'"
	
	^((self bareEntriesIn: fullPath) reject: [:each | each isDirectory]) collect: [:each | each name]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'utilities' stamp: 'mu 4/2/2008 17:15'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in nextSequentialNameFor: for more details"

	| baseName version extension i j |

	baseName := self baseNameFor: fileName.
	extension := self extensionFor: fileName.
	i := j := baseName findLast: [:c | c isDigit not].
	i = 0
		ifTrue: [version := 0]
		ifFalse:
			[(baseName at: i) = $.
				ifTrue:
					[version := (baseName copyFrom: i+1 to: baseName size) asNumber.
					j := j - 1]
				ifFalse: [version := 0].
			baseName := baseName copyFrom: 1 to: j].
	^ Array with: baseName with: version with: extension! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'accessing' stamp: 'mu 3/13/2007 12:05'!
concreteStreamClass
	^FileStream concreteStream! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'accessing' stamp: 'mu 3/14/2007 20:16'!
currentDriveName
	^self primImagePath asDriveName! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'accessing' stamp: 'mu 3/14/2007 22:56'!
defaultDirectoryPath
	"Answer the value of defaultDirectoryPath"
	defaultDirectoryPath ifNil: [self initDefaultDirectoryPath].
	^ defaultDirectoryPath! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'accessing' stamp: 'mu 3/14/2007 19:21'!
drives
	self onUnix ifTrue: [^#()].
	^(self bareEntriesIn: '') collect: [:each | each name]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'accessing' stamp: 'mu 3/19/2007 17:56'!
extensionDelimiter
	"Return the character used to delimit filename extensions on this platform. Most platforms use the period (.) character."

	^ $.
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'accessing' stamp: 'mu 3/14/2007 20:09'!
slash
	^self pathNameDelimiter asString! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'file stream creation' stamp: 'mu 3/27/2007 23:58'!
fileNamed: fullPathName
	"Open the file with the given name in this directory for writing."

	^ self concreteStreamClass fileNamed: fullPathName
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'file stream creation' stamp: 'mu 3/13/2007 12:09'!
forceNewFileNamed: fullPathName
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	^ self concreteStreamClass forceNewFileNamed: fullPathName
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'file stream creation' stamp: 'mu 3/27/2007 23:58'!
newFileNamed: fullPathName
	"Create a new file with the given name in this directory."

	^ self concreteStreamClass newFileNamed: fullPathName
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'file stream creation' stamp: 'mu 3/13/2007 12:05'!
readOnlyFileNamed: fullPathName
	"Open the existing file with the given name in this directory for read-only access."

	^ self concreteStreamClass readOnlyFileNamed: fullPathName
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'initialize-release' stamp: 'MU 3/26/2014 12:23'!
initDefaultDirectoryPath
	| dir |
	dir := self imageDirectoryPath.
	self initDefaultDirectoryPathWith: dir! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'initialize-release' stamp: 'MU 3/26/2014 12:21'!
initDefaultDirectoryPathWith: barePath
	| dirName |
	"dirName := (FilePath pathName: barePath isEncoded: true) asSqueakPathName."
	dirName := barePath.
	[dirName endsWith: self slash] whileTrue:[
		dirName := dirName copyFrom: 1 to: dirName size - self slash size.
	].
	defaultDirectoryPath := dirName! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'initialize-release' stamp: 'mu 3/14/2007 20:23'!
initialize
	super initialize.
	defaultDirectoryPath := nil! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'testing' stamp: 'mu 3/14/2007 22:07'!
isCaseSensitive
	"FmFileDirectoryFileIOAccessor default isCaseSensitive"
	^self onUnix! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'testing' stamp: 'mu 3/13/2007 21:40'!
isDriveSupported
	^self onWindows or: [self onMac]! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'testing' stamp: 'mu 3/14/2007 20:08'!
onMac
	^self pathNameDelimiter = $:! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'testing' stamp: 'mu 3/14/2007 20:08'!
onUnix
	^self pathNameDelimiter = $/! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'testing' stamp: 'mu 3/14/2007 20:08'!
onWindows
	^self pathNameDelimiter = $\! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:11'!
primCreateDirectory: fullPath
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:11'!
primDeleteDirectory: fullPath
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:11'!
primDeleteFileNamed: aFileName
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'primitives' stamp: 'mu 3/14/2007 19:27'!
primImagePath

	<primitive: 121>
	self primitiveFailed! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:12'!
primLookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	The empty string enumerates the top-level files or drives. (For example, on Unix, the empty path enumerates the contents of '/'. On Macs and PCs, it enumerates the mounted volumes/drives.)

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'primitives' stamp: 'mu 3/14/2007 19:51'!
primPathNameDelimiter
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed
! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:12'!
primRename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil! !


!FmFileDirectoryFileIOAccessorCuis class methodsFor: 'class initialization' stamp: 'mu 3/27/2013 23:33'!
initialize
	"FmFileDirectoryFileIOAccessor initialize"
	super initialize.
	self beDefault! !

FmFileDirectoryFileIOAccessorCuis initialize!
