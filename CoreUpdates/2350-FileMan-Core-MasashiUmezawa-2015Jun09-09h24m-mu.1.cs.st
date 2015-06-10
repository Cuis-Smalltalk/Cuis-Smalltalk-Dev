'From Cuis 4.2 of 25 July 2013 [latest update: #2350] on 9 June 2015 at 9:26:39.669503 am'!
!classDefinition: #FmFileEntry category: #'FileMan-Core'!
Object subclass: #FmFileEntry
	instanceVariableNames: 'drive pathComponents parent name fileSize creationTime modificationTime isDirectory options'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileEntry commentStamp: 'mu 11/6/2006 20:21' prior: 0!
I represent a single file entry (including directory).

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21!

!classDefinition: #FmDirectoryEntry category: #'FileMan-Core'!
FmFileEntry subclass: #FmDirectoryEntry
	instanceVariableNames: 'children'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmDirectoryEntry commentStamp: 'mu 11/6/2006 20:21' prior: 0!
I represent a single file directory.
I implement various directory specific behaviors.

You can write data by #at:put: , and read the data by #at:.

---
mu 11/6/2006 20:21
!

!classDefinition: #FmFileIOAccessor category: #'FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: ''
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileIOAccessor commentStamp: '<historical>' prior: 0!
I am an accessor to the low level file IO.

You can extend/rewrite me if you port FileMan to other Smalltalk dialects.

---
mu 3/13/2007 11:11!

!classDefinition: 'FmFileIOAccessor class' category: #'FileMan-Core'!
FmFileIOAccessor class
	instanceVariableNames: 'default librarySpecificClass dialectUtilClass'!
!classDefinition: #FmPortableUtil category: #'FileMan-Core'!
Object subclass: #FmPortableUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!

!Object methodsFor: '*fileman-core-converting' stamp: 'mu 10/12/2006 20:20'!
asFileEntry
	^self asString asFileEntry! !

!Object methodsFor: '*fileman-core-testing' stamp: 'mu 6/11/2006 17:28'!
isFileEntry
	^false! !


!ByteArray methodsFor: '*fileman-core-actions' stamp: 'mu 3/10/2008 17:51'!
fileContents
	^self asFileEntry fileContents! !

!ByteArray methodsFor: '*fileman-core-actions' stamp: 'mu 3/10/2008 17:51'!
fileContents: aString
	self asFileEntry fileContents: aString.
	^aString! !


!String methodsFor: '*fileman-core-converting' stamp: 'mu 4/5/2008 12:22'!
asAbsolutePathName

	^FmFileIOAccessor default asAbsolutePathName: self! !

!String methodsFor: '*fileman-core-converting' stamp: 'mu 6/15/2006 21:48'!
asDirectoryEntry
	^FmDirectoryEntry pathName: self! !

!String methodsFor: '*fileman-core-converting' stamp: 'mu 5/4/2007 22:37'!
asDirectoryEntry: directoryEntryClass
	^self asDirectoryEntry as: directoryEntryClass! !

!String methodsFor: '*fileman-core-converting' stamp: 'mu 3/15/2007 10:24'!
asDriveName
	(FmFileIOAccessor default onWindows and: [self beginsWithDriveLetter]) ifTrue: [
			^self copyFrom: 1 to: 2.
	].

	(FmFileIOAccessor default onMac) ifTrue: [ 
			^self upToFirstPathSeparator
	].

! !

!String methodsFor: '*fileman-core-converting' stamp: 'mu 6/11/2006 22:42'!
asFileEntry
	self isRelativeMark ifTrue: [^self asDirectoryEntry].
	^FmFileEntry pathName: self! !

!String methodsFor: '*fileman-core-converting' stamp: 'mu 5/4/2007 22:38'!
asFileEntry: fileEntryClass
	^self asFileEntry as: fileEntryClass! !

!String methodsFor: '*fileman-core-converting' stamp: 'mu 3/4/2007 11:04'!
asPathComponents
	| tokens |

	self isRelativePathName ifTrue: [self error: 'relative form is invaild!!'].

	tokens := self asPathTokens.
	^tokens! !

!String methodsFor: '*fileman-core-converting' stamp: 'mu 3/4/2007 11:04'!
asPathTokens
	| tokens |
	tokens := self findTokens: '\:/'.
	^tokens! !

!String methodsFor: '*fileman-core-testing' stamp: 'mu 3/3/2007 09:46'!
beginsWithDriveLetter
	self size < 2 ifTrue: [^false].
	^self first isDriveLetter and: [self second isDriveSeparator ]! !

!String methodsFor: '*fileman-core-testing' stamp: 'mu 3/4/2007 10:22'!
beginsWithPathSeparator
	self ifEmpty: [^false].
	^self first isPathSeparator! !

!String methodsFor: '*fileman-core-testing' stamp: 'mu 4/6/2008 20:36'!
isAbsolutePathName
	| upperName |
	self isEmpty ifTrue: [^ false].
	('\:/' includes: self first) ifTrue: [^ true].
	upperName := self asUppercase.
	(FmFileIOAccessor default drives
			anySatisfy: [:each | (upperName beginsWith: each)
					and: [| nextPos | 
						nextPos := each size + 1 min: self size max: 1.
						'\:/' includes: (self at: nextPos)]]) ifTrue: [^ true].
	^ false! !

!String methodsFor: '*fileman-core-testing' stamp: 'mu 3/15/2007 10:24'!
isDriveName
	FmFileIOAccessor default onWindows
		ifTrue: [^ (self size between: 2 and: 3)
				and: [(self second = $:)
						and: [self first asCharacter isDriveLetter]]].

	FmFileIOAccessor default onMac ifTrue: [^FileDirectory root directoryNames includes: self].

	^false! !

!String methodsFor: '*fileman-core-testing' stamp: 'mu 3/2/2007 22:05'!
isPathSeparator
	self = '\' ifTrue: [^true].
	self = ':' ifTrue: [^true].
	self = '/' ifTrue: [^true].
	^false! !

!String methodsFor: '*fileman-core-testing' stamp: 'mu 6/11/2006 22:41'!
isRelativeMark
	
	self = '.' ifTrue: [^true].
	self = '..' ifTrue: [^true].

	^false! !

!String methodsFor: '*fileman-core-testing' stamp: 'mu 3/4/2007 10:46'!
isRelativePathName
	self ifEmpty: [^false].
	self isRelativeMark ifTrue: [^true].
	^#('.' '..') includes: self upToFirstPathSeparator ! !

!String methodsFor: '*fileman-core-actions' stamp: 'mu 6/11/2006 22:12'!
fileContents
	^self asFileEntry fileContents! !

!String methodsFor: '*fileman-core-actions' stamp: 'mu 6/11/2006 22:19'!
fileContents: aString
	self asFileEntry fileContents: aString.
	^aString! !

!String methodsFor: '*fileman-core-actions' stamp: 'mu 3/10/2008 17:52'!
fileContents: aString by: encoderName
	self asFileEntry textContents: aString by: encoderName.
	^aString! !

!String methodsFor: '*fileman-core-actions' stamp: 'mu 3/10/2008 17:53'!
fileContentsBy: encoderName
	^self asFileEntry textContentsBy: encoderName.
! !

!String methodsFor: '*fileman-core-accessing' stamp: 'mu 3/3/2007 19:38'!
indexOfFirstPathSeparator
	self
		doWithIndex: [:each :idx | each isPathSeparator
				ifTrue: [^ idx]].
	^ 0! !

!String methodsFor: '*fileman-core-accessing' stamp: 'mu 5/7/2007 15:48'!
indexOfLastPathSeparator
	self reverse
		doWithIndex: [:each :idx | each isPathSeparator
				ifTrue: [^ (self size - idx) + 1]].
	^ 0! !

!String methodsFor: '*fileman-core-accessing' stamp: 'mu 5/7/2007 15:55'!
pathAndLocalName
	| index path localName |
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^{''. self}].

	path := self copyFrom: 1 to: index-1.
	localName := self copyFrom: index+1 to: self size.

	^{path. localName}

	! !

!String methodsFor: '*fileman-core-accessing' stamp: 'mu 3/4/2007 10:45'!
upToFirstPathSeparator
	| index |
	self ifEmpty: [^self].
	index := self indexOfFirstPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
	! !

!String methodsFor: '*fileman-core-accessing' stamp: 'mu 5/7/2007 15:49'!
upToLastPathSeparator
	| index |
	self ifEmpty: [^self].
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
	! !


!FmFileEntry methodsFor: 'actions-path' stamp: 'mu 6/19/2006 23:29'!
, pathString
	^self concat: pathString! !

!FmFileEntry methodsFor: 'actions-path' stamp: 'mu 6/19/2006 23:52'!
/ pathString
	^self, pathString! !

!FmFileEntry methodsFor: 'actions-path' stamp: 'mu 7/30/2006 14:06'!
concat: pathString
	^self concatPathComponents: pathString asString asPathComponents.! !

!FmFileEntry methodsFor: 'actions-path' stamp: 'MasashiUmezawa 7/8/2012 16:25'!
concatPathComponents: components 
	| entry |
	components ifEmpty: [^self].

	entry := self fileEntryClass pathComponents: (self pathComponents copy addAll: components;
			 yourself) drive: self drive.

	self isRoot ifFalse: [
		entry parent: self asDirectoryEntry.
	].
	^entry! !

!FmFileEntry methodsFor: 'actions-rio' stamp: 'mu 3/12/2007 23:32'!
< aStringOrBytes 
	self fileContents: aStringOrBytes! !

!FmFileEntry methodsFor: 'actions-rio' stamp: 'mu 3/12/2007 23:32'!
<< aStringOrBytes 
	self appendContents: aStringOrBytes! !

!FmFileEntry methodsFor: 'comparing' stamp: 'mu 6/11/2006 18:01'!
= aFileEntry
	self class = aFileEntry class ifFalse: [^false].
	^self pathComponents = aFileEntry pathComponents! !

!FmFileEntry methodsFor: 'comparing' stamp: 'mu 10/10/2008 16:04'!
hash
	^self pathComponents hash! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:47'!
allChildrenDo: aBlock
	"do nothing"! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'MasashiUmezawa 7/8/2012 21:37'!
allChildrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	"do nothing"! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:50'!
allDirectoriesDo: aBlock
	"do nothing"! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:51'!
allFilesDo: aBlock
	"do nothing"! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'MasashiUmezawa 7/8/2012 17:18'!
allFilesDo: aBlock matches: selectionBlock
	"do nothing"! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 10/12/2006 20:18'!
assureExistence
	self exists ifTrue: [^self].
	self parent assureExistence.
	self writeStream close! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:57'!
childrenDo: aBlock
	^self asDirectoryEntry childrenDo: aBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'MasashiUmezawa 7/8/2012 21:31'!
childrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	"do nothing"! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:57'!
directoriesDo: aBlock
	^self asDirectoryEntry directoriesDo: aBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 8/9/2006 13:55'!
directoriesMatches: selectionBlock
	^self asDirectoryEntry directoriesMatches: selectionBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:57'!
filesDo: aBlock
	^self asDirectoryEntry filesDo: aBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 8/9/2006 13:54'!
filesMatches: selectionBlock
	^self asDirectoryEntry filesMatches: selectionBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 8/9/2006 13:55'!
latestFileMatches: selectionBlock
	^self asDirectoryEntry latestFileMatches: selectionBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'mu 8/9/2006 13:55'!
oldestFileMatches: selectionBlock
	^self asDirectoryEntry oldestFileMatches: selectionBlock! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 9/27/2006 20:21'!
appendContents: aStringOrBytes 
	self
		appendStreamContents: [:str | 
			aStringOrBytes isString
				ifFalse: [str binary].
			str nextPutAll: aStringOrBytes]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 8/10/2006 18:13'!
binaryContents
	^self contentsOf: self readStream binary! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 8/10/2006 18:15'!
binaryContents: aByteArray
	self setContentsOf: self writeStream binary to: aByteArray ! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 3/28/2013 00:39'!
copyTo: aFileEntryOrString 
	| targetEntry |
	(self fileAccessor directoryExists: self pathName) ifTrue: [^self asDirectoryEntry copyTo: aFileEntryOrString].
	
	targetEntry := aFileEntryOrString asFileEntry.
	targetEntry isDirectory
		ifTrue: [targetEntry := (targetEntry / self name) ensureAsFile].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self pathName to: targetEntry pathName 

	! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 3/15/2007 19:52'!
delete
	self fileAccessor deleteFile: self pathName.
	! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 8/10/2006 18:12'!
fileContents
	"Default is text mode"
	^self textContents! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 8/10/2006 18:15'!
fileContents: aStringOrBytes 
	aStringOrBytes isString
		ifTrue: [self textContents: aStringOrBytes]
		ifFalse: [self binaryContents: aStringOrBytes]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 3/13/2007 11:52'!
rename: newName
	
	self fileAccessor rename: self pathName to: (self parent / newName) pathName.
	self name: newName! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 8/10/2006 18:12'!
textContents
	^self contentsOf: self readStream! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 8/10/2006 18:13'!
textContents: aString 
	self setContentsOf: self writeStream to: aString ! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 3/10/2008 15:58'!
textContents: aString by: encoderName
	self setContentsOf: self writeStream to: aString by: encoderName! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'mu 3/10/2008 15:58'!
textContentsBy: encoderName 
	^self contentsOf: self readStream by: encoderName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'mu 3/27/2007 23:53'!
appendStream
	self exists
		ifFalse: [^ self writeStream].
	^ (self fileAccessor fileNamed: self pathName) setToEnd! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'MU 3/26/2014 12:39'!
appendStreamContents: blockWithArg 
	| stream |
	stream := self appendStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'mu 4/7/2008 16:12'!
readStream
	^ [self fileAccessor readOnlyFileNamed: self pathName]
		on: FmFileIOAccessor fileDoesNotExistException
		do: [:ex |
			self assureExistence.
			ex retry]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'MU 3/26/2014 12:41'!
readStreamContents: blockWithArg 
	| stream conts |
	stream := self readStream.
	[conts := blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]].
	^ conts! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'mu 3/27/2007 23:51'!
writeStream
	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor forceNewFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'mu 3/27/2007 23:52'!
writeStreamConfirming
	self refresh.
	^self fileAccessor newFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'MU 3/26/2014 12:41'!
writeStreamContents: blockWithArg 
	| stream |
	stream := self writeStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !

!FmFileEntry methodsFor: 'converting' stamp: 'mu 3/27/2007 15:30'!
as: aCustomFileEntryClass
	"Usable when adding new behaviors to the existing FileEntry"
	^aCustomFileEntryClass pathComponents: self pathComponents drive: self drive! !

!FmFileEntry methodsFor: 'converting' stamp: 'mu 3/27/2007 23:10'!
asDirectoryEntry
	^ self as: self directoryEntryClass! !

!FmFileEntry methodsFor: 'converting' stamp: 'mu 5/4/2007 22:38'!
asDirectoryEntry: directoryEntryClass
	^self asDirectoryEntry as: directoryEntryClass! !

!FmFileEntry methodsFor: 'converting' stamp: 'mu 9/21/2006 10:20'!
asFileEntry
	^self! !

!FmFileEntry methodsFor: 'converting' stamp: 'mu 5/4/2007 22:38'!
asFileEntry: fileEntryClass
	^self asFileEntry as: fileEntryClass! !

!FmFileEntry methodsFor: 'converting' stamp: 'mu 3/28/2013 00:23'!
ensureAsDirectory
	^self asDirectoryEntry! !

!FmFileEntry methodsFor: 'converting' stamp: 'mu 3/28/2013 00:22'!
ensureAsFile
	^self! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 7/30/2006 22:45'!
at: localFileName 
	^self asDirectoryEntry at: localFileName asString ifAbsent:[]! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 7/27/2006 17:33'!
at: localFileName ifAbsent: block
	^self asDirectoryEntry at: localFileName asString ifAbsent: block! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 7/19/2006 16:59'!
at: localFileName put: contents
	^self asDirectoryEntry at: localFileName asString put: contents! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 8/10/2006 18:47'!
binaryAt: localFileName 
	^self asDirectoryEntry binaryAt: localFileName asString ifAbsent:[]! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 8/10/2006 18:47'!
binaryAt: localFileName ifAbsent: block
	^self asDirectoryEntry binaryAt: localFileName asString ifAbsent: block! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 11/6/2006 20:38'!
includesKey: fileName
	^self asDirectoryEntry includesKey: fileName! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 7/27/2006 17:35'!
keys
	^self asDirectoryEntry keys! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 7/19/2006 17:00'!
removeKey: localFileName 
	^self asDirectoryEntry removeKey: localFileName asString! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'mu 9/20/2006 18:10'!
removeKey: localFileName ifAbsent: failBlock
	^self asDirectoryEntry removeKey: localFileName asString ifAbsent: failBlock! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'mu 11/30/2006 17:58'!
baseName
	^self nameVersionExtension first! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'mu 6/19/2006 23:27'!
extension
	^self nameVersionExtension last! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'mu 3/14/2007 18:03'!
nameVersionExtension
	^self fileAccessor splitNameVersionExtensionFor: self name! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'mu 10/25/2010 22:24'!
nameWithoutExtension
	| nm index |
	nm := self name.
	index := nm lastIndexOf: $..
	index = 0 ifTrue: [^nm].
	nm first = $. ifTrue: [^nm].
	
	^self name copyFrom: 1 to: index - 1! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'mu 10/24/2007 23:08'!
shortName
	^self name readStream upTo: $.! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'mu 3/13/2007 21:42'!
slash
	^self fileAccessor slash! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'mu 6/19/2006 23:28'!
version
	^self nameVersionExtension second! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 17:49'!
basicPathComponents: aCollection
	pathComponents := aCollection! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 8/1/2006 19:24'!
children
	^self asDirectoryEntry children! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 18:19'!
creationTime
	creationTime ifNil: [self initValuesFromParent].
	^creationTime! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 14:03'!
creationTime: value
	creationTime := value! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 8/1/2006 19:21'!
directories
	^self asDirectoryEntry directories! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 8/1/2006 19:23'!
directory
	^self asDirectoryEntry directory! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 3/3/2007 22:46'!
drive
	self onUnix ifTrue: [^ drive := nil].
	drive
		ifNil: [drive := self currentDriveName].
	^ drive! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 13:26'!
drive: aString
	drive := aString! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 3/2/2007 21:04'!
drives
	^self class drives! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 8/1/2006 19:23'!
file
	^self asDirectoryEntry file! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 18:19'!
fileSize
	fileSize ifNil: [self initValuesFromParent].
	^fileSize! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 14:07'!
fileSize: value
	fileSize := value! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 8/1/2006 19:22'!
files
	^self asDirectoryEntry files! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 5/4/2007 23:08'!
isDirectory: aBoolean
	isDirectory := aBoolean! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 18:19'!
modificationTime
	modificationTime ifNil: [self initValuesFromParent].
	^modificationTime! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 14:03'!
modificationTime: value
	modificationTime := value! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 13:35'!
name
	^name! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/18/2006 22:05'!
name: aString 
	name := aString.
	self pathComponents
		ifNotEmpty: [self pathComponents at: self pathComponents size put: name]! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 3/27/2007 15:21'!
options
	options ifNil: [options := IdentityDictionary new].
	^options! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/19/2006 23:21'!
options: aCollection
	options := aCollection! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 17:41'!
parent
	parent ifNil: [parent := self ensureParent].
	^parent! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 3/13/2007 21:43'!
parent: aFmDirectory
	parent := aFmDirectory.

	self pathName: aFmDirectory pathName, self slash, self name! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 1/24/2012 15:44'!
parents
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 13:16'!
pathComponents
	pathComponents ifNil: [pathComponents := OrderedCollection new].
	^pathComponents! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 4/6/2008 14:24'!
pathName
	| str |
	str := String new writeStream.
	self printPathOn: str.
	^str contents! !

!FmFileEntry methodsFor: 'accessing' stamp: 'mu 3/27/2007 23:12'!
root
	^self directoryEntryClass root! !

!FmFileEntry methodsFor: 'private' stamp: 'mu 3/10/2008 15:52'!
contentsOf: aStream
	^self contentsOf: aStream by: nil! !

!FmFileEntry methodsFor: 'private' stamp: 'MU 3/26/2014 12:39'!
contentsOf: aStream by: encoderName
	| str conts |
	[str := aStream.
	str isBinary ifFalse: [self fileAccessor encode: encoderName on: str]. 
	conts := str contents]
		ensure: [str
				ifNotNil: [:s | s close]].
	^ conts! !

!FmFileEntry methodsFor: 'private' stamp: 'mu 3/14/2007 19:30'!
currentDriveName
	^self fileAccessor currentDriveName! !

!FmFileEntry methodsFor: 'private' stamp: 'mu 3/27/2007 23:11'!
ensureParent
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent := self directoryEntryClass
				pathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent! !

!FmFileEntry methodsFor: 'private' stamp: 'mu 6/11/2006 18:18'!
initValuesFrom: otherEntry
	otherEntry ifNil: [^self].
	self creationTime: otherEntry creationTime.
	self modificationTime: otherEntry modificationTime.
	self fileSize: otherEntry fileSize.
	! !

!FmFileEntry methodsFor: 'private' stamp: 'mu 6/11/2006 18:18'!
initValuesFromParent
	| targets target |
	self ensureParent.
	self parent ifNil: [^self].

	targets := self isDirectory ifTrue: [self parent directories] ifFalse: [self parent files].
	target := targets detect: [:each | each = self] ifNone:[].

	self initValuesFrom: target.
	! !

!FmFileEntry methodsFor: 'private' stamp: 'mu 3/10/2008 15:41'!
setContentsOf: aStream to: aStringOrBytes 
	self setContentsOf: aStream to: aStringOrBytes by: nil! !

!FmFileEntry methodsFor: 'private' stamp: 'MU 3/26/2014 12:41'!
setContentsOf: aStream to: aStringOrBytes by: encoderName
	| str |
	[str := aStream.
	aStringOrBytes isString ifTrue: [self fileAccessor encode: encoderName on: str] ifFalse: [str binary].
	str nextPutAll: aStringOrBytes.
	self refresh]
		ensure: [str
				ifNotNil: [:s | s close]]! !

!FmFileEntry methodsFor: 'private' stamp: 'mu 3/27/2007 23:12'!
setDriveNameAndPathComponentsFrom: rawPathString 
	|  guessedDriveName |

	rawPathString first isPathSeparator
		ifTrue: [^self pathComponents: (rawPathString asPathTokens)].

	guessedDriveName := rawPathString asDriveName.
	guessedDriveName isDriveName
		ifTrue: [ | targetPathString | 
			self drive: guessedDriveName.
			targetPathString := rawPathString copyFrom: guessedDriveName size + 1 to: rawPathString size.
			^self pathComponents: targetPathString asPathTokens].

	^self pathComponents: (self directoryEntryClass default pathComponents copyWith: rawPathString)! !

!FmFileEntry methodsFor: 'factory' stamp: 'mu 3/28/2007 00:01'!
directoryEntryClass
	"override"
	^self class directoryEntryClass! !

!FmFileEntry methodsFor: 'factory' stamp: 'mu 3/28/2007 00:01'!
fileAccessor
	"override"
	^FmFileIOAccessor default! !

!FmFileEntry methodsFor: 'factory' stamp: 'mu 3/28/2007 00:01'!
fileEntryClass
	"override"
	^self class fileEntryClass! !

!FmFileEntry methodsFor: 'testing' stamp: 'MU 3/26/2014 12:40'!
exists
	| pathName |
	pathName := self parent ifNil: [''] ifNotNil: [:par | par pathName].
	^self fileAccessor fileExists: self name in: pathName! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 5/4/2007 23:11'!
isDirectory
	isDirectory ifNil: [^false].
	^ isDirectory! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 3/13/2007 21:39'!
isDriveSupported
	^self fileAccessor isDriveSupported! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 5/4/2007 23:20'!
isFile
	^self isDirectory not! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 6/11/2006 17:27'!
isFileEntry
	^true! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 3/15/2007 13:08'!
isRoot
	^self parent isNil! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 3/13/2007 21:38'!
onMac
	^self fileAccessor onMac! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 3/13/2007 21:38'!
onUnix
	^self fileAccessor onUnix! !

!FmFileEntry methodsFor: 'testing' stamp: 'mu 3/13/2007 21:38'!
onWindows
	^self fileAccessor onWindows! !

!FmFileEntry methodsFor: 'initialize-release' stamp: 'mu 10/26/2006 19:20'!
pathComponents: aCollection
	self pathComponents: aCollection detectDrive: true! !

!FmFileEntry methodsFor: 'initialize-release' stamp: 'mu 3/3/2007 23:34'!
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

!FmFileEntry methodsFor: 'initialize-release' stamp: 'mu 3/27/2007 23:12'!
pathName: aString 
	| path tokens |
	path := aString asAbsolutePathName.
	tokens := path findTokens: self slash.
	tokens ifEmpty: [^ nil].
	(self onUnix and: [path beginsWithPathSeparator]) ifTrue: [
		^ self pathComponents: ((path beginsWith: '/') ifTrue: [tokens] ifFalse: [path asPathTokens])].
	tokens size = 1
		ifTrue: [
			path isPathSeparator ifTrue: [^self].
			self isDriveSupported
				 ifTrue: [^self setDriveNameAndPathComponentsFrom: aString].
			 ^self pathName: (self directoryEntryClass default concat: aString) pathName].
	self pathComponents: tokens! !

!FmFileEntry methodsFor: 'initialize-release' stamp: 'mu 9/13/2006 19:36'!
refresh
	fileSize := creationTime := modificationTime := nil! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'mu 10/5/2006 19:47'!
pipe: filterBlock to: aFileEntryOrString 
	| nextEntry inStream outStream |
	nextEntry := aFileEntryOrString asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	filterBlock value: inStream value: outStream]
		ensure: [inStream close.
			outStream close].
	^nextEntry
! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'mu 10/5/2006 19:41'!
pipeRepeat: filterBlock to: aFileEntryOrString 
	^self pipeRepeat: filterBlock while: [:in :out | in atEnd not] to: aFileEntryOrString ! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'mu 10/5/2006 19:48'!
pipeRepeat: filterBlock while: terminateBlock to: aFileEntryOrString 
	| nextEntry inStream outStream |
	nextEntry := aFileEntryOrString asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	[terminateBlock value: inStream value: outStream]
		whileTrue: [filterBlock value: inStream value: outStream]]
		ensure: [inStream close.
			outStream close].
	^nextEntry! !

!FmFileEntry methodsFor: 'printing' stamp: 'mu 2/28/2007 00:06'!
printOn: aStream 
	self printPathOn: aStream! !

!FmFileEntry methodsFor: 'printing' stamp: 'MU 3/26/2014 12:43'!
printPathOn: aStream 
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPutAll: self slash.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPutAll: self slash]! !


!FmFileEntry class methodsFor: 'factory' stamp: 'mu 3/27/2007 23:06'!
directoryEntryClass
	"override"
	^FmDirectoryEntry! !

!FmFileEntry class methodsFor: 'factory' stamp: 'mu 5/4/2007 22:40'!
fileEntryClass
	"override"
	^self! !

!FmFileEntry class methodsFor: 'accessing' stamp: 'mu 3/15/2007 10:24'!
drives
	"FmFileEntry drives"
	^FmFileIOAccessor default drives! !

!FmFileEntry class methodsFor: 'instance creation' stamp: 'mu 5/4/2007 23:08'!
entry: bareFileEntry
	| inst |
	inst := bareFileEntry isDirectory ifTrue: [self directoryEntryClass new] ifFalse: [self fileEntryClass new].
	inst isDirectory: bareFileEntry isDirectory.
	inst name: bareFileEntry name.
	inst initValuesFrom: bareFileEntry.
	^inst! !

!FmFileEntry class methodsFor: 'instance creation' stamp: 'mu 6/11/2006 17:42'!
pathComponents: comps
	| inst |
	inst := self new.
	inst pathComponents: comps.
	^inst

	! !

!FmFileEntry class methodsFor: 'instance creation' stamp: 'mu 10/26/2006 19:21'!
pathComponents: comps drive: driveString
	| inst |
	inst := self new.
	inst pathComponents: comps detectDrive: false.
	inst drive: driveString.
	^inst

	! !

!FmFileEntry class methodsFor: 'instance creation' stamp: 'mu 6/11/2006 22:21'!
pathName: aString
	| inst |
	inst := self new.
	inst pathName: aString.
	^inst

	! !

!FmFileEntry class methodsFor: 'examples' stamp: 'mu 3/27/2013 23:51'!
example1
	"FmFileEntry example1"

	"- Create subDirectory named: 'subDir'.
	 - Put a new file named: 'file1'.
	 - Write contents 'Hello!! to that file'"

	"Traditional way (Squeak)"
	"| subDir str |
	subDir := FileDirectory default directoryNamed: 'subDir'.
	subDir assureExistence.
	[str := subDir newFileNamed: 'file1'.
	str nextPutAll: 'Hello!!']
		ensure: [str close]."

	"FileMan"

	'./subDir' asDirectoryEntry at: 'file2' put: 'Hello!!'! !

!FmFileEntry class methodsFor: 'examples' stamp: 'mu 3/27/2013 23:53'!
example2
	"FmFileEntry example2"

	"FileMan's path representation is portable"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') at: 'test1' put: 'Hello2!!'.! !

!FmFileEntry class methodsFor: 'examples' stamp: 'mu 3/27/2013 23:54'!
example3
	"FmFileEntry example3"

	"Remove 'test1' file created exapmle2"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') removeKey: 'test1'.

	"Recursive delete"
	'./subDir' asDirectoryEntry recursiveDelete! !

!FmFileEntry class methodsFor: 'examples' stamp: 'mu 6/20/2006 00:24'!
example4
	"FmFileEntry example4"

	"Collect sm directory package names"
	| packageNames |
	packageNames := './sm/cache/packages' asDirectoryEntry directories collect: [:each | each directory file name].
	packageNames inspect! !

!FmFileEntry class methodsFor: 'examples' stamp: 'mu 9/21/2006 09:57'!
example5
	"FmFileEntry example5"
	
	"Write test1 file and copy it to test2 in the parent directory"

	'./test1' fileContents: 'This is a test'.
	'./test1' asFileEntry copyTo: '../test2'.

	'../test2' fileContents inspect! !

!FmFileEntry class methodsFor: 'examples' stamp: 'mu 10/5/2006 19:58'!
example6
	"FmFileEntry example6"
	
	"test1 file contents will be written to test2 using reverse filter.
	 test2 file contents will be written to test3 again using reverse filter."

	| reverseFilter |
	'test1.txt' fileContents: 'This is a test'.
	reverseFilter := [:in :out | out nextPutAll: (in upToEnd reverse)].

	('test1.txt' asFileEntry pipe: reverseFilter to: 'test2.txt')
		pipe: reverseFilter to: 'test3.txt'.

	(#('test1.txt' 'test2.txt' 'test3.txt') collect: [:each | each fileContents]) inspect

	! !

!FmFileEntry class methodsFor: 'examples' stamp: 'mu 3/28/2013 00:29'!
example7
	"FmFileEntry example7"
	"Useful example: copy latest mcz files to releaseDir (for preparing SAR)"

	| releaseDir dir fileNames |
	releaseDir := './releasePkg' asDirectoryEntry.
	releaseDir keys isEmpty ifFalse: [releaseDir recursiveDelete].
	dir := './package-cache' asDirectoryEntry.
	fileNames := ((dir keys collect: [:each | each name readStream upTo: $.]) select: [:each | each notEmpty]) asSet.
	(fileNames collect: [:eachName | dir
				latestFileMatches: [:each | each name beginsWith: eachName]])
		do: [:eachEnt | eachEnt copyTo: releaseDir]! !

!FmFileEntry class methodsFor: 'class initialization' stamp: 'mu 3/26/2013 01:25'!
obsolete
	FmFileIOAccessor dialectUtilClass removeFromStartUpList: self.
	super obsolete! !

!FmFileEntry class methodsFor: 'starting up' stamp: 'mu 3/26/2013 01:23'!
onSystemStartUp
	self allSubInstancesDo: [:each | each refresh]! !


!FmDirectoryEntry methodsFor: 'actions-path' stamp: 'mu 3/28/2013 01:19'!
/ pathString
	| ent |
	ent := super / pathString.
	(((pathString select: [:e | e = $.]) size) = 1 and: [pathString anySatisfy: [:e | e isLetter]]) 
			ifTrue: [^ent ensureAsFile].
	^ ent! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:45'!
allChildrenDo: aBlock
	self childrenDo: 
		[:child | 
		aBlock value: child.
		child allChildrenDo: aBlock]! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'MasashiUmezawa 7/8/2012 21:55'!
allChildrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	self childrenDo: 
		[:child | 
		(child isFile and: [selectionBlock value: child]) 
			ifTrue: [aBlock value: child]
			ifFalse: [(child isDirectory and: [dirSelectionBlock value: child]) ifTrue: [aBlock value: child]].
		child allChildrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock]! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:49'!
allDirectoriesDo: aBlock
	self directoriesDo: 
		[:child | 
		aBlock value: child.
		child allDirectoriesDo: aBlock]! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:54'!
allFilesDo: aBlock
	self childrenDo: 
		[:child | 
		child isFile ifTrue: [aBlock value: child] ifFalse: [child allFilesDo: aBlock]]! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'MasashiUmezawa 7/8/2012 22:00'!
allFilesDo: aBlock matches: selectionBlock
	self childrenDo: 
		[:child | 
		child isFile
			ifTrue: [(selectionBlock value: child) ifTrue: [aBlock value: child]]
			ifFalse: [child allFilesDo: aBlock matches: selectionBlock]]! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:43'!
childrenDo: aBlock
	^self children do: aBlock! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'MasashiUmezawa 7/8/2012 21:45'!
childrenDo: aBlock filesMatches: selectionBlock directoryMatches: dirSelectionBlock
	self children do: [:each | 
		(each isFile and: [selectionBlock value: each]) 
			ifTrue: [aBlock value: each]
			ifFalse: [(each isDirectory and: [dirSelectionBlock value: each]) ifTrue: [aBlock value: each]]
	]! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:42'!
directoriesDo: aBlock
	^self directories do: aBlock! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 6/18/2006 18:49'!
directoriesMatches: selectionBlock
	^self directories select: selectionBlock! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 9/13/2006 19:40'!
filesDo: aBlock
	^self files do: aBlock! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 6/18/2006 18:48'!
filesMatches: selectionBlock
	^self files select: selectionBlock! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 6/18/2006 18:48'!
latestFileMatches: selectionBlock
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) first! !

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'mu 6/18/2006 18:48'!
oldestFileMatches: selectionBlock
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) last! !

!FmDirectoryEntry methodsFor: 'converting' stamp: 'mu 9/21/2006 10:21'!
asDirectoryEntry
	^self! !

!FmDirectoryEntry methodsFor: 'converting' stamp: 'mu 3/28/2013 00:25'!
asFileEntry
	"For converting to FileEntry, use #ensureAsFile"
	^super asFileEntry! !

!FmDirectoryEntry methodsFor: 'converting' stamp: 'mu 3/28/2013 00:23'!
ensureAsDirectory
	^self! !

!FmDirectoryEntry methodsFor: 'converting' stamp: 'mu 3/28/2013 00:22'!
ensureAsFile
	^self as: self fileEntryClass! !

!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'mu 3/15/2007 13:09'!
assureExistence
	self isRoot ifTrue: [^self].
	self parent assureExistenceFileNamed: self name.
	^self! !

!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'mu 10/25/2010 22:44'!
copyTo: aDirectoryEntryOrString 
	| fileEnt toDir |
	fileEnt := aDirectoryEntryOrString asFileEntry.
	(self fileAccessor fileExists: fileEnt pathName) ifTrue: [^self error: 'Cannot copy directory to file'].
	
	toDir := fileEnt asDirectoryEntry assureExistence.

	self
		filesDo: [:file | file copyTo: (toDir / file name)].
	
	self
		directoriesDo: [:dir | | copyToDir |
			copyToDir := toDir pathName / dir name.
			dir copyTo: copyToDir]! !

!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'mu 3/15/2007 19:52'!
delete
	self fileAccessor deleteDirectory: self pathName.
	self refreshChildren! !

!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'mu 3/15/2007 19:53'!
recursiveDelete
	self exists
		ifTrue: [self basicRecursiveDelete]! !

!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'mu 6/4/2010 02:16'!
rename: newName
	
	self fileAccessor renameDirectory: self pathName to: (self parent / newName) pathName.
	self name: newName! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'MU 3/26/2014 12:38'!
assureExistenceFileNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self pathName) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceFileNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'mu 3/15/2007 19:53'!
basicRecursiveDelete
	self refreshChildren.
	self directoriesDo: [:dir | dir basicRecursiveDelete].
	self filesDo: [:file | file delete].
	self delete! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'mu 3/27/2007 23:11'!
entriesIn: directoryFullPath from: parentEntry 
	^ (self fileAccessor bareEntriesIn: directoryFullPath)
		collect: [:each | 
			| entry | 
			entry := self fileEntryClass entry: each.
			entry parent: parentEntry.
			entry]! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'mu 3/27/2007 23:04'!
initChildren
	| dir |
	dir := self pathName.
	self exists ifFalse: [^children := #()].
	children := self entriesIn: dir from: self.
	^children! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/10/2008 15:56'!
at: localFileName 
	^self at: localFileName by: nil! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/10/2008 15:56'!
at: localFileName by: encoderName
	^self contentsOf: (self fileAccessor readOnlyFileNamed: (self / localFileName) pathName) by: encoderName! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 4/7/2008 16:12'!
at: localFileName ifAbsent: block 
	^ [self at: localFileName]
		on: FmFileIOAccessor fileDoesNotExistException
		do: [:ex | block value]! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/10/2008 15:48'!
at: localFileName put: contents 
	^self at: localFileName put: contents by: nil! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/10/2008 15:48'!
at: localFileName put: contents by: encoderName
	self assureExistence.
	self setContentsOf: (self fileAccessor forceNewFileNamed: (self / localFileName) pathName) to: contents by: encoderName.
	self refreshChildren.
	^contents! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/13/2007 12:12'!
binaryAt: localFileName 
	^self contentsOf: (self fileAccessor readOnlyFileNamed: (self / localFileName) pathName) binary! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 4/7/2008 16:12'!
binaryAt: localFileName ifAbsent: block 
	^ [self binaryAt: localFileName]
		on: FmFileIOAccessor fileDoesNotExistException
		do: [:ex | block value]! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 9/21/2006 10:07'!
binaryAt: localFileName put: contents 
	^self at: localFileName put: contents asByteArray! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/13/2007 12:14'!
includesKey: fileName
	^self fileNames includes: fileName! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 7/27/2006 17:35'!
keys
	^self files! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/13/2007 11:55'!
removeKey: localFileName 
	self removeKey: localFileName ifAbsent: []! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'mu 3/15/2007 18:59'!
removeKey: localFileName ifAbsent: failBlock
	self fileAccessor deleteFile: (self / localFileName) pathName ifAbsent: [^failBlock value].
	self refreshChildren.! !

!FmDirectoryEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 17:37'!
children
	children ifNil: [self initChildren].
	^children! !

!FmDirectoryEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 17:26'!
directories
	^self children select: [:each | each isFile not]! !

!FmDirectoryEntry methodsFor: 'accessing' stamp: 'mu 6/20/2006 00:22'!
directory
	^self directories ifEmpty: [] ifNotEmpty: [:ds | ds first]! !

!FmDirectoryEntry methodsFor: 'accessing' stamp: 'mu 3/10/2008 15:52'!
directoryNames
	^self directories collect: [:each | each name]! !

!FmDirectoryEntry methodsFor: 'accessing' stamp: 'mu 6/20/2006 00:22'!
file
	^self files ifEmpty: [] ifNotEmpty: [:fs | fs first]! !

!FmDirectoryEntry methodsFor: 'accessing' stamp: 'mu 3/13/2007 12:14'!
fileNames
	^self files collect: [:each | each name]! !

!FmDirectoryEntry methodsFor: 'accessing' stamp: 'mu 6/11/2006 17:27'!
files
	^self children select: [:each | each isFile]! !

!FmDirectoryEntry methodsFor: 'testing' stamp: 'mu 5/15/2007 14:24'!
exists
	
	(self isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^self drives includes: self drive].

	self isRoot ifTrue: [^true].
	
	self fileAccessor onUnix ifFalse: [^self fileAccessor directoryExists: self pathName]. 
	
	^self fileAccessor directoryExists: self name in: self parent pathName! !

!FmDirectoryEntry methodsFor: 'testing' stamp: 'mu 5/4/2007 23:19'!
isDirectory
	isDirectory ifNil: [^true].
	^isDirectory! !

!FmDirectoryEntry methodsFor: 'initialize-release' stamp: 'mu 3/15/2007 18:59'!
refresh
	super refresh.
	self refreshChildren! !

!FmDirectoryEntry methodsFor: 'initialize-release' stamp: 'mu 3/15/2007 18:59'!
refreshChildren
	
	children := nil! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'mu 3/27/2007 23:13'!
default 
	"FmDirectoryEntry default"
	^self directoryEntryClass pathName: FmFileIOAccessor defaultDirectoryPath
	! !

!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'mu 3/14/2007 19:49'!
on: aFileDirectry 
	"FmDirectory on: FileDirectory default"
	
	| inst |
	inst := self pathName: aFileDirectry pathName.
	^ inst! !

!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'mu 6/11/2006 18:46'!
parent 
	"FmDirectory parent"
	^self default parent
	! !

!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'mu 3/14/2007 19:48'!
pathName: fullPathName

	| inst |
	inst := self new.
	inst pathName: fullPathName.
	^ inst! !

!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'mu 3/3/2007 17:13'!
root 
	"FmDirectoryEntry root"
	^'/' asDirectoryEntry! !

!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'mu 3/3/2007 10:15'!
roots
	"For the systems having notion of drives"
	"FmDirectoryEntry roots"
	^ self drives
		ifEmpty: [Array with: self root]
		ifNotEmptyDo: [:drvs | drvs
				collect: [:each | each asFileEntry]]! !

!FmDirectoryEntry class methodsFor: 'factory' stamp: 'mu 5/4/2007 22:41'!
fileEntryClass
	"override"
	^FmFileEntry! !


!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 4/6/2008 14:10'!
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

!FmFileIOAccessor methodsFor: 'actions' stamp: 'MU 3/26/2014 12:46'!
copy: fromFileFullName to: toFileFullName 
	| readStr writeStr |
	[readStr := (self readOnlyFileNamed: fromFileFullName) binary.
	writeStr := (self forceNewFileNamed: toFileFullName) binary.
	self copyFile: readStr toFile: writeStr]
		ensure: [readStr
				ifNotNil: [:r | r close].
			writeStr
				ifNotNil: [:w | w close]]! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 18:08'!
createDirectory: fullPathName
	^self primCreateDirectory: fullPathName asVmPathName! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 23:54'!
deleteDirectory: fullPathName
	^self primDeleteDirectory: fullPathName asVmPathName! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 23:12'!
deleteFile: fullPathName
	^self deleteFile: fullPathName ifAbsent: []! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 23:12'!
deleteFile: fullPathName ifAbsent: failBlock 
	^(self
			try: [self primDeleteFileNamed: fullPathName asVmPathName]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/26/2007 12:01'!
directoryExists: fullPathName

	fullPathName = self slash ifTrue: [^true].

	^self basicDirectoryExists: fullPathName
	! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 19:15'!
directoryExists: localName in: directoryFullPath

	| dirNames |
	dirNames := self directoryNamesIn: directoryFullPath.

	^self isCaseSensitive 
		ifTrue:[dirNames includes: localName]
		ifFalse:[dirNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 5/7/2007 15:56'!
fileExists: fullPathName

	| pathAndLocalName |
	pathAndLocalName := fullPathName pathAndLocalName.
	^self fileExists: pathAndLocalName last in: pathAndLocalName first! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 22:46'!
fileExists: localName in: directoryFullPath

	| fileNames |
	fileNames := self fileNamesIn: directoryFullPath.
	^self isCaseSensitive 
		ifTrue:[fileNames includes: localName]
		ifFalse:[fileNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 22:44'!
fileOrDirectoryExists: localName in: directoryFullPath

	| entryNames |
	entryNames := self entryNamesIn: directoryFullPath.

	^self isCaseSensitive 
		ifTrue:[entryNames includes: localName]
		ifFalse:[entryNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 3/14/2007 20:09'!
pathNameDelimiter
	^self primPathNameDelimiter! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 5/24/2007 13:52'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName asVmPathName to: newFileFullName asVmPathName]
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

!FmFileIOAccessor methodsFor: 'actions' stamp: 'mu 6/4/2010 02:20'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName asVmPathName to: newFileFullName asVmPathName]
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

!FmFileIOAccessor methodsFor: 'private' stamp: 'MU 3/26/2014 12:27'!
bareEntriesIn: fullPath
	"FmFileIOAccessor default bareEntriesIn: ''"
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
			ifFalse: [entries addLast: (self bareEntryFromArray: entryArray)].
		index := index + 1].

	^ entries asArray! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'mu 5/9/2010 18:14'!
bareEntryFromArray: array
	^DirectoryEntry name: (array at: 1) creationTime: (array at: 2) modificationTime: (array at: 3) isDirectory: (array at: 4) fileSize: (array at: 5) ! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'mu 4/2/2008 17:14'!
basicDirectoryExists: fullPathName

	| result |
	result := self lookupEntryIn: fullPathName index: 1.
 	^result ~= #badDirectoryPath! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'mu 4/5/2008 13:32'!
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

!FmFileIOAccessor methodsFor: 'private' stamp: 'mu 7/14/2007 20:11'!
imageDirectoryPath
	"Note that pathname is not encoded"
	| imagePath lastIndex |
	imagePath := self primImagePath.
	lastIndex := imagePath lastIndexOf: self pathNameDelimiter.
	^ imagePath copyFrom: 1 to: lastIndex! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'mu 3/14/2007 18:08'!
lookupEntryIn: fullPathName index: index
	
	^self primLookupEntryIn: fullPathName asVmPathName index: index! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'mu 3/14/2007 23:45'!
try: execBlock forFileNamed: fullName

	"If fail, return nil"

	^ (self concreteStreamClass retryWithGC: execBlock until: [:result | result notNil] forFileNamed: fullName) notNil! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 3/19/2007 17:56'!
baseNameFor: localName 
	| delim index |
	delim := self extensionDelimiter.
	index := localName
				findLast: [:c | c = delim].
	^ index = 0
		ifTrue: [localName]
		ifFalse: [localName copyFrom: 1 to: index - 1]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 4/2/2008 17:15'!
copyFile: fileStream1 toFile: fileStream2
	| buffer |
	buffer := String new: 50000.
	[fileStream1 atEnd] whileFalse:
		[fileStream2 nextPutAll: (fileStream1 nextInto: buffer)].
! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 3/14/2007 19:20'!
directoryNamesIn: fullPath
	"FmFileIOAccessor default directoryNamesIn: 'D:\lang\Smalltalk\squeak\000Scallion'"
	
	^((self bareEntriesIn: fullPath) select: [:each | each isDirectory]) collect: [:each | each name]
	! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 4/4/2008 18:45'!
encode: encoderName on: externalStream 
	encoderName ifNil: [^self]. "UTF-8 will be used"
	externalStream
		converter: (TextConverter newForEncoding: encoderName)! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 3/14/2007 22:43'!
entryNamesIn: fullPath
	"FmFileIOAccessor default entryNamesIn: 'D:\lang\Smalltalk\squeak\000Scallion'"
	
	^(self bareEntriesIn: fullPath) collect: [:each | each name]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 3/19/2007 17:57'!
extensionFor: localName 
	| delim index |
	delim := self extensionDelimiter.
	index := localName
				findLast: [:c | c = delim].
	^ index = 0
		ifTrue: ['']
		ifFalse: [localName copyFrom: index + 1 to: localName size]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 3/14/2007 22:46'!
fileNamesIn: fullPath
	"FmFileAccessor default fileNamesIn: 'D:\lang\Smalltalk\squeak\000Scallion'"
	
	^((self bareEntriesIn: fullPath) reject: [:each | each isDirectory]) collect: [:each | each name]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'mu 4/2/2008 17:15'!
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

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'mu 3/13/2007 12:05'!
concreteStreamClass
	^FileStream concreteStream! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'mu 3/14/2007 20:16'!
currentDriveName
	^self primImagePath asDriveName! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'mu 3/26/2013 01:33'!
dialectUtilClass
	^self class dialectUtilClass! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'mu 3/14/2007 19:21'!
drives
	self onUnix ifTrue: [^#()].
	^(self bareEntriesIn: '') collect: [:each | each name]! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'mu 3/19/2007 17:56'!
extensionDelimiter
	"Return the character used to delimit filename extensions on this platform. Most platforms use the period (.) character."

	^ $.
! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'mu 3/14/2007 20:09'!
slash
	^self pathNameDelimiter asString! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'mu 3/27/2007 23:58'!
fileNamed: fullPathName
	"Open the file with the given name in this directory for writing."

	^ self concreteStreamClass fileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'mu 3/13/2007 12:09'!
forceNewFileNamed: fullPathName
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	^ self concreteStreamClass forceNewFileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'mu 3/27/2007 23:58'!
newFileNamed: fullPathName
	"Create a new file with the given name in this directory."

	^ self concreteStreamClass newFileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'mu 3/13/2007 12:05'!
readOnlyFileNamed: fullPathName
	"Open the existing file with the given name in this directory for read-only access."

	^ self concreteStreamClass readOnlyFileNamed: fullPathName
! !

!FmFileIOAccessor methodsFor: 'testing' stamp: 'mu 3/14/2007 22:07'!
isCaseSensitive
	"FmFileIOAccessor default isCaseSensitive"
	^self onUnix! !

!FmFileIOAccessor methodsFor: 'testing' stamp: 'mu 3/13/2007 21:40'!
isDriveSupported
	^self onWindows or: [self onMac]! !

!FmFileIOAccessor methodsFor: 'testing' stamp: 'mu 3/14/2007 20:08'!
onMac
	^self pathNameDelimiter = $:! !

!FmFileIOAccessor methodsFor: 'testing' stamp: 'mu 3/14/2007 20:08'!
onUnix
	^self pathNameDelimiter = $/! !

!FmFileIOAccessor methodsFor: 'testing' stamp: 'mu 3/14/2007 20:08'!
onWindows
	^self pathNameDelimiter = $\! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:11'!
primCreateDirectory: fullPath
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed
! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:11'!
primDeleteDirectory: fullPath
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:11'!
primDeleteFileNamed: aFileName
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'mu 3/14/2007 19:27'!
primImagePath

	<primitive: 121>
	self primitiveFailed! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:12'!
primLookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	The empty string enumerates the top-level files or drives. (For example, on Unix, the empty path enumerates the contents of '/'. On Macs and PCs, it enumerates the mounted volumes/drives.)

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'mu 3/14/2007 19:51'!
primPathNameDelimiter
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed
! !

!FmFileIOAccessor methodsFor: 'primitives' stamp: 'mu 3/13/2007 00:12'!
primRename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil! !


!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'mu 4/25/2013 23:32'!
beDefault
	| oldDefault |
	self = FmFileIOAccessor ifTrue: [^self]. "This method is for a subclass"
	oldDefault := FmFileIOAccessor default.
	oldDefault notNil ifTrue: [oldDefault dialectUtilClass removeFromStartUpList: oldDefault dialectUtilClass].
	Default := self new.
	FmFileIOAccessor default dialectUtilClass addToStartUpList: Default dialectUtilClass! !

!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'mu 3/26/2013 01:34'!
initDefault
	Default := nil! !

!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'mu 3/24/2013 00:48'!
initDialectUtilClass
	dialectUtilClass := nil.! !

!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'mu 3/19/2013 17:42'!
initLibrarySpecificClass
	librarySpecificClass := nil.! !

!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'mu 3/27/2013 23:32'!
initialize
	"FmFileIOAccessor initialize"
	self initLibrarySpecificClass.
	self initDialectUtilClass.
	self initDefault.! !

!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'mu 3/24/2013 00:49'!
obsolete
	self initDefault.
	self initLibrarySpecificClass.
	self initDialectUtilClass.
	self dialectUtilClass removeFromStartUpList: self.
	super obsolete.! !

!FmFileIOAccessor class methodsFor: 'instance creation' stamp: 'mu 3/26/2013 01:43'!
default
	Default isNil
		ifTrue: [| fileLibClass |
			fileLibClass := self librarySpecificClass.
			fileLibClass notNil
				ifTrue: [Default := fileLibClass new]].
	^ Default! !

!FmFileIOAccessor class methodsFor: 'accessing' stamp: 'mu 3/14/2007 22:23'!
defaultDirectoryPath
	^self default defaultDirectoryPath! !

!FmFileIOAccessor class methodsFor: 'accessing' stamp: 'mu 3/24/2013 00:37'!
dialectUtilClass
	^ dialectUtilClass
		ifNil: [| subclasses |
			subclasses := FmPortableUtil subclasses.
			subclasses notEmpty
				ifTrue: [dialectUtilClass := subclasses at: 1]]! !

!FmFileIOAccessor class methodsFor: 'accessing' stamp: 'mu 3/24/2013 00:38'!
dialectUtilClass: aDialectSpecificPortableUtil
	dialectUtilClass := aDialectSpecificPortableUtil! !

!FmFileIOAccessor class methodsFor: 'accessing' stamp: 'mu 3/19/2013 17:46'!
librarySpecificClass
	^ librarySpecificClass
		ifNil: [| subclasses |
			subclasses := self subclasses.
			subclasses notEmpty
				ifTrue: [librarySpecificClass := subclasses at: 1]]! !

!FmFileIOAccessor class methodsFor: 'accessing' stamp: 'mu 3/19/2013 17:44'!
librarySpecificClass: aLibrarySpecificFileIOAccessor
	librarySpecificClass := aLibrarySpecificFileIOAccessor! !

!FmFileIOAccessor class methodsFor: 'accessing exception' stamp: 'mu 3/24/2013 00:48'!
fileDoesNotExistException
	^self dialectUtilClass fileDoesNotExistException! !

!FmFileIOAccessor class methodsFor: 'events' stamp: 'mu 3/26/2013 01:29'!
onSystemStartUp
	self initDefault.
	FmFileEntry onSystemStartUp! !


!FmPortableUtil class methodsFor: 'actions' stamp: 'mu 3/26/2013 01:19'!
addToStartUpList: aClass
	self subclassResponsibility! !

!FmPortableUtil class methodsFor: 'actions' stamp: 'mu 3/26/2013 01:19'!
removeFromStartUpList: aClass
	self subclassResponsibility! !

!FmPortableUtil class methodsFor: 'accessing' stamp: 'mu 3/24/2013 00:47'!
fileDoesNotExistException
	^nil! !


!Character methodsFor: '*fileman-core-testing' stamp: 'mu 3/2/2007 21:34'!
isDriveLetter
	^'ABCDEFGHIJKLMNOPQRSTUVWXYZ' includes: self asUppercase
	! !

!Character methodsFor: '*fileman-core-testing' stamp: 'mu 3/3/2007 09:45'!
isDriveSeparator
	^self == $:
	! !

!Character methodsFor: '*fileman-core-testing' stamp: 'mu 3/3/2007 19:37'!
isPathSeparator
	self = $\ ifTrue: [^true].
	self = $: ifTrue: [^true].
	self = $/ ifTrue: [^true].
	^false! !

FmFileIOAccessor initialize!
