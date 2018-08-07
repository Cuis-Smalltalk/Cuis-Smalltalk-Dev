'From Cuis 5.0 of 7 November 2016 [latest update: #3386] on 1 August 2018 at 5:32:47 pm'!

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 8/1/2018 17:11:06'!
updateEntry: aFileSystemEntry
	| entryArray index lookIn isDirectory |

	"If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries."
	aFileSystemEntry primEntryInParent ifNotNil: [ :tentativeIndex |
		(self primLookupEntryIn: aFileSystemEntry parent pathName index: tentativeIndex) ifNotNil: [ :found |
	 		found == #badDirectoryPath ifFalse: [
				aFileSystemEntry name = (found at: 1) ifTrue: [
					aFileSystemEntry updateFrom: found entryInParent: tentativeIndex.
					^ self ]]]].

	"Otherwise, do a full iteration"
	lookIn _ aFileSystemEntry parent pathName.
	index _ 1.
	[
		entryArray _ self primLookupEntryIn: lookIn index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ self].
		entryArray == nil ifTrue: [
			^ self].
		isDirectory _ entryArray at: 4.
		aFileSystemEntry name = (entryArray at: 1) ifTrue: [
			isDirectory == aFileSystemEntry isDirectory ifTrue: [
				aFileSystemEntry updateFrom: entryArray entryInParent: index ].
			"If found, exit even if invalid. No point to keep iterating."
			^ self ].
		index _ index + 1] repeat! !


!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 16:56:17'!
exists
	self invalidateIfOld.
	exists ifNil: [self updateExists].
	^exists! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 16:28:04'!
invalidateIfOld

	lastSync isNil ifTrue: [
		^ self invalidate ].
	(DateAndTime now - lastSync) totalSeconds > 2 ifTrue: [
		self invalidate ]! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:27:06'!
invalidate
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	lastSync _ nil.
	exists _ nil.
	creationTime _ nil.
	modificationTime _ nil.! !


!DirectoryEntry methodsFor: 'testing' stamp: 'jmv 8/1/2018 16:55:24'!
updateExists
	
	| pathName |
	(self fileAccessor isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^ exists _ self fileAccessor drives includes: self ].

	self isRoot ifTrue: [ ^ exists _ true ].
	
	pathName _ self pathName.
	pathName = self fileAccessor slash ifTrue: [ ^ exists _ true ].

	exists _ self fileAccessor basicDirectoryExists: pathName! !

!DirectoryEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:31:30'!
invalidate
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	super invalidate.
	self invalidateChildren! !

!DirectoryEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:31:20'!
invalidateChildren
	
	children _ nil! !


!FileEntry methodsFor: 'testing' stamp: 'jmv 8/1/2018 16:55:46'!
updateExists

	self fileSize "Updates both"! !

!FileEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:27:14'!
invalidate
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	super invalidate.
	fileSize _ nil! !


!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 17:13:59'!
creationTime
	self invalidateIfOld.
	creationTime ifNil: [self fileAccessor updateEntry: self].
	^creationTime! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 17:01:34'!
modificationTime
	self invalidateIfOld.
	modificationTime ifNil: [self fileAccessor updateEntry: self].
	^modificationTime! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 16:27:50'!
name: aString parent: parentEntryOrNil
	name _ aString.
	parentEntryOrNil
		ifNil: [
			self pathString: aString ]
		ifNotNil: [
			parent _ parentEntryOrNil.
			drive _ parentEntryOrNil drive. "harmless if no drive supported, as in Unix"
			pathComponents _ parentEntryOrNil pathComponents copyWith: name ].
	self invalidate! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:27:53'!
pathComponents: tokens drive: driveStringOrNil
	| firstToken  |
	tokens isEmptyOrNil ifTrue: [ ^pathComponents _ nil ].
	(driveStringOrNil isNil and: [ (firstToken _ tokens first) isDriveName])
		ifTrue: [
			self drive: firstToken.
			pathComponents _ tokens copyFrom: 2 to: tokens size ]
		ifFalse: [
			self drive: driveStringOrNil.
			pathComponents _ tokens ].

	pathComponents ifNotEmpty: [ name _ pathComponents last ].
	self invalidate! !


!FileSystemEntry class methodsFor: 'class state access' stamp: 'jmv 8/1/2018 16:28:10'!
releaseClassCachedState

	self allSubInstancesDo: [ :each | each invalidate]! !


!DirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 8/1/2018 16:31:28'!
delete
	self fileAccessor deleteDirectory: self pathName.
	self invalidateChildren! !

!DirectoryEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 16:31:26'!
basicRecursiveDelete
	self invalidateChildren.
	self directoriesDo: [:dir | dir basicRecursiveDelete].
	self filesDo: [:file | file delete].
	self delete! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 8/1/2018 16:31:24'!
at: localFileName put: contents

	(self // localFileName) forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: contents ].
	self invalidateChildren.
	^contents! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 8/1/2018 16:31:32'!
removeKey: localFileName ifAbsent: failBlock
	self fileAccessor deleteFile: (self // localFileName) pathName ifAbsent: [^failBlock value].
	self invalidateChildren.! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 16:45:23'!
children
	self invalidateIfOld.
	children ifNil: [self initChildren].
	^children! !


!FileEntry methodsFor: 'actions-file' stamp: 'jmv 8/1/2018 16:27:36'!
binaryContents: aByteArray
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream binary to: aByteArray ].
	self invalidate! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 8/1/2018 16:27:44'!
textContents: aString
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: aString ].
	self invalidate! !

!FileEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 17:02:21'!
fileSize

	self invalidateIfOld.

	"Slow version."
	"fileSize ifNil: [self fileAccessor updateEntry: self]."

	"Fast version, that asks just for the size of this file.
	Used if I was not created by reading a direcotry"
	fileSize ifNil: [
		fileSize _ self fileAccessor fileSize: self.
		exists _ fileSize notNil ].

	^fileSize! !

!FileEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 16:27:47'!
writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateNewFile: self! !

!FileEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 16:27:41'!
forceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateForceNewFile: self! !

!methodRemoval: FileEntry #exists!
FileEntry removeSelector: #exists!
!methodRemoval: FileEntry #fileSize:!
FileEntry removeSelector: #fileSize:!
!methodRemoval: FileEntry #initValuesFrom:!
FileEntry removeSelector: #initValuesFrom:!
!methodRemoval: FileEntry #refresh!
FileEntry removeSelector: #refresh!
!methodRemoval: DirectoryEntry #exists!
DirectoryEntry removeSelector: #exists!
!methodRemoval: DirectoryEntry #refresh!
DirectoryEntry removeSelector: #refresh!
!methodRemoval: DirectoryEntry #refreshChildren!
DirectoryEntry removeSelector: #refreshChildren!
!methodRemoval: FileSystemEntry #creationTime:!
FileSystemEntry removeSelector: #creationTime:!
!methodRemoval: FileSystemEntry #initValuesFrom:!
FileSystemEntry removeSelector: #initValuesFrom:!
!methodRemoval: FileSystemEntry #initValuesFromParent!
FileSystemEntry removeSelector: #initValuesFromParent!
!methodRemoval: FileSystemEntry #modificationTime:!
FileSystemEntry removeSelector: #modificationTime:!
!methodRemoval: FileSystemEntry #refresh!
FileSystemEntry removeSelector: #refresh!
!methodRemoval: FileIOAccessor #fileExists:!
FileIOAccessor removeSelector: #fileExists:!
