"
I represent a single file entry.

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See DirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFullFileEntry fileSize
	Smalltalk imageName asFullFileEntry parent directories do: [ :a | a print ]
"
Class {
	#name : 'FileEntry',
	#superclass : 'FileSystemEntry',
	#instVars : [
		'fileSize'
	],
	#category : 'System-File System',
	#package : 'System',
	#tag : 'File System'
}

{ #category : 'examples' }
FileEntry class >> example1 [
	"FileEntry example1"

	"- Create subDirectory named: 'subDir'.
	 - Put a new file named: 'file1'.
	 - Write contents 'Hello! to that file'"

	"Traditional way (Squeak)"
	"| subDir str |
	subDir := FileDirectory smalltalkImageDirectory directoryNamed: 'subDir'.
	subDir assureExistence.
	[str := subDir newFileNamed: 'file1'.
	str nextPutAll: 'Hello!']
		ensure: [str close]."

	"FileMan"

	'./subDir' asDirectoryEntry at: 'file2' put: 'Hello!'
]

{ #category : 'examples' }
FileEntry class >> example2 [
	"
	FileEntry example2
	"

	"FileMan's path representation is portable"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') at: 'test1' put: 'Hello2!'.
]

{ #category : 'examples' }
FileEntry class >> example3 [
	"FileEntry example3"

	"Remove 'test1' file created example2"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') removeKey: 'test1'.

	"Recursive delete"
	'./subDir' asDirectoryEntry recursiveDelete
]

{ #category : 'examples' }
FileEntry class >> example5 [
	"FileEntry example5"
	
	"Write test1 file and copy it to test2 in the parent directory"

	'./test1' fileContents: 'This is a test'.
	'./test1' asFileEntry copyTo: '../test2'.

	'../test2' fileContents inspect
]

{ #category : 'examples' }
FileEntry class >> example6 [
	"FileEntry example6"
	
	"test1 file contents will be written to test2 using reverse filter.
	 test2 file contents will be written to test3 again using reverse filter."

	| reverseFilter |
	'test1.txt' fileContents: 'This is a test'.
	reverseFilter := [:in :out | out nextPutAll: (in upToEnd reverse)].

	('test1.txt' asFileEntry pipe: reverseFilter to: 'test2.txt')
		pipe: reverseFilter to: 'test3.txt'.

	(#('test1.txt' 'test2.txt' 'test3.txt') collect: [:each | each fileContents]) inspect

	
]

{ #category : 'examples' }
FileEntry class >> example7 [
	"FileEntry example7"
	"Useful example: copy latest mcz files to releaseDir (for preparing SAR)"

	| releaseDir dir fileNames |
	releaseDir := './releasePkg' asDirectoryEntry.
	releaseDir keys isEmpty ifFalse: [releaseDir recursiveDelete].
	dir := './package-cache' asDirectoryEntry.
	fileNames := ((dir keys collect: [:each | each name readStream upTo: $.]) select: [:each | each notEmpty]) asSet.
	(fileNames collect: [:eachName | dir
				latestFileMatches: [:each | each name beginsWith: eachName]])
		do: [:eachEnt | eachEnt copyTo: releaseDir]
]

{ #category : 'actions-rio' }
FileEntry >> < aStringOrBytes  [
	self fileContents: aStringOrBytes
]

{ #category : 'actions-rio' }
FileEntry >> << aStringOrBytes  [
	self appendContents: aStringOrBytes
]

{ #category : 'actions-file' }
FileEntry >> appendContents: aStringOrBytes  [
	self
		appendStreamDo: [:str | 
			aStringOrBytes isString
				ifFalse: [str binary].
			str nextPutAll: aStringOrBytes]
]

{ #category : 'accessing' }
FileEntry >> appendStream [
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamDo: that closes the file for you."

	self invalidate.
	self exists ifFalse: [
		^ self writeStream ].
	^ (self fileAccessor privateWriteableFile: self) setToEnd
]

{ #category : 'accessing-stream' }
FileEntry >> appendStreamDo: blockWithArg  [
	| stream |
	stream := self appendStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ].
			self invalidate ].

]

{ #category : 'convenience' }
FileEntry >> asFileEntry [
	"For convenience."
	^self
]

{ #category : 'enumeration' }
FileEntry >> assureExistence [
	self exists ifTrue: [^self].
	self parent assureExistence.
	self forceWriteStreamDo: [ :stream | ]
]

{ #category : 'actions-file' }
FileEntry >> binaryContents [
	^self readStreamDo: [ :stream |
		stream binary contents ].
]

{ #category : 'actions-file' }
FileEntry >> binaryContents: aByteArray [
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream binary to: aByteArray ].
]

{ #category : 'actions-file' }
FileEntry >> copyTo: filename  [
	| targetEntry |
	
	targetEntry := filename asFileEntry.
	targetEntry existsAsDirectory
		ifTrue: [
			targetEntry := filename asDirectoryEntry // self name ].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry
]

{ #category : 'actions-file' }
FileEntry >> delete [
	self fileAccessor deleteFile: self pathName.
	self parent invalidateChildren.
	self invalidate.
]

{ #category : 'actions-file' }
FileEntry >> fileContents [
	"Default is text mode"
	^self textContents
]

{ #category : 'actions-file' }
FileEntry >> fileContents: aStringOrBytes  [
	aStringOrBytes isString
		ifTrue: [self textContents: aStringOrBytes]
		ifFalse: [self binaryContents: aStringOrBytes]
]

{ #category : 'accessing' }
FileEntry >> fileSize [

	self invalidateIfOld.

	"Slow version."
	"fileSize ifNil: [self fileAccessor updateEntry: self]."

	"Fast version, that asks just for the size of this file.
	Used if I was not created by reading a directory"
	fileSize ifNil: [
		fileSize := self fileAccessor fileSize: self.
		exists := fileSize notNil ].

	^fileSize
]

{ #category : 'private' }
FileEntry >> forceWriteStream [
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateForceNewFile: self
]

{ #category : 'accessing-stream' }
FileEntry >> forceWriteStreamDo: blockWithArg  [
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream := self forceWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ].
			self invalidate ].

]

{ #category : 'actions-file' }
FileEntry >> formContents [
	^Form fromFileEntry: self
]

{ #category : 'initialize-release' }
FileEntry >> invalidate [
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	super invalidate.
	fileSize := nil
]

{ #category : 'testing' }
FileEntry >> isFileEntry [
	^true
]

{ #category : 'actions-pipe' }
FileEntry >> pipe: filterBlock to: filename  [
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	filterBlock value: inStream value: outStream]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry

]

{ #category : 'actions-pipe' }
FileEntry >> pipeRepeat: filterBlock to: filename  [
	^self pipeRepeat: filterBlock while: [:in :out | in atEnd not] to: filename 
]

{ #category : 'actions-pipe' }
FileEntry >> pipeRepeat: filterBlock while: terminateBlock to: filename  [
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	[terminateBlock value: inStream value: outStream]
		whileTrue: [filterBlock value: inStream value: outStream]]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry
]

{ #category : 'private' }
FileEntry >> primEntryInParent [
	^ primEntryInParent
]

{ #category : 'accessing' }
FileEntry >> readStream [
	"Raise FileDoesNotExistException if not found.
	Note: You need to eventually close the stream.
	Usually prefer #readStreamDo: that closes the file for you."

	^ self fileAccessor privateReadOnlyFile: self
]

{ #category : 'accessing-stream' }
FileEntry >> readStreamDo: blockWithArg  [
	"Raise FileDoesNotExistException if not found."
	| stream |
	stream := self readStream.
	^ [ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]
]

{ #category : 'actions-file' }
FileEntry >> rename: newName [

	| fileEntry directoryEntry |
	fileEntry := self parent // newName.
	fileEntry exists
		ifTrue: [ Error signal: 'There already exists a file named: ', fileEntry printString ].
	directoryEntry := self parent / newName.
	directoryEntry exists
		ifTrue: [ Error signal: 'There already exists a directory named: ', fileEntry printString ].
	self fileAccessor rename: self pathName to: fileEntry pathName.
	self name: newName.
	self parent invalidateChildren.
	self invalidate.
]

{ #category : 'actions-file' }
FileEntry >> textContents [
	^self readStreamDo: [ :stream |
		stream contents ].
]

{ #category : 'actions-file' }
FileEntry >> textContents: aString [
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: aString ].
]

{ #category : 'testing' }
FileEntry >> updateExists [

	self fileSize "Updates both"
]

{ #category : 'private' }
FileEntry >> updateFrom: primitiveArray entryInParent: index [
	super updateFrom: primitiveArray entryInParent: index.
	fileSize := primitiveArray at: 5
]

{ #category : 'accessing' }
FileEntry >> writeStream [
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist.
	Note: Answer nil if for whatever reason the file can not be created.
	"

	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateNewFile: self
]

{ #category : 'accessing-stream' }
FileEntry >> writeStreamDo: blockWithArg  [
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist.
	Note: If file already exists and user selects 'cancel', the block is passed nil as argument!"
	| stream |
	stream := self writeStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ].
			self invalidate ].

]
