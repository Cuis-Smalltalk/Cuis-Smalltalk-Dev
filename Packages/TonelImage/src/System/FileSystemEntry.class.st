"
I represent a single file entry (including directory).

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
	#name : 'FileSystemEntry',
	#superclass : 'Object',
	#instVars : [
		'drive',
		'pathComponents',
		'parent',
		'name',
		'primEntryInParent',
		'lastSync',
		'exists',
		'creationTime',
		'modificationTime'
	],
	#category : 'System-File System',
	#package : 'System',
	#tag : 'File System'
}

{ #category : 'class state access' }
FileSystemEntry class >> releaseClassCachedState [

	self allSubInstancesDo: [ :each | each invalidate]
]

{ #category : 'instance creation' }
FileSystemEntry class >> withAbsolutePathName: aString [
	"May exist or not
	aString must be an absolute path name"

	| instance |
	instance := self new.
	instance absolutePathString: aString.
	^instance
]

{ #category : 'instance creation' }
FileSystemEntry class >> withPathComponents: comps drive: driveString [
	"May exist or not"
	| instance |
	instance := self new.
	instance pathComponents: comps drive: driveString.
	^instance
]

{ #category : 'instance creation' }
FileSystemEntry class >> withPathName: aString [
	"Note: It is generally advisable to call one or the more specific messages:
	#withAbsolutePathName: or #withRelativePathName:to:"

	^aString isAbsolutePathName
		ifTrue: [ self withAbsolutePathName: aString ]
		ifFalse: [
			self
				withRelativePathName: aString
				to: DirectoryEntry userBaseDirectory ].
]

{ #category : 'instance creation' }
FileSystemEntry class >> withRelativePathName: aString to: aDirectoryEntry [
	"May exist or not.
	aString must be a relative path name"

	| instance |
	instance := self new.
	instance relativePathString: aString to: aDirectoryEntry.
	^instance
]

{ #category : 'comparing' }
FileSystemEntry >> = aFileEntry [
	| isCaseSensitive myDrive otherDrive theirs mine |

	self == aFileEntry ifTrue: [ ^ true ].
	self class == aFileEntry class ifFalse: [ ^false ].

	isCaseSensitive := self fileAccessor isCaseSensitive.

	"Check for drive nil or same."
	myDrive := self drive.
	otherDrive := aFileEntry drive.
	isCaseSensitive
		ifTrue: [ self drive = aFileEntry drive ifFalse: [ ^false ]]
		ifFalse: [
			myDrive isNil = otherDrive isNil ifFalse: [ ^false ].		"only one of them is nil"
			myDrive ifNotNil: [											"none is nil"
				(myDrive sameAs: otherDrive) ifFalse: [ ^false ]]].

	"Check for all path components same."
	mine := self pathComponents.
	theirs := aFileEntry pathComponents.

	isCaseSensitive ifTrue: [
		^mine = theirs ].

	mine size = theirs size ifFalse: [
		^false ].

	^mine 
		with: theirs 
		allSatisfy: [ :m :t | m sameAs: t ]
]

{ #category : 'initialize-release' }
FileSystemEntry >> absolutePathString: aString [
	"Argument must be an absolute path"

	^self pathTokens: (FileIOAccessor default expandTildeFor: aString) asPathTokens.
]

{ #category : 'accessing' }
FileSystemEntry >> allParents [
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord
]

{ #category : 'accessing-file name' }
FileSystemEntry >> baseName [
	^self fileAccessor baseNameFor: name
]

{ #category : 'accessing' }
FileSystemEntry >> commonPathWith: aFileSystemEntry [
	"
	DirectoryEntry currentDirectory commonPathWith: DirectoryEntry projectBaseDirectory
	"
	| i lastI theirPathComponents |
	self drive = aFileSystemEntry drive
		ifFalse: [ ^nil ].
	i := 1.
	theirPathComponents := aFileSystemEntry pathComponents.
	lastI := pathComponents size min: theirPathComponents size.
	[i <= lastI and: [(pathComponents at: i) = (theirPathComponents at: i)]] whileTrue: [
		i := i + 1 ].
	^DirectoryEntry
				withPathComponents: (pathComponents copyFrom: 1 to: i-1)
				drive: self drive
		
]

{ #category : 'accessing' }
FileSystemEntry >> creationTime [
	self invalidateIfOld.
	creationTime ifNil: [self fileAccessor updateEntry: self].
	^creationTime
]

{ #category : 'accessing' }
FileSystemEntry >> drive [
	self fileAccessor onUnix ifTrue: [^ drive := nil].
	^ drive
]

{ #category : 'private' }
FileSystemEntry >> ensureParent [
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent := DirectoryEntry
				withPathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent
]

{ #category : 'accessing' }
FileSystemEntry >> exists [
	self invalidateIfOld.
	exists ifNil: [self updateExists].
	^exists
]

{ #category : 'testing' }
FileSystemEntry >> existsAsDirectory [
	^FileIOAccessor default isDirectory: self pathName
]

{ #category : 'testing' }
FileSystemEntry >> existsAsFile [
	^FileIOAccessor default isFile: self pathName
]

{ #category : 'accessing-file name' }
FileSystemEntry >> extension [
	^self fileAccessor extensionFor: name
]

{ #category : 'private' }
FileSystemEntry >> fileAccessor [

	^FileIOAccessor default
]

{ #category : 'comparing' }
FileSystemEntry >> hash [
	^self pathComponents hash
]

{ #category : 'convenience' }
FileSystemEntry >> ifExists: aBlock [
	"Evaluate a block with receiver as argument if it exists on the file system. If not, do nothing."
	self exists ifTrue: [
		aBlock value: self ]
]

{ #category : 'initialize-release' }
FileSystemEntry >> invalidate [
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	lastSync := nil.
	exists := nil.
	creationTime := nil.
	modificationTime := nil.
]

{ #category : 'private' }
FileSystemEntry >> invalidateIfOld [

	lastSync isNil ifTrue: [
		^ self invalidate ].
	(DateAndTime now - lastSync) totalSeconds > 2 ifTrue: [
		self invalidate ]
]

{ #category : 'testing' }
FileSystemEntry >> isDirectoryEntry [
	^false
]

{ #category : 'testing' }
FileSystemEntry >> isFileEntry [
	^false
]

{ #category : 'testing' }
FileSystemEntry >> isReadOnly [
	^FileIOAccessor default isReadOnly: self pathName
]

{ #category : 'testing' }
FileSystemEntry >> isReadable [
	^FileIOAccessor default isReadable: self pathName
]

{ #category : 'testing' }
FileSystemEntry >> isRoot [
	^self parent isNil
]

{ #category : 'testing' }
FileSystemEntry >> isWriteable [
	^FileIOAccessor default isWriteable: self pathName
]

{ #category : 'attributes' }
FileSystemEntry >> makeReadOnly [
	^FileIOAccessor default makeReadOnly: self pathName
]

{ #category : 'attributes' }
FileSystemEntry >> makeReadWrite [
	^FileIOAccessor default makeReadWrite: self pathName
]

{ #category : 'accessing' }
FileSystemEntry >> modificationTime [
	self invalidateIfOld.
	modificationTime ifNil: [self fileAccessor updateEntry: self].
	^modificationTime
]

{ #category : 'accessing' }
FileSystemEntry >> name [
	^name ifNil: [ drive ]
]

{ #category : 'accessing' }
FileSystemEntry >> name: aString  [
	name := aString.
	self pathComponents
		ifNotEmpty: [self pathComponents at: self pathComponents size put: name]
]

{ #category : 'accessing' }
FileSystemEntry >> name: aName parent: parentEntry [

	name := aName.
	parent := parentEntry.
	drive := parentEntry drive. "harmless if no drive supported, as in Unix"
	pathComponents := parentEntry pathComponents copyWith: name.
	self invalidate
]

{ #category : 'accessing-file name' }
FileSystemEntry >> nameVersionExtension [
	^self fileAccessor splitNameVersionExtensionFor: self name
]

{ #category : 'accessing-file name' }
FileSystemEntry >> nameWithoutExtension [
	"
	'writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings' asFileEntry nameWithoutExtension
	"
	^self fileAccessor baseNameFor: name
]

{ #category : 'accessing' }
FileSystemEntry >> parent [
	parent ifNil: [parent := self ensureParent].
	^parent
]

{ #category : 'accessing' }
FileSystemEntry >> pathComponents [

	^ pathComponents
]

{ #category : 'initialize-release' }
FileSystemEntry >> pathComponents: tokens drive: driveStringOrNil [
	| firstToken |
	drive := driveStringOrNil.
	pathComponents := tokens asArray.
	(driveStringOrNil isNil and: [
		pathComponents notEmpty and: [ (firstToken := pathComponents first) isDriveName ]]) ifTrue: [
			drive := firstToken.
			pathComponents := pathComponents allButFirst ].
	pathComponents ifNotEmpty: [ name := pathComponents last ].
	self invalidate.
]

{ #category : 'accessing' }
FileSystemEntry >> pathName [

	^ String streamContents: [ :stream |
		self printPathOn: stream ]
]

{ #category : 'initialize-release' }
FileSystemEntry >> pathTokens: tokens [
	"Argument can be an absolute or relative path"

	(tokens notEmpty and: [self fileAccessor isDriveSupported])
		 ifTrue: [
			tokens first asDriveName ifNotNil: [ :guessedDriveName |
				^ self pathComponents: (tokens copyFrom: 2 to: tokens size) drive: guessedDriveName ]].
	self pathComponents: tokens drive: nil.
]

{ #category : 'printing' }
FileSystemEntry >> printOn: aStream  [
	self printPathOn: aStream
]

{ #category : 'printing' }
FileSystemEntry >> printPathOn: aStream  [
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPutAll: self fileAccessor slash.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPutAll: self fileAccessor slash]
]

{ #category : 'initialize-release' }
FileSystemEntry >> relativePathString: aString to: aDirectoryEntry [
	"Argument must be a relative path"

	^self pathTokens: (FileIOAccessor default
		absolutePathComponentsFor: aString
		relativeTo: aDirectoryEntry).
]

{ #category : 'private' }
FileSystemEntry >> setContentsOf: aStream to: aStringOrBytes [

	aStringOrBytes isString
		ifFalse: [ aStream binary].
	aStream nextPutAll: aStringOrBytes
]

{ #category : 'private' }
FileSystemEntry >> setParent: aDirectoryEntry [
	parent := aDirectoryEntry
]

{ #category : 'testing' }
FileSystemEntry >> updateExists [

	self subclassResponsibility
]

{ #category : 'private' }
FileSystemEntry >> updateFrom: primitiveArray entryInParent: index [
	primEntryInParent := index.
	lastSync := DateAndTime now.
	exists := true.
	creationTime := DateAndTime fromSeconds: (primitiveArray at: 2).
	modificationTime := DateAndTime fromSeconds: (primitiveArray at: 3)
]
