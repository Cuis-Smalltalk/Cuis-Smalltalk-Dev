"
I am an accessor to the low level file IO.

You can extend/rewrite me if you port FileMan to other Smalltalk dialects.

---
mu 3/13/2007 11:11
"
Class {
	#name : 'FileIOAccessor',
	#superclass : 'Object',
	#instVars : [
		'slash',
		'drives'
	],
	#classVars : [
		'Default'
	],
	#category : 'System-File System',
	#package : 'System',
	#tag : 'File System'
}

{ #category : 'instance creation' }
FileIOAccessor class >> default [
	Default isNil
		ifTrue: [
			Default := self new].
	^ Default
]

{ #category : 'class initialization' }
FileIOAccessor class >> initialize [
	"
	FileIOAccessor initialize
	"
	Default := nil
]

{ #category : 'cached state access' }
FileIOAccessor class >> releaseClassCachedState [
	Default := nil
]

{ #category : 'utilities' }
FileIOAccessor >> absolutePathComponentsFor: aString relativeTo: baseDirectory [
	"aString is a relative path, and will be applied to baseDirectory
	See comment at #isAbsolutePathName"

	| baseDirectoryPathComponents tokens |

	baseDirectoryPathComponents := baseDirectory pathComponents.
	aString = '.' ifTrue: [ ^ baseDirectoryPathComponents copy ].
	aString = '..' ifTrue:  [^ baseDirectoryPathComponents allButLast ].

	tokens := aString asPathTokens.
	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		baseDirectoryPathComponents := baseDirectoryPathComponents allButLast.
		tokens removeFirst ].

	^ Array streamContents: [ :strm |
		baseDirectory drive ifNotNil: [ :drive |
			strm nextPut: drive ].
		strm nextPutAll: baseDirectoryPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]].
]

{ #category : 'utilities' }
FileIOAccessor >> baseNameAndExtensionFor: pathName do: aBlock [
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis."
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim.blam') pathName)
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim') pathName)
		should be the same and NOT  'foo'
		
		Oh, and
		FileIOAccessor default baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	| extension |
	extension := self extensionFor: pathName.
	extension isEmpty ifTrue: [
		^ aBlock value: pathName value: '' ].
	^ aBlock value: (pathName copyFrom: 1 to: pathName size - extension size - 1) value: extension
]

{ #category : 'utilities' }
FileIOAccessor >> baseNameFor: pathName [
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis."
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim.blam') pathName)
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim') pathName)
		should be the same and NOT  'foo'
		
		Oh, and
		FileIOAccessor default baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	self baseNameAndExtensionFor: pathName do: [ :baseName :extension |
		^baseName ]
]

{ #category : 'private' }
FileIOAccessor >> basicDirectoryExists: fullPathName [

 	^(
		(self primLookupEntryIn: fullPathName asUtf8Bytes index: 1) 
			== #badDirectoryPath
		) not
]

{ #category : 'private' }
FileIOAccessor >> containingDirectoryPathOf: pathName  [

	(((pathName isNil
			or: [pathName isEmpty])
			or: [pathName isPathSeparator])
			or: [pathName isDriveName])
		ifTrue: [^ nil].
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1
]

{ #category : 'actions' }
FileIOAccessor >> copy: fromFileEntry to: toFileEntry  [
	| readStr writeStr |
	[readStr := (self privateReadOnlyFile: fromFileEntry) binary.
	writeStr := (self privateForceNewFile: toFileEntry) binary.
	self copyFile: readStr toFile: writeStr]
		ensure: [
			readStr
				ifNotNil: [ :r | r close ].
			writeStr
				ifNotNil: [ :w | w close ]].
	toFileEntry parent invalidateChildren.
	toFileEntry invalidate.
]

{ #category : 'utilities' }
FileIOAccessor >> copyFile: fileStream1 toFile: fileStream2 [
	| buffer |
	buffer := ByteArray new: 50000.
	[fileStream1 atEnd] whileFalse: [
		fileStream2 nextPutAll: (fileStream1 nextBytes: buffer size into: buffer startingAt: 1)].
]

{ #category : 'actions' }
FileIOAccessor >> createDirectory: fullPathName [
	self primCreateDirectory: fullPathName asUtf8Bytes
]

{ #category : 'actions' }
FileIOAccessor >> deleteDirectory: fullPathName [
	self primDeleteDirectory: fullPathName asUtf8Bytes
]

{ #category : 'actions' }
FileIOAccessor >> deleteFile: fullPathName [
	^self deleteFile: fullPathName ifAbsent: nil
]

{ #category : 'actions' }
FileIOAccessor >> deleteFile: fullPathName ifAbsent: failBlock  [
	^(self
			try: [self primDeleteFileNamed: fullPathName asUtf8Bytes]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]
]

{ #category : 'utilities' }
FileIOAccessor >> directoryNamesIn: aDirectoryEntry [
	"
	FileIOAccessor default directoryNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry)
		select: [ :each | each isDirectoryEntry]
		thenCollect: [ :each | each name]
]

{ #category : 'accessing' }
FileIOAccessor >> drives [
	"
	Answer a collection of Strings
	Meaningful only on Windows. Answers an empty array elsewhere.
	FileIOAccessor default drives
	"
	drives := nil. 		"will change if you mount or unmount drives!"
	drives ifNil: [
		drives := self onWindows
			ifTrue: [ (self entriesIn: nil)]
			ifFalse: [ #() ]].
	^drives
]

{ #category : 'private' }
FileIOAccessor >> entriesIn: parentEntry [
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.
	parentEntryOrNil can only be nil if #isDriveSupported:

Windows (parentEntry = nil means root)
FileIOAccessor default entriesIn: nil #(C:\ D:\)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (parentEntry = nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

macOS - parentEntry = nil answers an empty array
FileIOAccessor default entriesIn: nil #() .

	"
	| entries index done entryArray entry isDirectory lookIn lookForDrives |
	parentEntry isNil
		ifTrue: [
			self isDriveSupported ifFalse: [ self error: 'Only for platforms that have the concept of Disk Drive' ].
			lookIn := ''.
			lookForDrives := true ]
		ifFalse: [
			lookIn := parentEntry pathName.
			lookForDrives := false ].
	
	entries := OrderedCollection new.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self primFixedNameLookupEntryIn: lookIn asUtf8Bytes index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory := entryArray at: 4.
				entry := isDirectory ifTrue: [DirectoryEntry new] ifFalse: [FileEntry new].
				lookForDrives
					ifTrue: [entry driveName: (entryArray at: 1)]
					ifFalse: [entry name: (entryArray at: 1) parent: parentEntry].
				entry updateFrom: entryArray entryInParent: index.
				entries addLast: entry ].
		index := index + 1].

	^entries asArray
]

{ #category : 'utilities' }
FileIOAccessor >> entryNamesIn: aDirectoryEntry [
	"
	FileIOAccessor default entryNamesIn: 'C:\Windows\' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry) collect: [ :each | each name]
]

{ #category : 'utilities' }
FileIOAccessor >> exists: fullPathName [
	^self primFileExists: fullPathName asUtf8Bytes 
]

{ #category : 'utilities' }
FileIOAccessor >> expandTildeFor: aString [

	aString size = 0 ifTrue: [ ^aString ].
	aString first == $~ifFalse: [ ^aString ].
	"Basic tilde expansion. Consider $~ to mean user's home folder, if possible.
	See, for example, https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	Smalltalk at: #OSProcess
		ifPresent: [ :osProcess |
			(osProcess thisOSProcess environmentAt: 'HOME')
				ifNotNil: [ :homeVariable |
					^homeVariable, (aString copyFrom: 2 to: aString size) ]
				ifNil: [ self error: 'The $HOME variable could not be resolved' ]]
		ifAbsent: [
			self error: 'Use of tilde (~) to mean $HOME folder requires OSProcess' ]
]

{ #category : 'utilities' }
FileIOAccessor >> extensionFor: pathName  [
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.
	
	FileIOAccessor default extensionFor: 'writings.txt'
	FileIOAccessor default extensionFor: 'folder.ext/file'
	FileIOAccessor default extensionFor: 'optionalstuff.pck.st'
	FileIOAccessor default extensionFor: 'code.cs.st'
	FileIOAccessor default extensionFor: 'code.cs'
	"
	| index |
	{ '.cs.st' . '.pck.st'. '.ws.st' } do: [ :specialExtension |
		(pathName endsWith: specialExtension)
			ifTrue: [ ^specialExtension copyFrom: 2 to: specialExtension size ]].
	index := pathName
				findLast: [ :c | c == $.].
	^ (index = 0 or: [ pathName indexOfLastPathSeparator > index ])
		ifTrue: ['']
		ifFalse: [pathName copyFrom: index + 1 to: pathName size]
]

{ #category : 'utilities' }
FileIOAccessor >> fileMode: fullPathName [
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 2
]

{ #category : 'utilities' }
FileIOAccessor >> fileNamesIn: aDirectoryEntry [
	"
	FileIOAccessor default fileNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry)
		select: [ :each | each isFileEntry ]
		thenCollect: [ :each | each name ]
]

{ #category : 'actions' }
FileIOAccessor >> fileOrDirectoryExists: localName in: aDirectoryEntry [

	| entryNames |
	entryNames := self entryNamesIn: aDirectoryEntry.

	^self isCaseSensitive 
		ifTrue:[entryNames includes: localName]
		ifFalse:[entryNames anySatisfy: [:name| name sameAs: localName]].
]

{ #category : 'utilities' }
FileIOAccessor >> fileSize: fileEntry [

	| pathName f size |
	pathName := fileEntry pathName.
	"At least on Linux 64 Cog, opening a directory as a stream and asking #size answers some absurd number: 9223372036854775807"
	(self basicDirectoryExists: pathName) ifTrue: [^ nil ].
	f := UniFileStream new open: pathName forWrite: false.
	f ifNil: [^ nil].
	size := f size.
	f close.
	fileEntry invalidate.
	^ size
]

{ #category : 'utilities' }
FileIOAccessor >> getModificationDate: fullPathName [
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 10
]

{ #category : 'testing' }
FileIOAccessor >> isCaseSensitive [
	"FileIOAccessor default isCaseSensitive"
	^self onUnix
]

{ #category : 'utilities' }
FileIOAccessor >> isDirectory: fullPathName [
	"
		S_IFSOCK   0140000   socket
		S_IFLNK    0120000   symbolic link
		S_IFREG    0100000   regular file
		S_IFBLK    0060000   block device
		S_IFDIR    0040000   directory
		S_IFCHR    0020000   character device
		S_IFIFO    0010000   FIFO
	"

	| mode |
	mode := [self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 2] on: Error do: [0].
	^(mode bitAnd:  8r0040000) ~~ 0
]

{ #category : 'testing' }
FileIOAccessor >> isDriveSupported [
	^self onWindows
]

{ #category : 'utilities' }
FileIOAccessor >> isFile: fullPathName [
	"
		S_IFSOCK   0140000   socket
		S_IFLNK    0120000   symbolic link
		S_IFREG    0100000   regular file
		S_IFBLK    0060000   block device
		S_IFDIR    0040000   directory
		S_IFCHR    0020000   character device
		S_IFIFO    0010000   FIFO
	"

	| mode |
	mode := [self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 2] on: Error do: [0].
	^(mode bitAnd:  8r0100000) ~~ 0
]

{ #category : 'utilities' }
FileIOAccessor >> isReadOnly: fullPathName [
	^(self isWriteable: fullPathName) not
]

{ #category : 'utilities' }
FileIOAccessor >> isReadable: fullPathName [
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 13
]

{ #category : 'utilities' }
FileIOAccessor >> isWriteable: fullPathName [
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 14
]

{ #category : 'utilities' }
FileIOAccessor >> makeReadOnly: fullPathName [
	| currentMode newMode |
	currentMode := self fileMode: fullPathName.
	newMode := currentMode bitAnd: 8r777577.
	^(self primitiveChangeMode: fullPathName asUtf8Bytes mode: newMode) isNil
]

{ #category : 'utilities' }
FileIOAccessor >> makeReadWrite: fullPathName [
	| currentMode newMode |
	currentMode := self fileMode: fullPathName.
	newMode := currentMode bitOr: 8r000600.
	^(self primitiveChangeMode: fullPathName asUtf8Bytes mode: newMode) isNil
]

{ #category : 'testing' }
FileIOAccessor >> onMacOsX [
	^self onUnix and: [Smalltalk platformName = 'Mac OS']
]

{ #category : 'testing' }
FileIOAccessor >> onUnix [
	^self pathNameDelimiter == $/
]

{ #category : 'testing' }
FileIOAccessor >> onWindows [
	^self pathNameDelimiter == $\
]

{ #category : 'accessing' }
FileIOAccessor >> pathNameDelimiter [
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $/ "
	^self primPathNameDelimiter
]

{ #category : 'primitives' }
FileIOAccessor >> primCreateDirectory: fullPathAsUtf8Bytes [
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
FileIOAccessor >> primDeleteDirectory: fullPathAsUtf8Bytes [
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
FileIOAccessor >> primDeleteFileNamed: aFileNameAsUtf8Bytes [
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil

]

{ #category : 'primitives-FileAttributesPlugin' }
FileIOAccessor >> primFileAttribute: aString attributeNumber: attributeNumber [
	"Answer a single file attribute.
	primFileAttributes: aString attributeNumber: attributeNumber
	aString is the path to the file
	attributeNumber identifies which attribute to return:
	1 - 12: stat(): name, mode, ino, dev, nlink, uid, gid, size, accessDate,
	modifiedDate, changeDate, creationDate
	13 - 15: access(): R_OK, W_OK, X_OK
	16: isSymlink"

 	<primitive: 'primitiveFileAttribute' module: 'FileAttributesPlugin'>
	self primitiveFailed

]

{ #category : 'primitives-FileAttributesPlugin' }
FileIOAccessor >> primFileAttributes: aString mask: attributeMask [
	"Answer an array of file attributes.
	primFileAttributes: aString mask: attributeMask
	aString is the path to the file
	attributeMask indicates which attributes to retrieve:
	bit 0 - get stat() attributes
	bit 1 - get access() attributes
	bit 2 - use lstat() instead of stat()
	On error answer the appropriate error code (Integer)"

 	<primitive: 'primitiveFileAttributes' module: 'FileAttributesPlugin'>
	self primitiveFailed

]

{ #category : 'primitives-FileAttributesPlugin' }
FileIOAccessor >> primFileExists: aPathString [

 	<primitive: 'primitiveFileExists' module: 'FileAttributesPlugin'>
	self primitiveFailed

]

{ #category : 'primitives-FileAttributesPlugin' }
FileIOAccessor >> primFileMasks [
	"
		masks[0] = (integerObjectOf(S_IFMT));
		masks[1] = (integerObjectOf(S_IFSOCK));
		masks[2] = (integerObjectOf(S_IFLNK));
		masks[3] = (integerObjectOf(S_IFREG));
		masks[4] = (integerObjectOf(S_IFBLK));
		masks[5] = (integerObjectOf(S_IFDIR));
		masks[6] = (integerObjectOf(S_IFCHR));
		masks[7] = (integerObjectOf(S_IFIFO))
	"

 	<primitive: 'primitiveFileMasks' module: 'FileAttributesPlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
FileIOAccessor >> primFixedNameLookupEntryIn: fullPathAsUtf8Bytes index: index [
	"Fix the malformed String answered by the primitive. See comment at #primLookupEntryIn:index:"

	| answer |
	answer := self primLookupEntryIn: fullPathAsUtf8Bytes index: index.
	answer isArray ifTrue: [
		answer at: 1 put: (UnicodeString fromUtf8Bytes: answer first asByteArray) asByteStringIfAscii ].
	^answer
]

{ #category : 'primitives' }
FileIOAccessor >> primLookupEntryIn: fullPathAsUtf8Bytes index: index [
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On Windows, the empty string enumerates the mounted volumes/drives.
	
	On macOS, the primitive fails.
	
	On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad.
	
	Warning: The 'name' field is an instance of String, but in reality it contains the bytes for its UTF-8 representation.
	For instance, if the real name is 'puño' we'll get  'puño', as
	(String withAll: ('puño' AsUtf8Bytes asArray collect: [ :n | Character codePoint: n ]))        =        'puño'
	Senders MUST do appropriate conversion.
	
	Consider calling #primFixedNameLookupEntryIn:index: instead.
	"

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath


]

{ #category : 'primitives' }
FileIOAccessor >> primPathNameDelimiter [
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
FileIOAccessor >> primRename: oldFileFullNameAsUtf8Bytes to: newFileFullNameAsUtf8Bytes [
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil
]

{ #category : 'primitives' }
FileIOAccessor >> primWorkingDirectory [
	"Answer the utf8 encoded name of the VM process's working/current directory.
	Warning: Answer is an instance of String, but an invalid one (unless pure 7-bit ASCII).
	Please call
		DirectoryEntry workingDirectory
	instead.
	"

	<primitive: 'primitiveGetWorkingDirectory' module: 'FilePlugin' error: ec>
	self primitiveFailed.
]

{ #category : 'primitives' }
FileIOAccessor >> primWorkingDirectory: utf8Path [
	"Set the VM process's working/current directory to be the argument, a utf8 string.
	Please call
		DirectoryEntry workingDirectory:
	instead.
	"

	<primitive: 'primitiveSetWorkingDirectory' module: 'FilePlugin' error: ec>
	self primitiveFailed.
]

{ #category : 'primitives-FileAttributesPlugin' }
FileIOAccessor >> primitiveChangeMode: aString mode: ModeFlags [
	""

 	<primitive: 'primitiveChangeMode' module: 'FileAttributesPlugin'>
	self primitiveFailed

]

{ #category : 'file stream creation' }
FileIOAccessor >> privateForceNewFile: aFileEntry [
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	| pathName |
	pathName := aFileEntry pathName.
	aFileEntry exists
		ifTrue: [
			self deleteFile: pathName ifAbsent: [
				(CannotDeleteFileException new
					messageText: 'Could not delete the old version of file ' , pathName) signal]].

	^ self privateWriteableFile: aFileEntry
]

{ #category : 'file stream creation' }
FileIOAccessor >> privateNewFile: aFileEntry [
	"Create a new file with the given full pathName."

	^aFileEntry exists
		ifTrue: [
			(FileExistsException fileName: aFileEntry pathName) signal]
		ifFalse: [
			self privateWriteableFile: aFileEntry ]
]

{ #category : 'file stream creation' }
FileIOAccessor >> privateReadOnlyFile: fileEntry [
	"Open the existing file with the given name in this directory for read-only access."

	| pathName |
	pathName := fileEntry pathName.
	(self basicDirectoryExists: pathName) ifTrue: [
		"If it is a directory, the it is not a file, and the requested file does not exist."
		^ ((FileDoesNotExistException fileName: pathName) readOnly: true) signal ].
	^(UniFileStream new open: pathName forWrite: false)
		ifNil: [
			"File does not exist..."
			((FileDoesNotExistException fileName: pathName) readOnly: true) signal ]
]

{ #category : 'file stream creation' }
FileIOAccessor >> privateWriteableFile: aFileEntry [
	"Open the file with the given name in this directory for writing."

	| pathName |
	pathName := aFileEntry pathName.
	^ (UniFileStream new open: pathName forWrite: true)
		ifNil: [
			"Failed to open the file"
			(FileWriteError fileName: pathName)
				signal: ('File [', pathName, '] open for write failed' ) ].
]

{ #category : 'actions' }
FileIOAccessor >> rename: oldFileFullName to: newFileFullName  [
	| selection |
	(self try: [self primRename: oldFileFullName asUtf8Bytes to: newFileFullName asUtf8Bytes ]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFullFileEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(newFileFullName asFullFileEntry exists or: [ newFileFullName asFullDirectoryEntry exists ])
		ifTrue: [
			selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'
]

{ #category : 'actions' }
FileIOAccessor >> renameDirectory: oldFileFullName to: newFileFullName  [
	| selection |
	(self try: [self primRename: oldFileFullName asUtf8Bytes to: newFileFullName asUtf8Bytes ]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFullDirectoryEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	newFileFullName asFullDirectoryEntry exists
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asFullDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'
]

{ #category : 'accessing' }
FileIOAccessor >> slash [
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash := String with: self pathNameDelimiter ].
	^slash
]

{ #category : 'utilities' }
FileIOAccessor >> splitNameVersionExtensionFor: fileName [
	" answer an array with the root name, version # and extension.
	See comment in senders for more details"

	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		| baseName version i j |
		baseName := b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) == $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ Array with: baseName with: version with: extension ]
]

{ #category : 'private' }
FileIOAccessor >> try: execBlock forFileNamed: fullName [

	"If fail, return nil"

	^ (UniFileStream retryWithGC: execBlock until: [:result | result notNil] forFileNamed: fullName) notNil
]

{ #category : 'private' }
FileIOAccessor >> updateEntry: aFileSystemEntry [
	| entryArray index lookIn isDirectory |

	"If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries."
	aFileSystemEntry primEntryInParent ifNotNil: [ :tentativeIndex |
		(self primFixedNameLookupEntryIn: aFileSystemEntry parent pathName asUtf8Bytes index: tentativeIndex) ifNotNil: [ :found |
	 		found == #badDirectoryPath ifFalse: [
				aFileSystemEntry name = (found at: 1) ifTrue: [
					aFileSystemEntry updateFrom: found entryInParent: tentativeIndex.
					^ self ]]]].

	"Otherwise, do a full iteration"
	lookIn := aFileSystemEntry parent pathName.
	index := 1.
	[
		entryArray := self primFixedNameLookupEntryIn: lookIn asUtf8Bytes index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ self].
		entryArray == nil ifTrue: [
			^ self].
		isDirectory := entryArray at: 4.
		aFileSystemEntry name = (entryArray at: 1) ifTrue: [
			isDirectory == aFileSystemEntry isDirectoryEntry ifTrue: [
				aFileSystemEntry updateFrom: entryArray entryInParent: index ].
			"If found, exit even if invalid. No point to keep iterating."
			^ self ].
		index := index + 1] repeat
]
