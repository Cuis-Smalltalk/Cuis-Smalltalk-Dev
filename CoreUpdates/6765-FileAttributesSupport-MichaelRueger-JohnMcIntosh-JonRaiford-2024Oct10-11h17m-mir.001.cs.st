'From Cuis7.1 [latest update: #6764] on 10 October 2024 at 11:29:12 am'!

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:58:16'!
exists: fullPathName
	^self primFileExists: fullPathName asUtf8Bytes ! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:58:29'!
fileMode: fullPathName
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 2! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:58:41'!
getModificationDate: fullPathName
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 10! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:58:48'!
isDirectory: fullPathName
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
	^(mode bitAnd:  8r0040000) ~~ 0! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:58:56'!
isFile: fullPathName
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
	^(mode bitAnd:  8r0100000) ~~ 0! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'JMM 8/30/2024 11:20:23'!
isReadOnly: fullPathName
	^(self isWriteable: fullPathName) not! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:59:13'!
isReadable: fullPathName
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 13! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:59:17'!
isWriteable: fullPathName
	^self primFileAttribute: fullPathName asUtf8Bytes attributeNumber: 14! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:59:22'!
makeReadOnly: fullPathName
	| currentMode newMode |
	currentMode := self fileMode: fullPathName.
	newMode := currentMode bitAnd: 8r777577.
	^(self primitiveChangeMode: fullPathName asUtf8Bytes mode: newMode) isNil! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jfr 8/16/2024 09:59:28'!
makeReadWrite: fullPathName
	| currentMode newMode |
	currentMode := self fileMode: fullPathName.
	newMode := currentMode bitOr: 8r000600.
	^(self primitiveChangeMode: fullPathName asUtf8Bytes mode: newMode) isNil! !

!FileIOAccessor methodsFor: 'primitives-FileAttributesPlugin' stamp: 'mir 7/29/2024 09:21:44'!
primFileAttribute: aString attributeNumber: attributeNumber
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
! !

!FileIOAccessor methodsFor: 'primitives-FileAttributesPlugin' stamp: 'mir 7/29/2024 09:21:25'!
primFileAttributes: aString mask: attributeMask
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
! !

!FileIOAccessor methodsFor: 'primitives-FileAttributesPlugin' stamp: 'mir 7/31/2024 11:53:03'!
primFileExists: aPathString

 	<primitive: 'primitiveFileExists' module: 'FileAttributesPlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives-FileAttributesPlugin' stamp: 'mir 7/29/2024 11:58:09'!
primFileMasks
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
! !

!FileIOAccessor methodsFor: 'primitives-FileAttributesPlugin' stamp: 'mir 7/29/2024 09:42:46'!
primitiveChangeMode: aString mode: ModeFlags
	""

 	<primitive: 'primitiveChangeMode' module: 'FileAttributesPlugin'>
	self primitiveFailed
! !


!FileSystemEntry methodsFor: 'testing' stamp: 'mir 7/29/2024 12:22:46'!
existsAsDirectory
	^FileIOAccessor default isDirectory: self pathName! !

!FileSystemEntry methodsFor: 'testing' stamp: 'mir 7/29/2024 12:23:12'!
existsAsFile
	^FileIOAccessor default isFile: self pathName! !

!FileSystemEntry methodsFor: 'testing' stamp: 'mir 7/29/2024 09:56:41'!
isReadOnly
	^FileIOAccessor default isReadOnly: self pathName! !

!FileSystemEntry methodsFor: 'testing' stamp: 'mir 7/29/2024 10:05:33'!
isReadable
	^FileIOAccessor default isReadable: self pathName! !

!FileSystemEntry methodsFor: 'testing' stamp: 'mir 7/29/2024 10:03:38'!
isWriteable
	^FileIOAccessor default isWriteable: self pathName! !

!FileSystemEntry methodsFor: 'attributes' stamp: 'mir 7/29/2024 10:18:33'!
makeReadOnly
	^FileIOAccessor default makeReadOnly: self pathName! !

!FileSystemEntry methodsFor: 'attributes' stamp: 'mir 7/29/2024 10:30:04'!
makeReadWrite
	^FileIOAccessor default makeReadWrite: self pathName! !


!FileIOAccessor reorganize!
('actions' copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn: try:forFileNamed: updateEntry:)
('utilities' absolutePathComponentsFor:relativeTo: baseNameAndExtensionFor:do: baseNameFor: copyFile:toFile: directoryNamesIn: entryNamesIn: exists: expandTildeFor: extensionFor: fileMode: fileNamesIn: fileSize: getModificationDate: isDirectory: isFile: isReadOnly: isReadable: isWriteable: makeReadOnly: makeReadWrite: splitNameVersionExtensionFor:)
('accessing' drives pathNameDelimiter slash)
('file stream creation' privateForceNewFile: privateNewFile: privateReadOnlyFile: privateWriteableFile:)
('testing' isCaseSensitive isDriveSupported onMacOsX onUnix onWindows)
('primitives' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primFixedNameLookupEntryIn:index: primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
('primitives-FileAttributesPlugin' primFileAttribute:attributeNumber: primFileAttributes:mask: primFileExists: primFileMasks primitiveChangeMode:mode:)
!


!FileSystemEntry reorganize!
('comparing' = hash)
('accessing-file name' baseName extension nameVersionExtension nameWithoutExtension)
('accessing' allParents commonPathWith: creationTime drive exists modificationTime name name: name:parent: parent pathComponents pathName)
('private' ensureParent fileAccessor invalidateIfOld setContentsOf:to: setParent: updateFrom:entryInParent:)
('testing' existsAsDirectory existsAsFile isDirectory isFile isReadOnly isReadable isRoot isWriteable updateExists)
('initialize-release' absolutePathString: invalidate pathComponents:drive: pathTokens: relativePathString:to:)
('printing' printOn: printPathOn:)
('convenience' ifExists:)
('attributes' makeReadOnly makeReadWrite)
!

