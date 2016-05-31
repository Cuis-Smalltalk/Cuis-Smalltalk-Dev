'From Cuis 4.2 of 25 July 2013 [latest update: #2781] on 31 May 2016 at 1:06:42.21371 pm'!
!classDefinition: #FileIOAccessor category: #'System-FileMan-Core'!
Object subclass: #FileIOAccessor
	instanceVariableNames: 'slash drives'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-FileMan-Core'!

!FileIOAccessor commentStamp: '<historical>' prior: 0!
I am an accessor to the low level file IO.

You can extend/rewrite me if you port FileMan to other Smalltalk dialects.

---
mu 3/13/2007 11:11!

!classDefinition: #FileSystemEntry category: #'System-FileMan-Core'!
Object subclass: #FileSystemEntry
	instanceVariableNames: 'drive pathComponents parent name creationTime modificationTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-FileMan-Core'!

!FileSystemEntry commentStamp: 'jmv 5/31/2016 10:37' prior: 0!
I represent a single file entry (including directory).

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See DirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFileEntry fileSize
	Smalltalk imageName asFileEntry parent directories do: [ :a | a print ]!

!classDefinition: #DirectoryEntry category: #'System-FileMan-Core'!
FileSystemEntry subclass: #DirectoryEntry
	instanceVariableNames: 'children'
	classVariableNames: 'CurrentDirectory ImageDirectory VMDirectory'
	poolDictionaries: ''
	category: 'System-FileMan-Core'!

!DirectoryEntry commentStamp: 'jmv 5/31/2016 10:42' prior: 0!
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
	
See FileEntry!

!classDefinition: #FileEntry category: #'System-FileMan-Core'!
FileSystemEntry subclass: #FileEntry
	instanceVariableNames: 'fileSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-FileMan-Core'!

!FileEntry commentStamp: 'jmv 5/31/2016 10:36' prior: 0!
I represent a single file entry.

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See DirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFileEntry fileSize
	Smalltalk imageName asFileEntry parent directories do: [ :a | a print ]!


!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:36'!
absolutePathComponentsFor: aString
	"Not complete, but in most cases it is OK
	See comment at #isAbsolutePathName"

	| tokens curDirPathComponents |
	tokens _ aString asPathTokens.

	aString isAbsolutePathName ifTrue: [ ^ tokens asArray ].

	curDirPathComponents _ DirectoryEntry currentDirectory pathComponents.
	aString = '.' ifTrue: [ ^ curDirPathComponents copy ].
	aString = '..' ifTrue:  [^ curDirPathComponents allButLast ].

	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		curDirPathComponents _ curDirPathComponents allButLast.
		tokens removeFirst ].

	^ Array streamContents: [ :strm |
		strm nextPutAll: curDirPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]]! !

!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
copy: fromFileEntry to: toFileEntry 
	| readStr writeStr |
	[readStr := (self privateReadOnlyFile: fromFileEntry) binary.
	writeStr := (self privateForceNewFile: toFileEntry) binary.
	self copyFile: readStr toFile: writeStr]
		ensure: [
			readStr
				ifNotNil: [ :r | r close ].
			writeStr
				ifNotNil: [ :w | w close ]]! !

!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
createDirectory: fullPathName
	^self primCreateDirectory: fullPathName! !

!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
deleteDirectory: fullPathName
	^self primDeleteDirectory: fullPathName! !

!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
deleteFile: fullPathName
	^self deleteFile: fullPathName ifAbsent: []! !

!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
deleteFile: fullPathName ifAbsent: failBlock 
	^(self
			try: [self primDeleteFileNamed: fullPathName]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/31/2016 10:37'!
fileOrDirectoryExists: localName in: aDirectoryEntry

	| entryNames |
	entryNames := self entryNamesIn: aDirectoryEntry.

	^self isCaseSensitive 
		ifTrue:[entryNames includes: localName]
		ifFalse:[entryNames anySatisfy: [:name| name sameAs: localName]].! !

!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFileEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(newFileFullName asFileEntry exists or: [ newFileFullName asDirectoryEntry exists ])
		ifTrue: [
			selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asDirectoryEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	newFileFullName asDirectoryEntry exists
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FileIOAccessor methodsFor: 'private' stamp: 'pb 5/25/2016 00:32'!
basicDirectoryExists: fullPathName

	| result |
	result := self primLookupEntryIn: fullPathName index: 1.
 	^(result == #badDirectoryPath) not! !

!FileIOAccessor methodsFor: 'private' stamp: 'pb 5/25/2016 00:32'!
containingDirectoryPathOf: pathName 

	(((pathName isNil
			or: [pathName isEmpty])
			or: [pathName isPathSeparator])
			or: [pathName isDriveName])
		ifTrue: [^ nil].
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1! !

!FileIOAccessor methodsFor: 'private' stamp: 'pb 5/25/2016 00:41'!
entriesIn: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.

Windows (nil means root)
FileIOAccessor default entriesIn: nil #(C:\ D:\)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

MacOsX (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(/Volumes/SanDisk32-NTFS/CuisTest/2554-REVISAR-JuanVuletich-2015Oct21-16h40m-jmv.1.cs.st /Volumes/SanDisk32-NTFS/CuisTest/Cog.app /Volumes/SanDisk32-NTFS/CuisTest/Cog.app.tgz /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.changes /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.image /Volumes/SanDisk32-NTFS/CuisTest/CuisV4.sources)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/.dbfseventsd /.DocumentRevisions-V100 /.DS_Store /.file /.fseventsd /.hotfiles.btree /.Spotlight-V100 /.Trashes /.vol /Applications /bin /cores /dev /etc /home /installer.failurerequests /Library /net /Network /opt /private /sbin /System /tmp /Users /usr /var /Volumes)

	"
	| entries index done entryArray entry isDirectory lookIn |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	lookIn _ parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName].
	[done] whileFalse: [
		entryArray := self primLookupEntryIn: lookIn index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				isDirectory
					ifTrue: [entry _ DirectoryEntry new]
					ifFalse: [
						entry _ FileEntry new.
						entry fileSize: (entryArray at: 5) ].
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ]
				ifNil: [
					entry pathName: entry name ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FileIOAccessor methodsFor: 'private' stamp: 'pb 5/25/2016 00:32'!
try: execBlock forFileNamed: fullName

	"If fail, return nil"

	^ (self concreteStreamClass retryWithGC: execBlock until: [:result | result notNil] forFileNamed: fullName) notNil! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:36'!
baseNameAndExtensionFor: pathName do: aBlock
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
	extension _ self extensionFor: pathName.
	extension isEmpty ifTrue: [
		^ aBlock value: pathName value: '' ].
	^ aBlock value: (pathName copyFrom: 1 to: pathName size - extension size - 1) value: extension! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:36'!
baseNameFor: pathName
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
		^baseName ]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:32'!
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| badChars hasBadChars |
	badChars _ #( $: $< $> $| $/ $\ $? $* $") asSet.
	hasBadChars _ aFileName includesAnyOf: badChars.
	(hasBadChars and:[fixing not]) ifTrue: [^self error:'Invalid file name'].
	hasBadChars ifFalse:[^ aFileName].
	^ aFileName collect: [ :char |
			(badChars includes: char) 
				ifTrue:[$#] 
				ifFalse:[char]]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:32'!
copyFile: fileStream1 toFile: fileStream2
	| buffer |
	buffer := String new: 50000.
	[fileStream1 atEnd] whileFalse:
		[fileStream2 nextPutAll: (fileStream1 nextInto: buffer)].
! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 5/31/2016 10:47'!
directoryNamesIn: aDirectoryEntry
	"
	FileIOAccessor default directoryNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry)
		select: [ :each | each isDirectory]
		thenCollect: [ :each | each name]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 5/31/2016 10:48'!
entryNamesIn: aDirectoryEntry
	"
	FileIOAccessor default entryNamesIn: 'C:\Windows\' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry) collect: [ :each | each name]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:42'!
extensionFor: pathName 
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.
	
	FileIOAccessor default extensionFor: 'writings.txt'
	FileIOAccessor default extensionFor: 'folder.ext/file'
	FileIOAccessor default extensionFor: 'optionalstuff.pck.st'
	FileIOAccessor default extensionFor: 'code.cs.st'
	FileIOAccessor default extensionFor: 'code.cs'
	"
	| index |
	{ '.cs.st' . '.pck.st' } do: [ :specialExtension |
		(pathName endsWith: specialExtension)
			ifTrue: [ ^specialExtension copyFrom: 2 to: specialExtension size ]].
	index _ pathName
				findLast: [ :c | c = $.].
	^ (index = 0 or: [ pathName indexOfLastPathSeparator > index ])
		ifTrue: ['']
		ifFalse: [pathName copyFrom: index + 1 to: pathName size]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:32'!
fileExists: fileEntry

	| f |
	f _ self concreteStreamClass new open: fileEntry pathName forWrite: false.
	f ifNil: [^ false].
	f close.
	^ true! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 5/31/2016 10:48'!
fileNamesIn: aDirectoryEntry
	"
	FileIOAccessor default fileNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^((self entriesIn: aDirectoryEntry)
		reject: [ :each | each isDirectory ])
		collect: [ :each | each name ]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:32'!
fileSize: fileEntry

	| f size |
	f _ self concreteStreamClass new open: fileEntry pathName forWrite: false.
	f ifNil: [^ nil].
	size _ f size.
	f close.
	^ size! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'pb 5/25/2016 00:32'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in nextSequentialNameFor: for more details"

	| baseName version i j |

	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		baseName _ b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) = $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ Array with: baseName with: version with: extension ]! !

!FileIOAccessor methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:32'!
concreteStreamClass
	^FileStream concreteStream! !

!FileIOAccessor methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:40'!
drives
	"
	Answer a collection of Strings
	FileIOAccessor default drives
	"
	drives _ nil. 		"will change if you mount or unmount drives!!"
	drives ifNil: [
		drives _ self onUnix
			ifTrue: [ #() ]
			ifFalse: [ (self entriesIn: nil)]].
	^drives! !

!FileIOAccessor methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:32'!
pathNameDelimiter
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $/ "
	^self primPathNameDelimiter! !

!FileIOAccessor methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:32'!
slash
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash _ self pathNameDelimiter asString ].
	^slash! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'pb 5/25/2016 00:32'!
privateForceNewFile: aFileEntry
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	| pathName |
	pathName _ aFileEntry pathName.
	aFileEntry exists
		ifTrue: [
			self deleteFile: pathName ifAbsent: [
				(CannotDeleteFileException new
					messageText: 'Could not delete the old version of file ' , pathName) signal]].

	^ self privateWriteableFile: aFileEntry! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'pb 5/25/2016 00:32'!
privateNewFile: aFileEntry
	"Create a new file with the given full pathName."

	^aFileEntry exists
		ifTrue: [
			(FileExistsException fileName: aFileEntry pathName fileClass: self concreteStreamClass) signal]
		ifFalse: [
			self privateWriteableFile: aFileEntry ]! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'pb 5/25/2016 00:32'!
privateReadOnlyFile: fileEntry
	"Open the existing file with the given name in this directory for read-only access."

	| pathName |
	pathName _ fileEntry pathName.
	^(self concreteStreamClass new open: pathName forWrite: false)
		ifNil: [
			"File does not exist..."
			((FileDoesNotExistException fileName: pathName) readOnly: true) signal ]! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'pb 5/25/2016 00:32'!
privateWriteableFile: aFileEntry
	"Open the file with the given name in this directory for writing."

	| pathName |
	pathName _ aFileEntry pathName.
	^ (self concreteStreamClass new open: pathName forWrite: true)
		ifNil: [
			"Failed to open the file"
			(FileWriteError fileName: pathName)
				signal: ('File [', pathName, '] open for write failed' ) ]! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:42'!
isCaseSensitive
	"FileIOAccessor default isCaseSensitive"
	^self onUnix! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:32'!
isDriveSupported
	^self onWindows or: [self onMacClassic]! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:32'!
onMacClassic
	^self pathNameDelimiter = $:! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:32'!
onMacOsX
	^self onUnix and: [Smalltalk platformName = 'Mac OS']! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:32'!
onUnix
	^self pathNameDelimiter = $/! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 5/25/2016 00:32'!
onWindows
	^self pathNameDelimiter = $\! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'pb 5/25/2016 00:32'!
primCreateDirectory: fullPath
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'pb 5/25/2016 00:32'!
primDeleteDirectory: fullPath
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'pb 5/25/2016 00:32'!
primDeleteFileNamed: aFileName
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'pb 5/25/2016 00:37'!
primLookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On MacOS and Windows,  the empty string enumerates the mounted volumes/drives.
	
	On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'pb 5/25/2016 00:32'!
primPathNameDelimiter
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'pb 5/25/2016 00:32'!
primRename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil! !


!FileIOAccessor class methodsFor: 'class initialization' stamp: 'pb 5/25/2016 00:42'!
initialize
	"
	FileIOAccessor initialize
	"
	Default := nil! !

!FileIOAccessor class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 00:32'!
default
	Default isNil
		ifTrue: [
			Default _ self new].
	^ Default! !

!FileIOAccessor class methodsFor: 'cached state access' stamp: 'pb 5/25/2016 00:32'!
releaseClassCachedState
	Default _ nil! !


!FileSystemEntry methodsFor: 'comparing' stamp: 'jmv 5/31/2016 10:23'!
= aFileEntry
	| isCaseSensitive myDrive otherDrive |
	self class = aFileEntry class ifFalse: [ ^false ].

	isCaseSensitive _ self fileAccessor isCaseSensitive.

	"Check for drive nil or same."
	myDrive _ self drive.
	otherDrive _ aFileEntry drive.
	isCaseSensitive
		ifTrue: [ self drive = aFileEntry drive ifFalse: [ ^false ]]
		ifFalse: [
			myDrive isNil = otherDrive isNil ifFalse: [ ^false ].		"only one of them is nil"
			myDrive ifNotNil: [											"none is nil"
				(myDrive sameAs: otherDrive) ifFalse: [ ^false ]]].

	"Check for all path components same."
	self pathComponents with: aFileEntry pathComponents do: [ :mine :other |
		isCaseSensitive
			ifTrue: [ mine = other ifFalse: [ ^false ]]
			ifFalse: [ (mine sameAs: other) ifFalse: [ ^false ]]].

	^ true! !

!FileSystemEntry methodsFor: 'comparing' stamp: 'jmv 5/31/2016 10:23'!
hash
	^self pathComponents hash! !

!FileSystemEntry methodsFor: 'accessing-file name' stamp: 'jmv 5/31/2016 10:23'!
baseName
	^self nameVersionExtension first! !

!FileSystemEntry methodsFor: 'accessing-file name' stamp: 'jmv 5/31/2016 10:23'!
extension
	^self fileAccessor extensionFor: name! !

!FileSystemEntry methodsFor: 'accessing-file name' stamp: 'jmv 5/31/2016 10:23'!
nameVersionExtension
	^self fileAccessor splitNameVersionExtensionFor: self name! !

!FileSystemEntry methodsFor: 'accessing-file name' stamp: 'jmv 5/31/2016 10:23'!
nameWithoutExtension
	"
	'writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings' asFileEntry nameWithoutExtension
	"
	^self fileAccessor baseNameFor: name! !

!FileSystemEntry methodsFor: 'accessing-file name' stamp: 'jmv 5/31/2016 10:23'!
version
	^self nameVersionExtension second! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
basicPathComponents: aCollection
	pathComponents := aCollection! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
creationTime
	creationTime ifNil: [self initValuesFromParent].
	^creationTime! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
creationTime: value
	creationTime := value! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
drive
	self fileAccessor onUnix ifTrue: [^ drive := nil].
	^ drive! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
drive: aString
	drive := aString! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
modificationTime
	modificationTime ifNil: [self initValuesFromParent].
	^modificationTime! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
modificationTime: value
	modificationTime := value! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
name
	^name ifNil: [ drive ]! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
name: aString 
	name := aString.
	self pathComponents
		ifNotEmpty: [self pathComponents at: self pathComponents size put: name]! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
parent
	parent ifNil: [parent := self ensureParent].
	^parent! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
parent: aFmDirectory

	parent _ aFmDirectory.
	drive _ aFmDirectory drive. "harmless if no drive supported, as in Unix"
	pathComponents _ aFmDirectory pathComponents copyWith: name! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
parents
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
pathComponents
	pathComponents ifNil: [pathComponents _ #() ].
	^pathComponents! !

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 5/31/2016 10:23'!
pathName

	^ String streamContents: [ :stream |
		self printPathOn: stream ]! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/31/2016 10:26'!
ensureParent
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent := DirectoryEntry
				pathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/31/2016 10:45'!
fileAccessor

	^FileIOAccessor default! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/31/2016 10:23'!
initValuesFrom: otherEntry
	otherEntry ifNil: [^self].
	self creationTime: otherEntry creationTime.
	self modificationTime: otherEntry modificationTime! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/31/2016 10:23'!
initValuesFromParent
	| targets target |
	self ensureParent.
	self parent ifNil: [ ^self ].

	targets := self isDirectory ifTrue: [ self parent directories ] ifFalse: [ self parent files ].
	target := targets detect: [ :each | each = self ] ifNone: [
		^ self ].

	self initValuesFrom: target! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/31/2016 10:23'!
setContentsOf: aStream to: aStringOrBytes

	aStringOrBytes isString
		ifFalse: [ aStream binary].
	aStream nextPutAll: aStringOrBytes! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/31/2016 10:43'!
setParent: aDirectoryEntry
	parent := aDirectoryEntry! !

!FileSystemEntry methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:23'!
isDirectory
	^false! !

!FileSystemEntry methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:23'!
isFile
	^false! !

!FileSystemEntry methodsFor: 'testing' stamp: 'jmv 5/31/2016 10:23'!
isRoot
	^self parent isNil! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 5/31/2016 10:23'!
pathComponents: aCollection
	self pathComponents: aCollection detectDrive: true! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 5/31/2016 10:23'!
pathComponents: tokens detectDrive: detectDrive
	| firstToken  |
	tokens isEmptyOrNil ifTrue: [ ^pathComponents _ nil ].
	(detectDrive and: [ (firstToken _ tokens first) isDriveName])
		ifTrue: [
			self drive: firstToken.
			self basicPathComponents: (tokens copyFrom: 2 to: tokens size)]
		ifFalse: [ self basicPathComponents: tokens ].

	pathComponents ifNotEmpty: [ self name: pathComponents last ]! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 5/31/2016 10:45'!
pathName: aString 
	| tokens guessedDriveName |
	tokens _ FileIOAccessor default absolutePathComponentsFor: aString.
	tokens ifEmpty: [^ nil].
	self fileAccessor isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName ifNotNil: [
				self drive: guessedDriveName.
				tokens := tokens copyFrom: 2 to: tokens size ]].
	self pathComponents: tokens! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 5/31/2016 10:23'!
refresh
	creationTime := modificationTime := nil! !

!FileSystemEntry methodsFor: 'printing' stamp: 'jmv 5/31/2016 10:23'!
printOn: aStream 
	self printPathOn: aStream! !

!FileSystemEntry methodsFor: 'printing' stamp: 'jmv 5/31/2016 10:23'!
printPathOn: aStream 
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPutAll: self fileAccessor slash.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPutAll: self fileAccessor slash]! !


!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 5/31/2016 10:23'!
pathComponents: comps
	| inst |
	inst := self new.
	inst pathComponents: comps.
	^inst! !

!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 5/31/2016 10:23'!
pathComponents: comps drive: driveString
	| inst |
	inst := self new.
	inst pathComponents: comps detectDrive: false.
	inst drive: driveString.
	^inst! !

!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 5/31/2016 10:23'!
pathName: aString
	| inst |
	inst := self new.
	inst pathName: aString.
	^inst! !

!FileSystemEntry class methodsFor: 'class state access' stamp: 'jmv 5/31/2016 10:23'!
releaseClassCachedState

	self allSubInstancesDo: [ :each | each refresh]! !


!DirectoryEntry methodsFor: 'actions-path' stamp: 'pb 5/25/2016 00:33'!
/ pathString
	"Answer an instance of DirectoryEntry.
	If you want an instance of FileEntry, please call #//"
	^self concatPathComponentsAsDirectory: pathString asString asPathComponents! !

!DirectoryEntry methodsFor: 'actions-path' stamp: 'pb 5/25/2016 00:34'!
// pathString
	"Answer an instance of FileEntry.
	If you want an instance of DirectoryEntry, please call #/"
	^self concatPathComponentsAsFile: pathString asString asPathComponents! !

!DirectoryEntry methodsFor: 'actions-path' stamp: 'pb 5/25/2016 00:34'!
concatPathComponentsAsDirectory: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	^entry! !

!DirectoryEntry methodsFor: 'actions-path' stamp: 'pb 5/25/2016 00:34'!
concatPathComponentsAsFile: components

	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components allButLast do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	entryComponents := entryComponents copyWith: components last.
	entry := FileEntry pathComponents: entryComponents drive: self drive.
	parentEntry ifNotNil: [
		entry setParent: parentEntry ].

	^entry! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
allChildrenDo: aBlock
	self childrenDo: 
		[:child | 
		aBlock value: child.
		child allChildrenDo: aBlock]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
allDirectoriesDo: aBlock
	self directoriesDo: 
		[:child | 
		aBlock value: child.
		child allDirectoriesDo: aBlock]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
allFilesDo: aBlock
	self childrenDo: 
		[:child | 
		child isFile ifTrue: [aBlock value: child] ifFalse: [child allFilesDo: aBlock]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
allFilesDo: aBlock matches: selectionBlock
	self childrenDo: 
		[:child | 
		child isFile
			ifTrue: [(selectionBlock value: child) ifTrue: [aBlock value: child]]
			ifFalse: [child allFilesDo: aBlock matches: selectionBlock]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
childrenDo: aBlock
	^self children do: aBlock! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
directoriesDo: aBlock
	^self directories do: aBlock! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
directoriesMatches: selectionBlock
	^self directories select: selectionBlock! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
filesDo: aBlock
	self children do: [ :each |
		each isFile ifTrue: [
			aBlock value: each ]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
filesMatches: selectionBlock
	^self files select: selectionBlock! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
latestFileMatches: selectionBlock
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) first! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'pb 5/25/2016 00:31'!
oldestFileMatches: selectionBlock
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) last! !

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'pb 5/25/2016 00:31'!
assureExistence
	self isRoot ifTrue: [^self].
	self parent assureExistenceDirNamed: self name.
	^self! !

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'pb 5/25/2016 00:31'!
copyTo: filename 
	
	| toDir |
	filename asFileEntry exists ifTrue: [^self error: 'Cannot copy directory to file'].
	
	toDir := filename asDirectoryEntry assureExistence.

	self
		filesDo: [:file | file copyTo: (toDir / file name) pathName].
	
	self
		directoriesDo: [ :dir |
			dir copyTo:  dir pathName ]! !

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'pb 5/25/2016 00:31'!
delete
	self fileAccessor deleteDirectory: self pathName.
	self refreshChildren! !

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'pb 5/25/2016 00:31'!
recursiveDelete
	self exists
		ifTrue: [self basicRecursiveDelete]! !

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'pb 5/25/2016 00:31'!
rename: newName
	
	self fileAccessor renameDirectory: self pathName to: (self parent / newName) pathName.
	self name: newName! !

!DirectoryEntry methodsFor: 'private' stamp: 'pb 5/25/2016 00:31'!
assureExistenceDirNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceDirNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName! !

!DirectoryEntry methodsFor: 'private' stamp: 'pb 5/25/2016 00:31'!
basicRecursiveDelete
	self refreshChildren.
	self directoriesDo: [:dir | dir basicRecursiveDelete].
	self filesDo: [:file | file delete].
	self delete! !

!DirectoryEntry methodsFor: 'private' stamp: 'pb 5/25/2016 00:31'!
initChildren

	self exists ifFalse: [ ^children _ #()] .
	children _ self fileAccessor entriesIn: self.
	^children! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
at: localFileName

	^(self // localFileName) textContents! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
at: localFileName ifAbsent: block

	^ [self at: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 5/31/2016 11:24'!
at: localFileName put: contents

	(self // localFileName) forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: contents ].
	self refreshChildren.
	^contents! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
binaryAt: localFileName 

	^ (self // localFileName) binaryContents! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
binaryAt: localFileName ifAbsent: block

	^ [self binaryAt: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
binaryAt: localFileName put: contents 
	^self at: localFileName put: contents asByteArray! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
includesKey: fileName
	^self fileNames includes: fileName! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
keys
	^self files! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
removeKey: localFileName 
	self removeKey: localFileName ifAbsent: []! !

!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
removeKey: localFileName ifAbsent: failBlock
	self fileAccessor deleteFile: (self // localFileName) pathName ifAbsent: [^failBlock value].
	self refreshChildren.! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:31'!
children
	"Caching this is prone to trouble. Files can be created and deleted anytime."
	children _ nil.

	children ifNil: [self initChildren].
	^children! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:31'!
directories
	^self children select: [:each | each isFile not]! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:31'!
directoryNames
	^self directories collect: [:each | each name]! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:31'!
fileNames
	^self files collect: [:each | each name]! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:31'!
files
	^self children select: [:each | each isFile]! !

!DirectoryEntry methodsFor: 'testing' stamp: 'pb 5/25/2016 00:31'!
exists
	
	| pathName |
	(self fileAccessor isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^self fileAccessor drives includes: self ].

	self isRoot ifTrue: [ ^true ].
	
	pathName _ self pathName.
	pathName = self fileAccessor slash ifTrue: [ ^ true ].

	^self fileAccessor basicDirectoryExists: pathName! !

!DirectoryEntry methodsFor: 'testing' stamp: 'pb 5/25/2016 00:31'!
isDirectory
	^true! !

!DirectoryEntry methodsFor: 'testing' stamp: 'pb 5/25/2016 00:31'!
isFile
	^false! !

!DirectoryEntry methodsFor: 'initialize-release' stamp: 'pb 5/25/2016 00:31'!
refresh
	super refresh.
	self refreshChildren! !

!DirectoryEntry methodsFor: 'initialize-release' stamp: 'pb 5/25/2016 00:31'!
refreshChildren
	
	children := nil! !

!DirectoryEntry methodsFor: 'cuis specific' stamp: 'pb 5/25/2016 00:34'!
directoryMatching: pattern
"
	DirectoryEntry smalltalkImageDirectory directoryMatching: 'C*Pack*'.
"
	| dirNameFound |

	dirNameFound := 
		self directoryNames 
				detect: [ :dirName | pattern match: dirName ] 
				ifNone: [nil].
	^ dirNameFound ifNotNil: [ :dirName | self / dirName ] ifNil: [ nil ]! !

!DirectoryEntry methodsFor: 'cuis specific' stamp: 'pb 5/25/2016 00:34'!
directoryNamesMatching: pat
	"
	DirectoryEntry currentDirectory directoryNamesMatching: '*'
	"

	^ self directoryNames select: [ :each | pat match: each ]! !

!DirectoryEntry methodsFor: 'cuis specific' stamp: 'pb 5/25/2016 00:34'!
fileNamesMatching: pat
	"
	DirectoryEntry currentDirectory fileNamesMatching: '*'
	"

	^ self fileNames select: [ :each | pat match: each ]! !

!DirectoryEntry methodsFor: 'cuis specific' stamp: 'pb 5/25/2016 00:31'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version candidate |

	files _ self fileNamesMatching: (baseFileName,'*.', extension).
	splits _ files collect: [ :file | self fileAccessor splitNameVersionExtensionFor: file ].
	splits _ splits asArray sort: [ :a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [ version _ 1 ]
			ifFalse: [ version _ (splits last at: 2) + 1 ].
	candidate _ (baseFileName, fileNameCoda, '.', version asString, '.', extension) asFileName.
	"all the above seems to fail on Cog on Linux. It looks like the OS file list is not updated immediately.
	To see this, just do 'file out and keep' twice on your current change set."
	[ candidate asFileEntry exists ] whileTrue: [
		version _ version + 1.
		candidate _ (baseFileName, fileNameCoda, '.', version asString, '.', extension) asFileName ].
	^ candidate! !

!DirectoryEntry methodsFor: 'cuis specific' stamp: 'pb 5/25/2016 00:31'!
nextNameFor: baseFileName extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	^self nextNameFor: baseFileName coda: '' extension: extension! !


!DirectoryEntry class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 00:35'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory

	DirectoryEntry currentDirectory
	"

	CurrentDirectory ifNil: [
		CurrentDirectory _ self pathName: (Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]) ].
	^ CurrentDirectory! !

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 00:35'!
roots
	"Appropriate for all systems, including those with several roots, each being a logical 'drive' (Like Windows),
	and for systems having a single root with file systems (i.e. 'drives') mounted anywhere in the tree (Unix, MacOS)
	"
	"
	DirectoryEntry roots
	"
	^ FileIOAccessor default drives
		ifEmpty: [
			"On Linux and MacOsX"
			{ '/' asDirectoryEntry } ]! !

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 00:35'!
smalltalkImageDirectory
	"Answer the directory on which this Smalltalk image was started (or last saved)

	DirectoryEntry smalltalkImageDirectory
	"

	ImageDirectory ifNil: [
		ImageDirectory _ self pathName: Smalltalk imagePath ].
	^ ImageDirectory! !

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 00:35'!
vmDirectory
	"Answer the directory containing the VM that runs us.

	DirectoryEntry vmDirectory
	"

	VMDirectory ifNil: [
		VMDirectory _ self pathName: Smalltalk vmPath ].
	^ VMDirectory! !

!DirectoryEntry class methodsFor: 'cached state access' stamp: 'pb 5/25/2016 00:31'!
releaseClassCachedState

	ImageDirectory _ nil.
	VMDirectory _ nil.
	CurrentDirectory _ nil! !


!FileEntry methodsFor: 'actions-rio' stamp: 'pb 5/25/2016 00:31'!
< aStringOrBytes 
	self fileContents: aStringOrBytes! !

!FileEntry methodsFor: 'actions-rio' stamp: 'pb 5/25/2016 00:31'!
<< aStringOrBytes 
	self appendContents: aStringOrBytes! !

!FileEntry methodsFor: 'enumeration' stamp: 'jmv 5/31/2016 11:24'!
assureExistence
	self exists ifTrue: [^self].
	self parent assureExistence.
	self forceWriteStreamDo: [ :stream | ]! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 01:42'!
appendContents: aStringOrBytes 
	self
		appendStreamDo: [:str | 
			aStringOrBytes isString
				ifFalse: [str binary].
			str nextPutAll: aStringOrBytes]! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 01:32'!
binaryContents
	| answer |
	self readStreamDo: [ :stream |
		answer _ stream binary contents ].
	^ answer! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 5/31/2016 11:24'!
binaryContents: aByteArray
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream binary to: aByteArray ].
	self refresh! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 00:31'!
copyTo: filename 
	| targetEntry |
	
	targetEntry := filename asFileEntry.
	targetEntry isDirectory
		ifTrue: [ targetEntry := targetEntry // self name ].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 00:31'!
delete
	self fileAccessor deleteFile: self pathName.
	! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 00:31'!
fileContents
	"Default is text mode"
	^self textContents! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 00:31'!
fileContents: aStringOrBytes 
	aStringOrBytes isString
		ifTrue: [self textContents: aStringOrBytes]
		ifFalse: [self binaryContents: aStringOrBytes]! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 00:31'!
formContents
	^Form fromFileEntry: self! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 00:31'!
rename: newName
	
	self fileAccessor rename: self pathName to: (self parent // newName) pathName.
	self name: newName! !

!FileEntry methodsFor: 'actions-file' stamp: 'pb 5/25/2016 01:32'!
textContents
	| answer |
	self readStreamDo: [ :stream |
		answer _ stream contents ].
	^ answer! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 5/31/2016 11:24'!
textContents: aString
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: aString ].
	self refresh! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'pb 5/25/2016 01:46'!
appendStreamDo: blockWithArg 
	| stream |
	stream _ self appendStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/31/2016 11:23'!
forceWriteStreamDo: blockWithArg 
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self forceWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'pb 5/25/2016 01:50'!
readStreamDo: blockWithArg 
	"Raise FileDoesNotExistException if not found."
	| stream result |
	stream _ self readStream.
	[ result _ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]].
	^ result! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'pb 5/25/2016 01:53'!
writeStreamDo: blockWithArg 
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self writeStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FileEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 01:53'!
appendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamDo: that closes the file for you."

	self exists ifFalse: [
		^ self writeStream ].
	^ (self fileAccessor privateWriteableFile: self) setToEnd! !

!FileEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:31'!
fileSize
	"Note: as this value is cached, it will not be updated if the file changes... mhhh...."

	"Slow, original version that reads values from containing directory."
	"fileSize ifNil: [self initValuesFromParent]."

	"Fast version, that asks just for the size of this file.
	Used if I was not created by reading a direcotry"
	fileSize ifNil: [fileSize _ self fileAccessor fileSize: self].

	^fileSize! !

!FileEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 00:31'!
fileSize: value
	fileSize := value! !

!FileEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 01:45'!
readStream
	"Raise FileDoesNotExistException if not found.
	Note: You need to eventually close the stream.
	Usually prefer #readStreamDo: that closes the file for you."

	^ self fileAccessor privateReadOnlyFile: self! !

!FileEntry methodsFor: 'accessing' stamp: 'pb 5/25/2016 01:46'!
writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateNewFile: self! !

!FileEntry methodsFor: 'private' stamp: 'jmv 5/31/2016 11:23'!
forceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateForceNewFile: self! !

!FileEntry methodsFor: 'private' stamp: 'pb 5/25/2016 00:31'!
initValuesFrom: otherEntry
	otherEntry ifNil: [^self].
	super initValuesFrom: otherEntry.
	self fileSize: otherEntry fileSize! !

!FileEntry methodsFor: 'testing' stamp: 'pb 5/25/2016 00:31'!
exists
"
	| fileNames |
	fileNames := self fileAccessor fileNamesIn: self parent.
	^self fileAccessor isCaseSensitive 
		ifTrue: [ fileNames includes: self name ]
		ifFalse: [ fileNames anySatisfy: [ :n | n sameAs: self name ]]
"
	^self fileAccessor fileExists: self! !

!FileEntry methodsFor: 'testing' stamp: 'pb 5/25/2016 00:31'!
isFile
	^true! !

!FileEntry methodsFor: 'initialize-release' stamp: 'pb 5/25/2016 00:31'!
refresh
	super refresh.
	fileSize := nil! !

!FileEntry methodsFor: 'actions-pipe' stamp: 'pb 5/25/2016 01:53'!
pipe: filterBlock to: filename 
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	filterBlock value: inStream value: outStream]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry
! !

!FileEntry methodsFor: 'actions-pipe' stamp: 'pb 5/25/2016 00:31'!
pipeRepeat: filterBlock to: filename 
	^self pipeRepeat: filterBlock while: [:in :out | in atEnd not] to: filename ! !

!FileEntry methodsFor: 'actions-pipe' stamp: 'pb 5/25/2016 01:53'!
pipeRepeat: filterBlock while: terminateBlock to: filename 
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	[terminateBlock value: inStream value: outStream]
		whileTrue: [filterBlock value: inStream value: outStream]]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry! !

!FileEntry methodsFor: 'cuis extensions' stamp: 'pb 5/25/2016 01:32'!
form
	"Read a Form or ColorForm from the given file.
	'../../4.2.04.tiff' asFileEntry form display
	"

	| form |
	self readStreamDo: [ :stream |
		form _ Form fromBinaryStream: stream binary ].
	^ form! !


!FileEntry class methodsFor: 'examples' stamp: 'pb 5/25/2016 00:38'!
example1
	"FileEntry example1"

	"- Create subDirectory named: 'subDir'.
	 - Put a new file named: 'file1'.
	 - Write contents 'Hello!! to that file'"

	"Traditional way (Squeak)"
	"| subDir str |
	subDir := FileDirectory smalltalkImageDirectory directoryNamed: 'subDir'.
	subDir assureExistence.
	[str := subDir newFileNamed: 'file1'.
	str nextPutAll: 'Hello!!']
		ensure: [str close]."

	"FileMan"

	'./subDir' asDirectoryEntry at: 'file2' put: 'Hello!!'! !

!FileEntry class methodsFor: 'examples' stamp: 'pb 5/25/2016 00:38'!
example2
	"
	FileEntry example2
	"

	"FileMan's path representation is portable"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') at: 'test1' put: 'Hello2!!'.! !

!FileEntry class methodsFor: 'examples' stamp: 'pb 5/25/2016 00:39'!
example3
	"FileEntry example3"

	"Remove 'test1' file created exapmle2"
	('./subDir' asDirectoryEntry / 'aaa\bbb' / 'ccc' / 'ddd\eee' / 'fff:ggg') removeKey: 'test1'.

	"Recursive delete"
	'./subDir' asDirectoryEntry recursiveDelete! !

!FileEntry class methodsFor: 'examples' stamp: 'pb 5/25/2016 00:39'!
example5
	"FileEntry example5"
	
	"Write test1 file and copy it to test2 in the parent directory"

	'./test1' fileContents: 'This is a test'.
	'./test1' asFileEntry copyTo: '../test2'.

	'../test2' fileContents inspect! !

!FileEntry class methodsFor: 'examples' stamp: 'pb 5/25/2016 00:39'!
example6
	"FileEntry example6"
	
	"test1 file contents will be written to test2 using reverse filter.
	 test2 file contents will be written to test3 again using reverse filter."

	| reverseFilter |
	'test1.txt' fileContents: 'This is a test'.
	reverseFilter := [:in :out | out nextPutAll: (in upToEnd reverse)].

	('test1.txt' asFileEntry pipe: reverseFilter to: 'test2.txt')
		pipe: reverseFilter to: 'test3.txt'.

	(#('test1.txt' 'test2.txt' 'test3.txt') collect: [:each | each fileContents]) inspect

	! !

!FileEntry class methodsFor: 'examples' stamp: 'pb 5/25/2016 00:39'!
example7
	"FileEntry example7"
	"Useful example: copy latest mcz files to releaseDir (for preparing SAR)"

	| releaseDir dir fileNames |
	releaseDir := './releasePkg' asDirectoryEntry.
	releaseDir keys isEmpty ifFalse: [releaseDir recursiveDelete].
	dir := './package-cache' asDirectoryEntry.
	fileNames := ((dir keys collect: [:each | each name readStream upTo: $.]) select: [:each | each notEmpty]) asSet.
	(fileNames collect: [:eachName | dir
				latestFileMatches: [:each | each name beginsWith: eachName]])
		do: [:eachEnt | eachEnt copyTo: releaseDir]! !


!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/31/2016 13:05'!
appendStreamDo: blockWithArg 
	| stream |
	stream _ self privateAppendStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/31/2016 13:05'!
forceWriteStreamDo: blockWithArg 
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self privateForceWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/31/2016 13:05'!
readStreamDo: blockWithArg 
	"Raise FileDoesNotExistException if not found."
	| stream result |
	stream _ self privateReadStream.
	[ result _ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]].
	^ result! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 5/31/2016 13:05'!
writeStreamDo: blockWithArg 
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self privateWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !


!ChangeList methodsFor: 'menu actions' stamp: 'pb 5/25/2016 01:36'!
fileOutSelections 
	
	(FillInTheBlankMorph
		request: 'Enter file name'
		initialAnswer: 'Filename.st'
		onCancel: [^nil])

			asFileEntry writeStreamDo: [ :stream |
				stream timeStamp.
				listSelections with: changeList do: [ :selected :item |
					selected ifTrue: [
						item fileOutOn: stream ]]]! !


!ChangeList class methodsFor: 'public access' stamp: 'pb 5/25/2016 01:30'!
browseContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList fileSize charCount |
	aFileEntry readStreamDo: [ :stream |
		fileSize _ stream size.
		charCount _ fileSize.
		charCount > 1000000 ifTrue: [
			(self confirm: 'The file ', aFileEntry name , '
is really long (' , charCount printString , ' characters).
Would you prefer to view only the last million characters?')
				ifTrue: [ charCount _ 1000000 ]].
		changeList _ self new
			scanFile: stream from: fileSize-charCount to: fileSize.
	].
	ChangeListWindow open: changeList label: aFileEntry name! !

!ChangeList class methodsFor: 'public access' stamp: 'pb 5/25/2016 01:31'!
browsePackageContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	aFileEntry readStreamDo: [ :stream |
		changeList _ self new scanFile: stream from: 0 to: stream size.
		stream reset.
		packageFile _ CodePackageFile buildFileStream: stream.
	].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name! !

!ChangeList class methodsFor: 'public access' stamp: 'pb 5/25/2016 01:31'!
browseRecent: charCount on: origChangesFileName 
	"Opens a changeList on the end of the specified changes log file"
	
	| changeList end |
	origChangesFileName asFileEntry readStreamDo: [ :changesFile |
		end _ changesFile size.
		changeList _ self new
			scanFile: changesFile
			from: (0 max: end - charCount)
			to: end.
	].
	ChangeListWindow open: changeList label: 'Recent changes'! !

!ChangeList class methodsFor: 'public access' stamp: 'pb 5/25/2016 01:31'!
browseRecentLogOn: origChangesFileName 
	"figure out where the last snapshot or quit was, then browse the recent entries."

	| end done block positions prevBlock |
	origChangesFileName asFileEntry readStreamDo: [ :changesFile |
		positions _ SortedCollection new.
		end _ changesFile size.
		prevBlock _ end.
		block _ end - 1024 max: 0.
		done _ false.
		[ done or: [ positions size > 0 ]]
			whileFalse: [
				changesFile position: block.
				"ignore first fragment"
				changesFile nextChunk.
				[ changesFile position < prevBlock ]
					whileTrue: [
						| pos chunk |
						pos _ changesFile position.
						chunk _ changesFile nextChunk.
						((chunk indexOfSubCollection: '----' startingAt: 1) = 1) ifTrue: [
							({ '----QUIT'. '----SNAPSHOT' } anySatisfy: [ :str |
								chunk beginsWith: str ])
									ifTrue: [ positions add: pos ]]].
				block = 0
					ifTrue: [done _ true]
					ifFalse: [
						prevBlock _ block.
						block _ block - 1024 max: 0]].
	].
	positions isEmpty
		ifTrue: [self inform: 'File ' , origChangesFileName , ' does not appear to be a changes file']
		ifFalse: [self browseRecentLogOn: origChangesFileName startingFrom: positions last]! !

!ChangeList class methodsFor: 'public access' stamp: 'pb 5/25/2016 01:31'!
browseRecentLogOn: origChangesFileName startingFrom: initialPos 
	"Prompt with a menu of how far back to go when browsing a changes file."

	| end banners positions pos chunk i |
	origChangesFileName asFileEntry readStreamDo: [ :changesFile |
		banners _ OrderedCollection new.
		positions _ OrderedCollection new.
		end _ changesFile size.
		pos _ initialPos.
		[pos = 0
			or: [banners size > 20]]
			whileFalse: [changesFile position: pos.
				chunk _ changesFile nextChunk.
				i _ chunk indexOfSubCollection: 'priorSource: ' startingAt: 1.
				i > 0
					ifTrue: [positions addLast: pos.
						banners
							addLast: (chunk copyFrom: 5 to: i - 2).
						pos _ Number
									readFrom: (chunk copyFrom: i + 13 to: chunk size)]
					ifFalse: [pos _ 0]].
	].
	banners size = 0 ifTrue: [^ self inform: 
'this image has never been saved
since changes were compressed'].
	pos _ (SelectionMenu labelList: banners selections: positions)
				startUpWithCaption: 'Browse as far back as...'.
	pos
		ifNil: [^ self].
	self browseRecent: end - pos on: origChangesFileName! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'pb 5/25/2016 01:51'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position prevPos prevFileIndex preamble tokens sourceFilesCopy stamp |
	selectorOfMethod _ selector.
	currentCompiledMethod _ method.
	classOfMethod _ meta ifTrue: [class class] ifFalse: [class].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	self addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex _ 0.
	position _ method filePosition.
	sourceFilesCopy _ SourceFiles collect:
		[:x | x ifNotNil: [ x name asFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file _ sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil]
		whileTrue:
		[file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos _ nil.
		stamp _ ''.
		(preamble findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens _ Scanner new scanTokens: preamble]
			ifFalse: [tokens _ Array new  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue: [
				(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokens size-2.
						prevPos _ tokens last.
						prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos _ tokens at: tokens size-2.
						prevFileIndex _ tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos _ nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue: [
				(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokens size]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position _ prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'pb 5/25/2016 01:50'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod _ class.
	oldCommentRemoteStr _ class  organization commentRemoteStr.
	currentCompiledMethod _ oldCommentRemoteStr.
	selectorOfMethod _ #Comment.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x name asFileEntry readStream]].
	position _ oldCommentRemoteStr position.
	file _ sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		file position: (0 max: position-150).  " Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		prevPos _ nil.
		stamp _ ''.
		(preamble findString: 'commentStamp:' startingAt: 1) > 0
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				(tokens at: tokens size-3) = #commentStamp: ifTrue: [
					"New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokens size-2.
					prevPos _ tokens last.
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp _ '<historical>'].
 		self addItem:
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos _ nil ].
		position _ prevPos.
		prevPos notNil ifTrue: [ file _ sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections! !


!FileList methodsFor: 'initialization' stamp: 'jmv 5/31/2016 10:37'!
directory: aDirectoryEntry
	"Set the path of the volume to be displayed."

	directory _ aDirectoryEntry.

	sortMode ifNil: [
		sortMode _ #date.
		sortAscending _ false ].
	self changed: #relabel.
	self pattern: pattern! !

!FileList methodsFor: 'initialization' stamp: 'pb 5/25/2016 01:10'!
initialDirectoryList

	| dirList |
	dirList _ DirectoryEntry roots collect: [ :each |
		FileDirectoryWrapper with: each name: (each name ifNil: ['/']) model: self].
	dirList isEmpty ifTrue: [
		dirList _ Array with: (FileDirectoryWrapper 
			with: directory
			name: directory localName 
			model: self)].
	^dirList! !

!FileList methodsFor: 'own services' stamp: 'pb 5/25/2016 01:32'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"
	
	| aString aName |


	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		aString _ stream next: stream size.
		aName _ stream localName ].

	(Workspace new contents: aString) openLabel: 'Workspace from ', aName! !

!FileList methodsFor: 'private' stamp: 'pb 5/25/2016 01:37'!
put: aText
	"Private - put the supplied text onto the file"

	| nameUsed type |
	brevityState == #fullFile ifTrue: [
		directory // fileName writeStreamDo: [ :stream |
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

!FileList methodsFor: 'private' stamp: 'pb 5/25/2016 01:32'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first50000 |

directory // fileName readStreamDo: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= 2000000]) ifTrue: [
		acceptedContentsCache _ f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	first50000 := f next: 50000.
].
	acceptedContentsCache _
'File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 50000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 50000 characters.' format: {fileName. fileSize. first50000}.
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'pb 5/25/2016 01:32'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData |

	directory // fileName readStreamDo: [ :stream |
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

Here are the first 10000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 10000 characters.' format: {fileName. size. hexData}]
		ifFalse: [hexData])! !


!FileList class methodsFor: 'file reader registration' stamp: 'pb 5/25/2016 01:20'!
itemsForFile: filename
	"Answer a list of services appropriate for a file of the given name"

	| services suffix |
	suffix _ (FileIOAccessor default extensionFor: filename) asLowercase.
	services _ OrderedCollection new.
	FileReaderRegistry do: [ :reader |
		reader ifNotNil: [
			services addAll: (reader fileReaderServicesForFile: filename suffix: suffix)]].
	^ services! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'pb 5/25/2016 01:36'!
fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry smalltalkImageDirectory // (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'pb 5/25/2016 01:36'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	nameBody _ self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry smalltalkImageDirectory // (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ]! !


!Class methodsFor: 'fileIn/Out' stamp: 'pb 5/25/2016 01:36'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	DirectoryEntry smalltalkImageDirectory // (self name, '.st') writeStreamDo: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self fileOutOn: stream moveSource: false toFile: 0 ]! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'pb 5/25/2016 01:38'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	((Smalltalk imageName withoutSuffix: '.image'), '-AllCode.st') asFileEntry writeStreamDo: [ :stream |
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: stream ]]! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'pb 5/25/2016 01:38'!
fileOutCategory: category
	"FileOut all the classes in the named system category."

	DirectoryEntry smalltalkImageDirectory // (category asFileName , '.st') writeStreamDo: [ :fileStream |
		self fileOutCategory: category on: fileStream initializing: true ]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'pb 5/25/2016 01:36'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asCamelCase. 
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [
					DirectoryEntry currentDirectory
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].
		
	nameToUse asFileEntry writeStreamDo: [ :stream |
		stream timeStamp.
		self fileOutPreambleOn: stream.
		self fileOutOn: stream.
		self fileOutPostscriptOn: stream ].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips _ self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 5/31/2016 11:15'!
fileIn: aFileEntry
	"File in the entire contents of the file specified by the name provided"

	aFileEntry ifNil: [^ Smalltalk beep ].
	aFileEntry readStreamDo: [ :stream |
		stream fileIn ]! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 5/31/2016 10:42'!
install: aFileEntry
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	ChangeSet installing: aFileEntry name do: [ self fileIn: aFileEntry ].
	('Installed ChangeSet: ', aFileEntry name) print! !

!ChangeSet class methodsFor: 'services' stamp: 'pb 5/25/2016 01:08'!
installNewUpdates
 	
	| updatesFileDirectory |

	updatesFileDirectory _ DirectoryEntry smalltalkImageDirectory / 'CoreUpdates'.
	updatesFileDirectory exists ifFalse: [
		updatesFileDirectory _ DirectoryEntry smalltalkImageDirectory parent / 			'Cuis-Smalltalk-Dev/CoreUpdates' ].

	updatesFileDirectory exists
		ifFalse: [ self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ] 
		ifTrue: [ ChangeSet installNewUpdates: updatesFileDirectory ]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'pb 5/25/2016 01:36'!
fileOut

	(FillInTheBlankMorph request: 'Enter the file name' initialAnswer:'') asFileEntry writeStreamDo: [ :stream | 
		sourceSystem isEmpty ifFalse:[
			stream nextChunkPut: sourceSystem printString; newLine ].
		self fileOutOn: stream.
		stream newLine; newLine.
		classes do: [ :cls |
			cls needsInitialize ifTrue: [
				stream newLine; nextChunkPut: cls name,' initialize']].
		stream newLine ]! !


!CodePackage methodsFor: 'testing' stamp: 'pb 5/25/2016 01:50'!
changeRecordForOverriddenMethod: aMethodReference
	| sourceFilesCopy method position |
	method := aMethodReference actualClass compiledMethodAt: aMethodReference methodSymbol.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x isNil ifTrue: [ nil ]
				ifFalse: [x name asFileEntry readStream]].
	[ | file prevPos prevFileIndex chunk stamp methodCategory tokens |
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [ chunk _ file nextChunk ].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos := nil.
		stamp := ''.
		(chunk findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens := Scanner new scanTokens: chunk]
			ifFalse: [tokens := #()  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size-2.
						prevPos := tokens last.
						prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos := sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos := tokens at: tokens size-2.
						prevFileIndex := tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos := nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size]].
		methodCategory := (tokens after: #methodsFor:) ifNil: ['as yet unclassifed'].
		(self includesMethodCategory: methodCategory ofClass: aMethodReference actualClass) ifTrue:
			[methodCategory = (Smalltalk at: #Categorizer ifAbsent: [Smalltalk at: #ClassOrganizer]) default ifTrue: [methodCategory := methodCategory, ' '].
			^ ChangeRecord new file: file position: position type: #method
						class: aMethodReference classSymbol category: methodCategory meta: aMethodReference classIsMeta stamp: stamp].
		position := prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
		^ nil]
			ensure: [sourceFilesCopy do: [:x | x notNil ifTrue: [x close]]]
	! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 5/31/2016 11:23'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFileEntry parent exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName _
			(DirectoryEntry smalltalkImageDirectory // self packageFileName) pathName ].

	fullFileName asFileEntry forceWriteStreamDo: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'pb 5/25/2016 01:49'!
timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	"(CompiledMethod compiledMethodAt: #timeStamp) timeStamp"

	| file preamble stamp tokens tokenCount |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ String new].  "sources file not available"
	"file does not exist happens in secure mode"
	file _ [file name asFileEntry readStream] on: FileDoesNotExistException do: [ :ex| nil ].
	file ifNil: [^ String new].
	preamble _ self getPreambleFrom: file at: (0 max: self filePosition).
	stamp _ String new.
	tokens _ (preamble findString: 'methodsFor:' startingAt: 1) > 0
		ifTrue: [Scanner new scanTokens: preamble]
		ifFalse: [Array new  "ie cant be back ref"].
	(((tokenCount _ tokens size) between: 7 and: 8) and: [(tokens at: tokenCount - 5) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount - 3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokenCount - 2]].
	((tokenCount between: 5 and: 6) and: [(tokens at: tokenCount - 3) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount  - 1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokenCount]].
	file close.
	^ stamp! !


!String methodsFor: 'converting' stamp: 'pb 5/25/2016 01:21'!
asFileName
	"Answer a String made up from the receiver that is an acceptable file 
	name."

	^FileIOAccessor default checkName: self fixErrors: true! !

!String methodsFor: 'arithmetic' stamp: 'pb 5/25/2016 01:13'!
/ arg
	"If working with file paths, just use $/
	Or better yet, use DirectoryEntry protocol"

	self shouldNotImplement! !

!String methodsFor: '*fileman-core-converting' stamp: 'pb 5/25/2016 01:21'!
asAbsolutePathName
	"See comment at #isAbsolutePathName"

	| slash |
	slash _ FileIOAccessor default slash.
	^ String streamContents: [ :childPath |
		childPath nextPutAll: slash.
		(FileIOAccessor default absolutePathComponentsFor: self)
			do: [ :each | childPath nextPutAll: each]
			separatedBy: [childPath nextPutAll: slash]]! !

!String methodsFor: '*fileman-core-converting' stamp: 'pb 5/25/2016 01:13'!
asDirectoryEntry
	"See examples in #asFileEntry method comment"
	^DirectoryEntry pathName: self! !

!String methodsFor: '*fileman-core-converting' stamp: 'pb 5/25/2016 01:21'!
asDriveName
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)
	
Windows
	'C:\' asDriveName 'C:'
	'NotAChance' asDriveName nil
	
Linux
	'/media/cdrom' asDriveName nil

MacOsX
    '/SanDisk32-NTFS' asDriveName nil
	
	"

	| candidate |
	FileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].

	(FileIOAccessor default onMacClassic) ifTrue: [
		candidate _ self upToFirstPathSeparator.
		"Aparently on Mac Classic, 
			xxx/yyy means xxx must be a drive name
			/xxx/yyy means xxx could be any folder in root. Check to make sure!!
		"
		('/' asDirectoryEntry directoryNames includes: candidate) ifTrue: [
			^candidate ]].
	
	^ nil! !

!String methodsFor: '*fileman-core-converting' stamp: 'pb 5/25/2016 01:19'!
asFileEntry
	"

Windows	
	'C:\Windows' asFileEntry exists false
	'C:\Windows' asDirectoryEntry exists true
	'/' asFileEntry exists false
	'/' asDirectoryEntry exists false
	'C:\' asFileEntry exists false
	'C:\' asDirectoryEntry exists true
	('C:' asDirectoryEntry // 'Windows') exists false
	('C:' asDirectoryEntry / 'Windows') exists true
	
Linux
    '/var' asFileEntry exists
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists true
    '/media/cdrom' asFileEntry exists false
    '/media/cdrom' asDirectoryEntry exists true
    ('/bin' asDirectoryEntry / 'more') exists false
    ('/bin' asDirectoryEntry // 'more') exists true

MacOsX
    '/var' asFileEntry exists false
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists  true
    '/Volumes/SanDisk32-NTFS' asFileEntry exists false
    '/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
    'SanDisk32-NTFS' asFileEntry exists false
    'SanDisk32-NTFS' asDirectoryEntry exists false
	
	"
	self isRelativeMark ifTrue: [ ^self error: 'Maybe you need to call #asDirectoryEntry!!' ].
	^FileEntry pathName: self! !

!String methodsFor: '*fileman-core-testing' stamp: 'pb 5/25/2016 01:21'!
isAbsolutePathName
	"Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt'
	and even '/Users/Someone/file.txt' are considered an absolute pathName.
	This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice."
	| upperName |
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].

	FileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	FileIOAccessor default onMacClassic ifTrue: [
		upperName := self asUppercase.
		^'/' asDirectoryEntry directoryNames anySatisfy: [ :each |
			(upperName beginsWith: each)
					and: [| nextPos | 
						nextPos := each size + 1 min: self size max: 1.
						(self at: nextPos) isPathSeparator ]]].

	^ false! !

!String methodsFor: '*fileman-core-testing' stamp: 'pb 5/25/2016 01:21'!
isDriveName
	FileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	FileIOAccessor default onMacClassic ifTrue: [
		^'/' asDirectoryEntry directoryNames includes: self].

	^false! !


!ContentPack methodsFor: 'importing' stamp: 'pb 5/25/2016 01:09'!
path: aString 
	
	| contentPacks directory |

	self flag: #todo. "Consider renaming this method. --cbr"

	directory _ DirectoryEntry smalltalkImageDirectory / aString.

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

!ContentPack methodsFor: 'private' stamp: 'pb 5/25/2016 01:08'!
exportDirectory
	
	^ DirectoryEntry smalltalkImageDirectory / self class defaultContentDirectory / 'Exported'! !

!ContentPack methodsFor: 'private' stamp: 'jmv 5/31/2016 10:35'!
supportedFilesIn: aDirectoryEntry

	| fileTypes supportedFiles |
		fileTypes _ (self class mapping as: Dictionary) values.
		supportedFiles _ Set new.
	
	fileTypes do: [ :type | 
		supportedFiles _ supportedFiles
			union: (aDirectoryEntry files select: [ :entry |
				'*.' , type match: entry name ])
	].

	^ supportedFiles! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/31/2016 11:24'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
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
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFileEntry appendStream.

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'pb 5/25/2016 01:38'!
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
	newSourcesName asFileEntry writeStreamDo: [ :f |
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
		DirectoryEntry smalltalkImageDirectory // oldChangesName rename: oldChangesName, '.old' ].
	newChangesName _ self defaultChangesName.
	newChangesName asFileEntry writeStreamDo: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'pb 5/25/2016 01:21'!
defaultChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), '.changes'! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'pb 5/25/2016 01:14'!
fullNameForChangesNamed: aName
	"
	Smalltalk fullNameForChangesNamed: 'newChanges'
	"
	| newName |
	newName _ FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.changes'! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'pb 5/25/2016 01:14'!
fullNameForImageNamed: aName
	"
	Smalltalk fullNameForImageNamed: 'newImage'
	"
	| newName |
	newName _ FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.image'! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 5/31/2016 11:24'!
logError: errMsg inContext: aContext to: localFileName
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	[
		DirectoryEntry smalltalkImageDirectory // localFileName forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: Error do: [] "avoid recursive errors"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'pb 5/25/2016 01:21'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFileEntry to: newChangesName asFileEntry ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'pb 5/25/2016 01:15'!
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
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ DirectoryEntry smalltalkImageDirectory
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

!SystemDictionary methodsFor: 'sources, change log' stamp: 'pb 5/25/2016 01:51'!
externalizeSources   
	"Write the sources and changes streams onto external files."
	"
	Smalltalk externalizeSources
	"

	| sourcesName changesName |

	sourcesName _ self defaultSourcesName.
	sourcesName asFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles first originalContents ].
	SourceFiles at: 1 put: sourcesName asFileEntry readStream.

	changesName _ self defaultChangesName.
	changesName  asFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles last contents ].
	SourceFiles at: 2 put: changesName asFileEntry appendStream.

	self inform: 'Sources successfully externalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'pb 5/25/2016 01:51'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes msg wmsg entry |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		entry _ Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry _ Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources _ [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , entry pathName) ].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		[ entry appendStream ] on: FileWriteError do: [ nil ] ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].

	SourceFiles _ Array with: sources with: changes! !

!SystemDictionary methodsFor: 'image format' stamp: 'pb 5/25/2016 01:34'!
imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	^ Smalltalk imageName asFileEntry readStreamDo: [ :stream |
		(stream binary; next: 4)
			unsignedLongAt: 1
			bigEndian: Smalltalk isBigEndian ]! !

!SystemDictionary methodsFor: 'startup' stamp: 'pb 5/25/2016 01:34'!
processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
	Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''
	"
	| p data entry |
	p _ rawArgStream next.

	(p first = $- and: [ p size > 1 ]) ifTrue: [
		"If the command is not included in p, it is next argument"
		p size = 2
			ifTrue: [
				"as in 		-r RequiredFeature1"
				data _ rawArgStream next ]
			ifFalse: [
				"as in 		-rRequiredFeature2"
				data _ p copyFrom: 3 to: p size ].
		p second caseOf: {
			[ $r ] -> [		"as in 		-rRequiredFeature2"
				{ 'Feature require: '. data } print.
				[ Feature require: data ] on: Error do: [] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Compiler evaluate: data ] on: Error do: [] ].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				entry _ data asFileEntry.
				entry exists ifTrue: [
					entry readStreamDo: [ :stream |
						[ Compiler evaluate: stream contentsOfEntireFile ] on: Error do: []]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]! !


!UnhandledError methodsFor: 'priv handling' stamp: 'pb 5/25/2016 01:38'!
runtimeDefaultAction
	"Dump the stack trace to a log file, then exit the program (image)."

	('error', Utilities dateTimeSuffix, '.log') asFileName asFileEntry writeStreamDo: [ :file |
		Smalltalk timeStamp: file.
		(thisContext sender stackOfSize: 20) do: [ :ctx | file newLine. ctx printOn: file ]].

	Smalltalk snapshot: false andQuit: true clearAllClassState: false! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'pb 5/25/2016 01:32'!
requirements
	"Answer my requirements"
	
	| answer |
	pathName asFileEntry readStreamDo: [ :stream |
		answer _ (CodePackageFile buildFileStream: stream) requires ].
	^ answer! !

!FeatureRequirement methodsFor: 'requires' stamp: 'pb 5/25/2016 01:32'!
install
	"Preconditions have been satisfied.  Install the required package."
	pathName asFileEntry readStreamDo: [ :stream |
		CodePackageFile basicInstallPackageStream: stream ].

	"No need to have a platform and machine specific path around anymore. It was just for installation. Clear it."
	pathName _ nil! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 5/31/2016 10:36'!
inPackagesSubtreeOf: aDirectoryEntry do: aBlock

	| pckDir compatPckDir |

	"Look in the requested directory"
	aBlock value: aDirectoryEntry.

	"Look in the usual Packages subfolders"
	pckDir _ aDirectoryEntry / 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	compatPckDir _ aDirectoryEntry / 'CompatibilityPackages'.
	compatPckDir exists ifTrue: [
		aBlock value: compatPckDir ].

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	aDirectoryEntry children do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			aBlock value: entry.
			pckDir _ entry / 'Packages'.
			pckDir exists ifTrue: [
				aBlock value: pckDir ].
			compatPckDir _ entry / 'CompatibilityPackages'.
			compatPckDir exists ifTrue: [
				aBlock value: compatPckDir ]]]! !

!FeatureRequirement methodsFor: 'private' stamp: 'pb 5/25/2016 01:10'!
placesToLookForPackagesDo: aBlock

	| base myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir _ pathName asFileEntry parent.
		aBlock value: myDir ].

	"Look in Cuis image folder and reasonable subfolders"
	base _ DirectoryEntry smalltalkImageDirectory.
	self inPackagesSubtreeOf: base do: aBlock.
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub"
	self inPackagesSubtreeOf: base parent do: aBlock.

	"Also look in host OS current directory"
	base _ DirectoryEntry currentDirectory.
	self inPackagesSubtreeOf: base do: aBlock! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 5/31/2016 11:24'!
writeOnFileNamed: fileName 
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."

	fileName asFileEntry forceWriteStreamDo: [ :file |
		file binary.
		file nextPut: 2.  "file code = 2"
		self writeOn: file ].

"
 | f |
[(f _ Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"! !


!Form class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:33'!
fromFileEntry: aFileEntry
	"Read a Form or ColorForm from the given file."

	| form |
	aFileEntry readStreamDo: [ :stream |
		form _ self fromBinaryStream: stream binary ].
	^ form! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'pb 5/25/2016 01:33'!
formFromFileNamed: fileName
	"Answer a ColorForm stored on the file with the given name."
	
	^fileName asFileEntry readStreamDo: [ :stream |
		stream useBytes.
		self formFromStream: stream ]! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 5/31/2016 11:24'!
putForm: aForm onFileNamed: filename
	"Store the given form on a file of the given name."

	filename asFileEntry forceWriteStreamDo: [ :stream |
		(self onBinaryStream: stream binary) nextPutImage: aForm ]! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'pb 5/25/2016 01:20'!
write: aForm onFileNamed: filename

	(self subclassFor: (FileIOAccessor default extensionFor: filename))
		ifNotNil: [ :cls |
			cls putForm: aForm onFileNamed: filename ]
		ifNil: [
			self write: aForm onFileNamed: filename, '.bmp' ]! !


!JPEGReadWriter2 class methodsFor: 'image reading/writing' stamp: 'jmv 5/31/2016 11:24'!
putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	fileName asFileEntry forceWriteStreamDo: [ :stream |
		(self onBinaryStream: stream binary)
			nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag ]! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'pb 5/25/2016 01:37'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	DirectoryEntry smalltalkImageDirectory // (class name , '.bytes') writeStreamDo: [ :file |
		class selectorsDo: [ :sel | 
			file newLine; nextPutAll: sel; newLine.
			(self on: (class compiledMethodAt: sel)) printInstructionsOn: file ]].

	"
	InstructionPrinter printClass: Parser.
	"! !


!ContextPart class methodsFor: 'examples' stamp: 'jmv 5/31/2016 11:23'!
trace: aBlock onFileNamed: fileName
	"
	ContextPart trace: [3 factorial] onFileNamed: 'trace'
	"
	"This method uses the simulator to print calls to a file."

	fileName asFileEntry forceWriteStreamDo: [ :stream |
		self trace: aBlock on: stream ]! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'pb 5/25/2016 01:31'!
browseFile: aFileEntry

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	aFileEntry readStreamDo: [ :stream |
		codeFile _ (CodeFile new fullName: aFileEntry pathName; buildFrom: stream) ].
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	self open: browser label: nil! !

!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'pb 5/25/2016 01:31'!
browsePackageFile: aFileEntry

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	aFileEntry readStreamDo: [ :stream |
		codeFile _ (CodePackageFile new fullName: aFileEntry pathName; buildFrom: stream) ].
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	self open: browser label: nil! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:10'!
openFileList
	"
	FileListWindow openFileList
	"
	^ FileListWindow open: (FileList new directory: DirectoryEntry currentDirectory) label: nil! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'pb 5/25/2016 01:37'!
fileOut

	DirectoryEntry smalltalkImageDirectory // (self name, '.st') writeStreamDo: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'pb 5/25/2016 01:37'!
fileOutCategory: categoryName

	DirectoryEntry smalltalkImageDirectory // (self name,'-',categoryName,'.st') writeStreamDo: [ :f |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: f ]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'pb 5/25/2016 01:37'!
fileOutMethod: selector

	DirectoryEntry smalltalkImageDirectory // (name,'-', selector asFileName, '.st') writeStreamDo: [ :f |
		self fileOutMethods: (Array with: selector) on: f ]! !


!SpaceTally methodsFor: 'fileOut' stamp: 'pb 5/25/2016 01:34'!
compareTallyIn: beforeFileName to: afterFileName
	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"

	| answer beforeDict a afterDict allKeys before after diff |
	beforeDict _ Dictionary new.
	beforeFileName asFileEntry readStreamDo: [ :s |
		[s atEnd ] whileFalse: [
			a _ Array readFrom: s nextLine.
			beforeDict at: a first put: a allButFirst ]].

	afterDict _ Dictionary new.
	afterFileName asFileEntry readStreamDo: [ :s |
		[ s atEnd ] whileFalse: [
			a _ Array readFrom: s nextLine.
			afterDict at: a first put: a allButFirst ]].

	answer _ String streamContents: [ :stream |
		allKeys _ (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asArray sort.
		allKeys do: [ :each |
			before _ beforeDict at: each ifAbsent: [#(0 0 0)].
			after _ afterDict at: each ifAbsent: [#(0 0 0)].
			diff _ before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].
			diff = #(0 0 0) ifFalse: [
				stream nextPutAll: each, '  ', diff printString; newLine.
			].
		]].

	TextModel new contents: answer; openLabel: 'space diffs'! !

!SpaceTally methodsFor: 'fileOut' stamp: 'pb 5/25/2016 01:37'!
printSpaceAnalysis	
	"
	SpaceTally new printSpaceAnalysis
	"

	DirectoryEntry smalltalkImageDirectory // 'STspace.text' writeStreamDo: [ :stream |
		self printSpaceAnalysis: 1 on: stream ]! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 5/31/2016 11:22'!
printSpaceDifferenceFrom: fileName1 to: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		'STspace.text1' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
			--- do something that uses space here ---
		'STspace.text2' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| coll1 coll2 item |
	coll1 _ OrderedCollection new.
	DirectoryEntry smalltalkImageDirectory // fileName1 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [coll1 add: stream crLfNextLine]].
	
	coll2 _ OrderedCollection new.
	DirectoryEntry smalltalkImageDirectory // fileName2 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [
			item _ stream crLfNextLine.
			((coll1 includes: item) and: [(item endsWith: 'percent') not])
				ifTrue: [coll1 remove: item]
				ifFalse: [coll2 add: item]]].

	(TextModel new contents: (String streamContents: 
			[ :s | 
			s nextPutAll: fileName1; newLine.
			coll1 do: [:x | s nextPutAll: x; newLine].
			s newLine; newLine.
			s nextPutAll: fileName2; newLine.
			coll2 do: [:x | s nextPutAll: x; newLine]]))
		openLabel: 'Differential Space Analysis'.
! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 5/31/2016 11:24'!
saveTo: aFileName
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	DirectoryEntry smalltalkImageDirectory // aFileName forceWriteStreamDo: [ :stream |
		results do: [ :each |
				stream nextPutAll: each analyzedClassName asString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'pb 5/25/2016 01:36'!
exampleWithPictures
	"
	DataStream exampleWithPictures
	"
	| f result |
	f _ Form fromUser.
	'Test-Picture' asFileEntry writeStreamDo: [ :file |
		file binary.
		(DataStream on: file) nextPut: f ].

	'Test-Picture' asFileEntry readStreamDo: [ :file |
		file binary.
		result _ (DataStream on: file) next ].

	result display! !

!DataStream class methodsFor: 'as yet unclassified' stamp: 'pb 5/25/2016 01:36'!
testWith: anObject
	"As a test of DataStream/ReferenceStream, write out anObject and read it back.
	11/19/92 jhm: Set the file type. More informative file name.
	DataStream testWith: 'hi'
	ReferenceStream testWith: 'hi'
	"
	| result |

	(self name, ' test') asFileEntry writeStreamDo: [ :file |
		file binary.
		(self on: file) nextPut: anObject ].

	(self name, ' test') asFileEntry readStreamDo: [ :file |
		file binary.
		result _ (self on: file) next ].

	^ result! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'pb 5/25/2016 01:34'!
crc16OfFileNamed: fileName
	"
	StandardFileStream crc16OfFileNamed: 'cursor.jpeg'
	StandardFileStream crc16OfFileNamed: 'deafultPID.txt'
	"

	^[fileName asFileEntry readStreamDo: [ :stream |
		stream crc16 ]]
			on: FileDoesNotExistException do: nil! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'pb 5/25/2016 01:20'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ FillInTheBlankMorph request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'pb 5/25/2016 01:20'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry _ fullFileName asFileEntry.
	dir _ entry parent.
	localName _ entry name.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ FillInTheBlankMorph request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FileIOAccessor default privateNewFile: newName asFileEntry ].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'pb 5/25/2016 01:21'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ fullFileName asFileEntry parent.
	files _ dir fileNames.
	fileName _ fullFileName asFileEntry name.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ FillInTheBlankMorph 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !


!TheWorldMenu methodsFor: 'commands' stamp: 'pb 5/25/2016 01:38'!
saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	| fileName |
	fileName _ FillInTheBlankMorph request: 'File name for this morph?'.
	fileName isEmpty ifTrue: [^ self].  "abort"

	"Save only model, stepList, submorphs in this world"
	myWorld submorphsDo: [ :m |
		m allMorphsDo: [ :subM | subM prepareToBeSaved ]].	"Amen"

	(fileName, '.morph') asFileEntry writeStreamDo: [ :fileStream |
		fileStream fileOutObject: myWorld ]! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 5/31/2016 11:24'!
clear

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.	
		unfinishedEntry reset.
		lastDisplayPosition _ 0.
		
		logToFile ifTrue: [
			self filename asFileEntry forceWriteStreamDo: [ :stream |
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 5/31/2016 11:24'!
clearFile
	accessSemaphore critical: [
		self filename asFileEntry forceWriteStreamDo: [ :stream |
			stream nextPutAll: 'Transcript log started: '.
			DateAndTime now printOn: stream.
			stream
				newLine;
				nextPutAll: '------------------------------------------------------------------------';
				newLine ]]! !

!Transcript class methodsFor: 'private' stamp: 'pb 5/25/2016 01:30'!
addEntry: aString logToFile: otherString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."

	accessSemaphore critical: [
		
		"Internal circular collection"
		lastIndex _ lastIndex \\ self maxEntries + 1.
		firstIndex = lastIndex ifTrue: [
			firstIndex _ firstIndex \\ self maxEntries + 1 ].
		entries at: lastIndex put: aString.
		
		"external file"
		otherString ifNotNil: [
			self filename asFileEntry appendStreamDo: [ :stream |
				stream nextPutAll: otherString ]]
	]! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 5/31/2016 11:24'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| profiler |
	
	"VM statistics (Memory use and GC, mainly)"
	'MemoryStats.txt' asFileEntry forceWriteStreamDo: [ :stream |
		stream nextPutAll: self vmStatisticsReportString ].
	
	"Process list"
	'ProcessList.txt' asFileEntry forceWriteStreamDo: [ :stream |
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]].

"Fork all these, so they run in sequence, as the system is back running"
[
	
	"Process taking most CPU"
	'ThePig.txt' asFileEntry forceWriteStreamDo: [ :stream |
		ProcessBrowser dumpPigStackOn: stream ].
	
	"Tally of all processes"
	AndreasSystemProfiler canWork ifTrue: [
		'FullTally.txt' asFileEntry forceWriteStreamDo: [ :stream |
			profiler _ AndreasSystemProfiler new.
			profiler spyOn: [ (Delay forMilliseconds: 1000) wait ].
			profiler
				reportTextOn: stream
				linesOn: (DummyStream on: nil)
				talliesOn: (DummyStream on: nil) ]].

	"Memory Analysis"
	'MemoryAnalysis.txt' asFileEntry forceWriteStreamDo: [ :stream |
		SpaceTally new printSpaceAnalysis: 1 on: stream ]

] forkNamed: 'CPU usage analysis'! !

FileIOAccessor initialize!
