'From Cuis 5.0 [latest update: #4635] on 9 June 2021 at 12:46:01 pm'!

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:37:54'!
primFixedNameLookupEntryIn: fullPathAsUTF8 index: index
	"Fix the malformed String answered by the primitive. See comment at #primLookupEntryIn:index:"

	| answer |
	answer _ self primLookupEntryIn: fullPathAsUTF8 index: index.
	answer isArray ifTrue: [
		answer at: 1 put: (String fromUtf8: answer first asByteArray) ].
	^answer! !


!FileList methodsFor: 'private' stamp: 'jmv 6/9/2021 12:38:46'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first50000 |

directory // fileName readStreamDo: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read' halt].
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


!StandardFileStream methodsFor: 'open/close' stamp: 'jmv 6/9/2021 12:43:48'!
open: fileName forWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ StandardFileStream retryWithGC: [ self primOpen: (fileName asUtf8: true) writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ String new: 1.
	self enableReadBuffering
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:42:59'!
primOpen: fileNameAsUTF8 writable: writableFlag
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil
! !


!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 6/9/2021 12:19:11'!
createDirectory: fullPathName
	self primCreateDirectory: (fullPathName asUtf8: true)! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 6/9/2021 12:19:31'!
deleteDirectory: fullPathName
	self primDeleteDirectory: (fullPathName asUtf8: true)! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 6/9/2021 12:12:22'!
deleteFile: fullPathName ifAbsent: failBlock 
	^(self
			try: [self primDeleteFileNamed: (fullPathName asUtf8: true)]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 6/9/2021 12:16:29'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: (oldFileFullName asUtf8: true) to: (newFileFullName asUtf8: true) ]
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

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 6/9/2021 12:16:54'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: (oldFileFullName asUtf8: true) to: (newFileFullName asUtf8: true) ]
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

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 6/9/2021 12:36:06'!
basicDirectoryExists: fullPathName

 	^(
		(self primLookupEntryIn: (fullPathName asUtf8: true) index: 1) 
			== #badDirectoryPath
		) not! !

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 6/9/2021 12:33:47'!
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
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	lookIn _ parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName].
	[done] whileFalse: [
		entryArray _ self primFixedNameLookupEntryIn: (lookIn asUtf8: true) index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done _ true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory ifTrue: [DirectoryEntry new] ifFalse: [FileEntry new].
				entry name: (entryArray at: 1) parent: parentEntryOrNil.
				entry updateFrom: entryArray entryInParent: index.
				entries addLast: entry ].
		index _ index + 1].

	^entries asArray! !

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 6/9/2021 12:34:06'!
updateEntry: aFileSystemEntry
	| entryArray index lookIn isDirectory |

	"If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries."
	aFileSystemEntry primEntryInParent ifNotNil: [ :tentativeIndex |
		(self primFixedNameLookupEntryIn: (aFileSystemEntry parent pathName asUtf8: true) index: tentativeIndex) ifNotNil: [ :found |
	 		found == #badDirectoryPath ifFalse: [
				aFileSystemEntry name = (found at: 1) ifTrue: [
					aFileSystemEntry updateFrom: found entryInParent: tentativeIndex.
					^ self ]]]].

	"Otherwise, do a full iteration"
	lookIn _ aFileSystemEntry parent pathName.
	index _ 1.
	[
		entryArray _ self primFixedNameLookupEntryIn: (lookIn asUtf8: true) index: index.
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

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:42:23'!
primCreateDirectory: fullPathAsUTF8
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:42:19'!
primDeleteDirectory: fullPathAsUTF8
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:42:14'!
primDeleteFileNamed: aFileNameAsUTF8
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:31:59'!
primLookupEntryIn: fullPathAsUTF8 index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On MacOS and Windows,  the empty string enumerates the mounted volumes/drives.
	
	On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad.
	
	Warning: The 'name' field is an instance of String, but in reality it contains the bytes for its UTF-8 representation.
	For instance, if the real name is 'puño' we'll get  'puÃ±o', as
	(String withAll: ('puño' asUtf8 asArray collect: [ :n | Character numericValue: n ]))        =        'puÃ±o'
	Senders MUST do appropriate conversion.
	
	Consider calling #primFixedNameLookupEntryIn:index: instead.
	"

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:15:55'!
primRename: oldFileFullNameAsUTF8 to: newFileFullNameAsUTF8
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil! !

