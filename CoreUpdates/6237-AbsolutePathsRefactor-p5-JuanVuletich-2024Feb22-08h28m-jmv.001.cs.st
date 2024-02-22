'From Cuis6.3 [latest update: #6236] on 22 February 2024 at 10:00:08 am'!

!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 2/22/2024 09:52:43'!
driveName: driveString
	"For instance, 'C:' or 'C:\'"

	name := driveString.
	drive := driveString.
	pathComponents := #().
	self invalidate! !


!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/22/2024 09:59:50'!
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
	^self isAbsolutePathName
		ifTrue: [
			FileEntry withAbsolutePathName: self ]
		ifFalse: [
			FileEntry withRelativePathName: self to: DirectoryEntry currentDirectory ].
! !


!FileIOAccessor methodsFor: 'private' stamp: 'jmv 2/22/2024 09:55:20'!
entriesIn: parentEntry
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

MacOsX - parentEntry = nil answers an empty array
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

	^entries asArray! !

!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 2/22/2024 09:19:43'!
drives
	"
	Answer a collection of Strings
	Meaningful only on Windows. Answers an empty array elsewhere.
	FileIOAccessor default drives
	"
	drives := nil. 		"will change if you mount or unmount drives!!"
	drives ifNil: [
		drives := self onWindows
			ifTrue: [ (self entriesIn: nil)]
			ifFalse: [ #() ]].
	^drives! !


!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 2/22/2024 09:57:22'!
name: aName parent: parentEntry

	name := aName.
	parent := parentEntry.
	drive := parentEntry drive. "harmless if no drive supported, as in Unix"
	pathComponents := parentEntry pathComponents copyWith: name.
	self invalidate! !

