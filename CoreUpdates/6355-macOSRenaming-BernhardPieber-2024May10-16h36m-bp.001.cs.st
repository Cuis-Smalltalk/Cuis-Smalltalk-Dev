'From Cuis6.3 [latest update: #6352] on 10 May 2024 at 4:37:41 pm'!

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'bp 5/10/2024 12:48:30'!
asDriveName
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)
	
Windows
	'C:\' asDriveName 'C:'
	'NotAChance' asDriveName nil
	
Linux
	'/media/cdrom' asDriveName nil

macOS
    '/SanDisk32-NTFS' asDriveName nil
	
	"
	FileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].
	
	^ nil! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'bp 5/10/2024 12:48:36'!
asFileEntry
	"Note: It is generally advisable to call one of the more specific messages:
		#//
		#withAbsolutePathName: 
		#withRelativePathName:to:"
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
	macOS
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
	^ FileEntry withPathName: self! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'bp 5/10/2024 12:48:42'!
asFullFileEntry
	"Only for fully qualified path names"
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

macOS
    '/var' asFileEntry exists false
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists  true
    '/Volumes/SanDisk32-NTFS' asFileEntry exists false
    '/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
    'SanDisk32-NTFS' asFileEntry exists false
    'SanDisk32-NTFS' asDirectoryEntry exists false
	
	"
	self isAbsolutePathName ifFalse: [ self error: 'Only for fully qualified (absolute) path names' ].
	^ FileEntry withAbsolutePathName: self.! !


!FileIOAccessor methodsFor: 'private' stamp: 'bp 5/10/2024 12:48:48'!
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

	^entries asArray! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'bp 5/10/2024 12:56:50'!
primLookupEntryIn: fullPathAsUtf8Bytes index: index
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

! !


!DirectoryEntry class methodsFor: 'base directories' stamp: 'bp 5/10/2024 12:28:13'!
currentDirectory
	"The current directory.
	
	It is used, for unqualified names: `'stuff.txt' asFileEntry`, `'subfldr' asDirectoryEntry`. It is also used for relative paths if the full path is not specified: `'../sibling/data.txt'`.

	In Linux and macOS, it is the current directory in the OS shell that started us.

	Same in Windows if Cuis is started from a command line. But, for instance, if the image is dropped on the VM in a Windows explorer, and no OS current directory is available, it defaults to #userBaseDirectory.

	DirectoryEntry currentDirectory
	See #cuisBaseDirectory. See #userBaseDirectory.
	See #getCurrentWorkingDirectory"

	CurrentDirectory notNil ifTrue: [
		^ CurrentDirectory ].

	Smalltalk getCurrentWorkingDirectory ifNotNil: [ :wd |
		^ CurrentDirectory := self withAbsolutePathName: wd ].

	^ CurrentDirectory := self userBaseDirectory.! !

!DirectoryEntry class methodsFor: 'system directories' stamp: 'bp 5/10/2024 11:56:36'!
roots
	"Appropriate for all systems, including those with several roots, each being a logical 'drive' (Like Windows),
	and for systems having a single root with file systems (i.e. 'drives') mounted anywhere in the tree (Unix, MacOS)
	"
	"
	DirectoryEntry roots
	"
	^ FileIOAccessor default drives
		ifEmpty: [
			"On Linux and macOS"
			{ '/' asFullDirectoryEntry } ]! !


!Utilities class methodsFor: 'default desktop' stamp: 'bp 5/10/2024 11:58:38'!
defaultTextEditorContents
	^ ('
', 
('Cuis Smalltalk

' centered blue pointSize: FontFamily defaultPointSize * 3),
('
"Yay, Juan.  You GO, guy!! ...a great example of malleable software (and a clever mind) at work."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Dan Ingalls
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I like it... It''s nice and clean and simple and pretty. Nice stuff!!"
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Alan Kay
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I think you have a very elegant design aesthetic."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('John Maloney
'italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
'

',
'Cuis is a modern, Open Source, multiplatform, Smalltalk-80 system.


',
'Cuis is:

' bold,
'    - Small
    - Clean
    - Appropriable
' blue,
'

Additionally, Cuis is:

' bold, 
'    - Open Source
    - Multiplatform
' blue,
'
 
Like other Smalltalk systems, Cuis is also:

' bold, 
'    - A complete development environment written in itself
    - A pure, dynamic Object Oriented language
' blue,
'

Cuis assumes very little on the underlying platform, and this lets it run out-of-the-box on Windows, macOS, Linux, ChromeOS and WebBrowsers. Cuis shares the [OpenSmalltalk Virtual Machine] (http://www.opensmalltalk.org) with Squeak, Pharo and Newspeak.

What sets Cuis apart from the other members of the Squeak family is the focus on Smalltalk-80 and an active attitude towards system complexity:

Unbound complexity growth, together with development strategies focused only in the short term, are the worst long term enemies of all software systems. As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity and size grow without control. Evolution slows down. Understanding the system becomes harder every day. Bugs are harder to fix. Codebases become huge for no clear reason. At some point, the system can''t evolve anymore and becomes "legacy code".

Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"] (http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!!

Cuis Smalltalk is our attempt at this challenge. Furthermore, we believe we are doing something else that no other Smalltalk, commercial or open source, does. We attempt to give the true Smalltalk-80 experience, and keep Smalltalk-80 not as legacy software of historic significance, but as a live, evolving system. We feel we are the keepers of the Smalltalk-80 heritage, and enablers of the Dynabook experience.

As Cuis evolves, we keep on these values. Every update, be it a bug fix or a feature enhancement, is reviewed carefully to avoid adding unneeded complexity to the system. Every opportunity to remove unneeded complexity is followed. As we go, features are enhanced, and any reported bugs fixed. We also adopt selected enhancements from Squeak and Pharo, and share our work with the wider Smalltalk community.


' justified,
'License
' bold,
'
Cuis is distributed subject to the MIT License, as in http://www.opensource.org/licenses/mit-license.php . Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.

Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982
Copyright (c) Apple Computer, Inc. 1985-1996
Copyright (c) Contributors to Squeak project. 1997-2024
Copyright (c) Contributors to Cuis Smalltalk project. 2009-2024')! !

