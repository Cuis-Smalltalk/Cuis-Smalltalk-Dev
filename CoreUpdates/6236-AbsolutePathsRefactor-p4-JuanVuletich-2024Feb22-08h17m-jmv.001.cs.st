'From Cuis6.3 [latest update: #6236] on 22 February 2024 at 8:28:39 am'!

!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 2/21/2024 16:59:35'!
withAbsolutePathName: aString
	"May exist or not
	aString must be an absolute path name"

	| instance |
	instance := self new.
	instance absolutePathString: aString.
	^instance! !

!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 2/21/2024 17:04:54'!
withRelativePathName: aString to: aDirectoryEntry
	"May exist or not.
	aString must be a relative path name"

	| instance |
	instance := self new.
	instance relativePathString: aString to: aDirectoryEntry.
	^instance! !


!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/21/2024 17:09:00'!
asDirectoryEntry
	"See examples in #asFileEntry method comment"

	^self isAbsolutePathName
		ifTrue: [
			DirectoryEntry withAbsolutePathName: self ]
		ifFalse: [
			DirectoryEntry withRelativePathName: self to: DirectoryEntry currentDirectory ].
! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/21/2024 17:08:55'!
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


!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 2/21/2024 17:06:49'!
name: aString parent: parentEntryOrNil



"inlinear tal cual en el sender.
juntar los 2 conocimientos de #currentDirectory."



	name := aString.
	parentEntryOrNil
		ifNil: [
			aString isAbsolutePathName
				ifTrue: [ self absolutePathString: aString ]
				ifFalse: [ self relativePathString: aString to: DirectoryEntry currentDirectory ]]
		ifNotNil: [
			parent := parentEntryOrNil.
			drive := parentEntryOrNil drive. "harmless if no drive supported, as in Unix"
			pathComponents := parentEntryOrNil pathComponents copyWith: name ].
	self invalidate! !


!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 2/22/2024 08:24:34'!
withPathName: aString
	"Note: It is generally advisable to call one or the more specific messages:
	#withAbsolutePathName: or #withRelativePathName:to:"

	^aString isAbsolutePathName
		ifTrue: [ self withAbsolutePathName: aString ]
		ifFalse: [
			self
				withRelativePathName: aString
				to: DirectoryEntry currentDirectory ].! !


!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 2/21/2024 17:02:12'!
cuisBaseDirectory
	"The main folder with the directory structure where the files provided with Cuis reside.
	Usually will not contain user files, that belong to the project using Cuis and not to Cuis itself.
	See #projectBaseDirectory.
	cuisBaseDirectory/CoreUpdates: 								Default folder to look for updates to Cuis
	cuisBaseDirectory/Packages: 								Folder for code packages included with Cuis
	cuisBaseDirectory/TrueTypeFonts: 								Default folder to look for TrueType Fonts
	DirectoryEntry cuisBaseDirectory
	"

	| imageDirectory baseDirectory |
	CuisBaseDirectory notNil ifTrue: [
		^ CuisBaseDirectory ].

	imageDirectory := self withAbsolutePathName: Smalltalk imagePath.
	baseDirectory := imageDirectory.
	[ baseDirectory notNil ] whileTrue: [
		(baseDirectory / 'CoreUpdates') exists | (baseDirectory / 'Packages') exists
			ifTrue: [ ^ CuisBaseDirectory := baseDirectory ].
		baseDirectory := baseDirectory parent ].
	"Could not find it. Just use the directory containing the image."
	^ CuisBaseDirectory := imageDirectory.! !

!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 2/21/2024 17:02:16'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory

	DirectoryEntry currentDirectory
	"
	CurrentDirectory notNil ifTrue: [
		^ CurrentDirectory ].

	Smalltalk getCurrentWorkingDirectory ifNotNil: [ :wd |
		^ CurrentDirectory := self withAbsolutePathName: wd ].

	^ CurrentDirectory := self userBaseDirectory.! !

!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 2/21/2024 17:02:19'!
smalltalkImageDirectory
	"Answer the directory on which this Smalltalk image was started (or last saved)

	DirectoryEntry smalltalkImageDirectory
	"

	ImageDirectory ifNil: [
		ImageDirectory := self withAbsolutePathName: Smalltalk imagePath ].
	^ ImageDirectory! !

!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 2/21/2024 17:02:22'!
vmDirectory
	"Answer the directory containing the VM that runs us.

	DirectoryEntry vmDirectory
	"

	VMDirectory ifNil: [
		VMDirectory := self withAbsolutePathName: Smalltalk vmPath ].
	^ VMDirectory! !

!methodRemoval: FileSystemEntry #pathString: stamp: 'jmv 2/22/2024 08:22:37'!
FileSystemEntry removeSelector: #pathString:!
