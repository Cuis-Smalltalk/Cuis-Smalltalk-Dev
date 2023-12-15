'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:30:00 pm'!
!classDefinition: #DirectoryEntry category: #'System-File System'!
FileSystemEntry subclass: #DirectoryEntry
	instanceVariableNames: 'children '
	classVariableNames: 'CurrentDirectory ImageDirectory VMDirectory CuisBaseDirectory ProjectBaseDirectory '
	poolDictionaries: ''
	category: 'System-File System'!

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 12/15/2023 14:09:44'!
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

	imageDirectory := self withPathName: Smalltalk imagePath.
	baseDirectory := imageDirectory.
	[ baseDirectory notNil ] whileTrue: [
		(baseDirectory / 'CoreUpdates') exists | (baseDirectory / 'Packages') exists
			ifTrue: [ ^ CuisBaseDirectory := baseDirectory ].
		baseDirectory := baseDirectory parent ].
	"Could not find it. Just use the directory containing the image."
	^ CuisBaseDirectory := imageDirectory.! !

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 12/15/2023 14:35:06'!
projectBaseDirectory
	"The main directory that contains the #cuisBaseDirectory, but also other directories with user and project files.
	See #cuisBaseDirectory.
	projectBaseDirectory/SavedChangeSets 									Default folder for saving new ChangeSets
	projectBaseDirectory/Packages: 									Default folder for new code packages
	projectBaseDirectory/UserChanges: 									Folder to store user changes files
	projectBaseDirectory/FileOuts: 									Default folder for filed out code
	projectBaseDirectory/Logs: 									Default folder for error logs and other logs
	
	Right now, the same as #currentDirectory. This could perhaps change.
	"
	ProjectBaseDirectory notNil ifTrue: [
		^ ProjectBaseDirectory ].

	Smalltalk getCurrentWorkingDirectory ifNotNil: [ :wd |
		^ ProjectBaseDirectory := self withPathName: wd ].

	^ ProjectBaseDirectory := self cuisBaseDirectory parent.! !


!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 12/15/2023 14:33:54'!
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
		^ CurrentDirectory := self withPathName: wd ].

	^ CurrentDirectory := self cuisBaseDirectory parent.! !

!DirectoryEntry class methodsFor: 'cached state access' stamp: 'jmv 12/15/2023 14:11:14'!
releaseClassCachedState

	ImageDirectory := nil.
	VMDirectory := nil.
	CurrentDirectory := nil.
	CuisBaseDirectory := nil.
	ProjectBaseDirectory := nil.! !

!classDefinition: #DirectoryEntry category: #'System-File System'!
FileSystemEntry subclass: #DirectoryEntry
	instanceVariableNames: 'children'
	classVariableNames: 'CuisBaseDirectory CurrentDirectory ImageDirectory ProjectBaseDirectory VMDirectory'
	poolDictionaries: ''
	category: 'System-File System'!
