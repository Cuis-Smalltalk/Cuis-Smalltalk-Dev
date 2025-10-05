'From Cuis7.5 [latest update: #7638] on 5 October 2025 at 6:16:58 pm'!

!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 10/5/2025 18:15:49'!
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


	self workingDirectory ifNotNil: [ :wd |
		"Don't use / as the UserBaseDirectory, as most likely we don't have rights to write to it.
		This could happen if a proper #workingDirectory is not set by the host os, for instance because
		Cuis was started from drag and drop in an os file explorer tool."
		wd isRoot ifFalse: [
			CurrentDirectory := wd ]].
	
	"Don't use / as the CurrentDirectory, as most likely we don't have rights to write to it.
	This could happen if a proper #workingDirectory is not set by the host os, for instance because
	Cuis was started from drag and drop in an os file explorer tool."
	CurrentDirectory isNil ifTrue: [
		CurrentDirectory := DirectoryEntry smalltalkImageDirectory ].
	
	^CurrentDirectory! !

