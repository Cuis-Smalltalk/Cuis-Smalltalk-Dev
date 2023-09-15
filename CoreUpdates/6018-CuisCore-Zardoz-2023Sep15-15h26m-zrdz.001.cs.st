'From Cuis 6.0 [latest update: #6017] on 15 September 2023 at 3:27:02 pm'!

!FileListWindow class methodsFor: 'instance creation' stamp: 'zrdz 9/15/2023 15:26:32'!
openFileList: aDirectory
	"
	FileListWindow openFileList
	"
	| win |
	
	win := FileListWindow open: aDirectory label: nil.
	win model when: #updateButtonRow send: #updateButtonRow to: win.
	^ win! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'zrdz 9/15/2023 15:26:49'!
openFileList
	"
	FileListWindow openFileList
	"
	self openFileList: (FileList new directory: DirectoryEntry currentDirectory)! !

