'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:42:11 pm'!

!ContentPack class methodsFor: 'configuration' stamp: 'jmv 12/15/2023 15:26:18'!
exportDirectory
	
	^ DirectoryEntry projectBaseDirectory / self defaultContentDirectory / 'Exported'! !

!ContentPack class methodsFor: 'configuration' stamp: 'jmv 12/15/2023 15:26:29'!
importDirectory
	
	^ DirectoryEntry projectBaseDirectory / self defaultContentDirectory / 'Import'! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'jmv 12/15/2023 14:18:08'!
openFileList
	"
	FileListWindow openFileList
	"
	self openFileList: (FileList new directory: DirectoryEntry projectBaseDirectory)! !

