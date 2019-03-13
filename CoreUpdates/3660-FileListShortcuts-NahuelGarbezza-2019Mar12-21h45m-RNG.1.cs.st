'From Cuis 5.0 of 7 November 2016 [latest update: #3659] on 12 March 2019 at 10:00:35 pm'!
"Change Set:		3660-CuisCore-AuthorName-2019Mar12-21h45m
Date:			12 March 2019
Author:			Nahuel Garbezza

Add some shortcuts to file list"!


!FileListWindow methodsFor: 'keyboard shortcuts' stamp: 'RNG 3/12/2019 21:55:43'!
fileListKey: aChar from: aView

	aChar == $x ifTrue: [ ^ aView model deleteFile ].
	aChar == $R ifTrue: [ ^ aView model renameFile ].
	aChar == $n ifTrue: [ ^ aView model addNewFile ].
	aChar == $N ifTrue: [ ^ aView model addNewDirectory ].! !


!FileList methodsFor: 'own services' stamp: 'RNG 3/12/2019 21:57:34'!
serviceAddNewDirectory
	"Answer a service entry characterizing the 'add new directory' command"

	^ SimpleServiceEntry
		provider: self
		label: 'add new directory (N)'
		selector: #addNewDirectory
		description: 'adds a new, empty directory (folder)'
		icon: #listAddIcon! !

!FileList methodsFor: 'own services' stamp: 'RNG 3/12/2019 21:56:19'!
serviceAddNewFile
	"Answer a service entry characterizing the 'add new file' command"

	^ SimpleServiceEntry 
		provider: self 
		label: 'add new file (n)' 
		selector: #addNewFile 
		description: 'create a new,. empty file, and add it to the current directory.'
		icon: #newIcon! !

!FileList methodsFor: 'own services' stamp: 'RNG 3/12/2019 21:58:57'!
serviceDeleteFile

	^ SimpleServiceEntry
		provider: self
		label: 'delete (x)'
		selector: #deleteFile
		description: 'delete the seleted item'
		icon: #deleteIcon! !

!FileList methodsFor: 'own services' stamp: 'RNG 3/12/2019 21:59:33'!
serviceRenameFile

	^ SimpleServiceEntry
		provider: self
		label: 'rename (R)'
		selector: #renameFile
		description: 'rename file'
		icon: #saveAsIcon! !


!FileListWindow methodsFor: 'GUI building' stamp: 'RNG 3/12/2019 21:47:48'!
morphicFileListPane

	^PluggableListMorph
		model: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: 
		mainView: self
		menuGetter: #fileListMenu
		keystrokeAction: #fileListKey:from:! !


!FileListWindow reorganize!
('GUI building' buildMorphicWindow buttonToTriggerIn:for: morphicDirectoryTreePane morphicFileContentsPane morphicFileListPane morphicPatternPane optionalButtonRow updateButtonRow: windowColor)
('menu building' fileListMenu fileSelectedMenu noFileSelectedMenu volumeMenu)
('updating' updateButtonRow)
('keyboard shortcuts' fileListKey:from:)
!

