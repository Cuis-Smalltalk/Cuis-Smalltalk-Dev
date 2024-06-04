'From Cuis7.1 [latest update: #6441] on 4 June 2024 at 2:43:46 pm'!

!FileListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 12:48:38'!
fileListMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	^model fileName
		ifNil: [ self noFileSelectedMenuSpec ]
		ifNotNil: [ self fileSelectedMenuSpec ]! !

!FileListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 13:27:25'!
fileSelectedMenuSpec
	| itemsPart1 itemsPart2 itemsPart3 itemsPart4 services |
	itemsPart1 := model itemsForAnyFile1.
	itemsPart2 := model itemsForFileEntry: model selectedFileEntry.
	itemsPart3 := model itemsForAnyFile2.
	itemsPart4 := model itemsForNoFile.
	itemsPart1 notEmpty ifTrue: [ itemsPart1 last useLineAfter: true ].
	itemsPart2 notEmpty ifTrue: [ itemsPart2 last useLineAfter: true ].
	itemsPart3 notEmpty ifTrue: [ itemsPart3 last useLineAfter: true ].
	services := itemsPart1, itemsPart2, itemsPart3, itemsPart4.
	services do: [ :svc |
		svc argumentProvider: model.
		svc when: #fileListChanged send: #updateFileList to: model ].
	^Array streamContents: [ :strm | | itemId |
		itemId :=0.
		services do: [ :service |
			itemId := itemId + 10.
			"strm nextPut: {itemId. service label. service keyboardShortcutCharacter . #model. service selector. service iconSpec}."
			strm nextPut: {itemId. service label. service keyboardShortcutCharacter . service. #performService. service iconSpec}.
			service useLineAfter ifTrue: [
				itemId := itemId + 10.
				strm nextPut: {itemId. '-----'} ]]].! !

!FileListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 12:52:40'!
noFileSelectedMenuSpec
	^Array streamContents: [ :strm | | itemId |
		itemId :=0.
		model itemsForNoFile do: [ :service |
			itemId := itemId + 10.
			strm nextPut: {itemId. service label. service keyboardShortcutCharacter . #model. service selector. service iconSpec}.
			service useLineAfter ifTrue: [
				itemId := itemId + 10.
				strm nextPut: {itemId. '-----'} ]]].! !


!FileList methodsFor: 'own services' stamp: 'jmv 6/4/2024 12:38:21'!
serviceAddNewDirectory
	"Answer a service entry characterizing the 'add new directory' command"

	^ (SimpleServiceEntry
		provider: self
		label: 'add new directory (N)'
		selector: #addNewDirectory
		description: 'adds a new, empty directory (folder)'
		icon: #listAddIcon)
			keyboardShortcutCharacter: $N! !

!FileList methodsFor: 'own services' stamp: 'jmv 6/4/2024 12:38:15'!
serviceAddNewFile
	"Answer a service entry characterizing the 'add new file' command"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'add new file (n)' 
		selector: #addNewFile 
		description: 'create a new,. empty file, and add it to the current directory.'
		icon: #newIcon)
			keyboardShortcutCharacter: $n! !

!FileList methodsFor: 'own services' stamp: 'jmv 6/4/2024 12:38:12'!
serviceDeleteFile

	^ (SimpleServiceEntry
		provider: self
		label: 'delete (x)'
		selector: #deleteFile
		description: 'delete the seleted item'
		icon: #deleteIcon)
			keyboardShortcutCharacter: $x! !

!FileList methodsFor: 'own services' stamp: 'jmv 6/4/2024 12:38:07'!
serviceRenameFile

	^ (SimpleServiceEntry
		provider: self
		label: 'rename (R)'
		selector: #renameFile
		description: 'rename file'
		icon: #saveAsIcon)
			keyboardShortcutCharacter: $R! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 6/4/2024 13:33:43'!
morphicFileListPane

	^(PluggableListMorph
		withModel: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: )
			menuNshortcutsKey: #fileListMenuSpec.! !

!methodRemoval: FileListWindow #fileListKey:from: stamp: 'jmv 6/4/2024 14:42:13'!
FileListWindow removeSelector: #fileListKey:from:!
!methodRemoval: FileListWindow #noFileSelectedMenu stamp: 'jmv 6/4/2024 14:42:13'!
FileListWindow removeSelector: #noFileSelectedMenu!
!methodRemoval: FileListWindow #fileListMenu stamp: 'jmv 6/4/2024 14:42:13'!
FileListWindow removeSelector: #fileListMenu!
!methodRemoval: FileListWindow #fileSelectedMenu stamp: 'jmv 6/4/2024 14:42:13'!
FileListWindow removeSelector: #fileSelectedMenu!

!FileListWindow reorganize!
('GUI building' buildMorphicWindow buttonToTriggerIn:for: morphicDirectoryTreePane morphicFileContentsPane morphicFileListPane morphicPatternPane optionalButtonRow updateButtonRow: windowColor)
('updating' updateButtonRow)
('menu & kbd shortcuts' directoryTreeMenuSpec fileListMenuSpec fileSelectedMenuSpec noFileSelectedMenuSpec)
('menu commands' useCuisTopAsTreeRoot useOSrootsAsTreeRoot)
!

