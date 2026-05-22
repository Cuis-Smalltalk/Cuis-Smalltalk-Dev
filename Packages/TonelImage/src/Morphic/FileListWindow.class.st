"
Morphic view for FileList models. See category 'GUI building'.
"
Class {
	#name : 'FileListWindow',
	#superclass : 'SystemWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu-world' }
FileListWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		10.
			#label 			-> 		'File List'.
			#object 		-> 		FileList.
			#selector 		-> 		#open.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary}`
]

{ #category : 'GUI building' }
FileListWindow >> buildMorphicWindow [
	| middleRow upperRow |
	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: self morphicPatternPane proportionalWidth: 0.3;
		addAdjusterAndMorph: self optionalButtonRow proportionalWidth: 0.7.
	middleRow := LinearLayoutMorph newRow.
	middleRow
		addMorph: self morphicDirectoryTreePane proportionalWidth: 0.3;
		addAdjusterAndMorph: self morphicFileListPane proportionalWidth: 0.7.
	self layoutMorph
		addMorph: upperRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterAndMorph: middleRow proportionalHeight: 0.4;
		addAdjusterAndMorph: self morphicFileContentsPane proportionalHeight: 0.6.
	self setLabel: model directory pathName.
	model postOpen
]

{ #category : 'GUI building' }
FileListWindow >> buttonToTriggerIn: aFileList for: service [
	"Answer a button that will trigger the receiver service in a file list"

	| aButton |
	service argumentProvider: aFileList.
	aButton := PluggableButtonMorph 
				model: service
				action: #performService.
	aButton label: service buttonLabel.
	aButton color: self buttonColor.
	aButton setBalloonText: service description.
	^aButton
]

{ #category : 'menu & kbd shortcuts' }
FileListWindow >> directoryTreeMenuSpec [
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| showHiddenTogleAndLabel |
	showHiddenTogleAndLabel := model showHidden asMenuItemTextPrefix, 'show hidden folders and files'.
	^{
		{10. 'Rename Directory...'. 						nil. 	#model. 		#renameDirectory. 			#saveAsIcon.
			'Rename the selected directory'}.
		{20. 'Delete Directory...'. 							nil. 	#model. 		#deleteDirectory. 				#warningIcon.
			'Delete the selected directory'}.
		{30. showHiddenTogleAndLabel. 					nil. 	#model. 		#toggleShowHidden. 			nil.
			'Show or Hide files and directories that begin with a dot'}.
		{40. 'show Cuis top as tree root'. 					nil. 	#window. 		#useCuisTopAsTreeRoot. 		nil.
			'Use the default Cuis top directory'}.
		{50. 'show OS roots as tree root'. 					nil. 	#window. 		#useOSrootsAsTreeRoot. 		nil.
			'Use the host OS root directories. Drives on Windows; "/" on Unix'}
	}
]

{ #category : 'menu & kbd shortcuts' }
FileListWindow >> fileListMenuSpec [
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	^model fileName
		ifNil: [ self noFileSelectedMenuSpec ]
		ifNotNil: [ self fileSelectedMenuSpec ]
]

{ #category : 'menu & kbd shortcuts' }
FileListWindow >> fileSelectedMenuSpec [
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
				strm nextPut: {itemId. '-----'} ]]].
]

{ #category : 'GUI building' }
FileListWindow >> morphicDirectoryTreePane [

	^(HierarchicalListMorph
		withModel: model
		listGetter: #topDirectoryWrappers
		indexGetter: #currentDirectorySelected
		indexSetter: #setSelectedDirectoryTo:)
			menuNshortcutsKey: #directoryTreeMenuSpec;
			autoDeselect: false;
			yourself
]

{ #category : 'GUI building' }
FileListWindow >> morphicFileContentsPane [

	^TextModelMorph
		textProvider: model
		textGetter: #acceptedContents 
		textSetter: #put:
		selectionGetter: #contentsSelection
]

{ #category : 'GUI building' }
FileListWindow >> morphicFileListPane [

	^(PluggableListMorph
		withModel: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: )
			menuNshortcutsKey: #fileListMenuSpec.
]

{ #category : 'GUI building' }
FileListWindow >> morphicPatternPane [

	^ (TextModelMorph
		textProvider: model
		textGetter: #pattern
		textSetter: #pattern:)
		acceptOnCR: true;
		yourself.
]

{ #category : 'menu & kbd shortcuts' }
FileListWindow >> noFileSelectedMenuSpec [
	^Array streamContents: [ :strm | | itemId |
		itemId :=0.
		model itemsForNoFile do: [ :service |
			itemId := itemId + 10.
			strm nextPut: {itemId. service label. service keyboardShortcutCharacter . #model. service selector. service iconSpec}.
			service useLineAfter ifTrue: [
				itemId := itemId + 10.
				strm nextPut: {itemId. '-----'} ]]].
]

{ #category : 'GUI building' }
FileListWindow >> optionalButtonRow [
	"Answer the button row associated with a file list"

	| row buttonColor |
	row := LinearLayoutMorph newRow.
	buttonColor := self buttonColor.
	row setProperty: #buttonRow toValue: true.  "Used for dynamic retrieval later on"
	row doAdoptWidgetsColor.
	row color: buttonColor.
	self updateButtonRow: row.
	^row
]

{ #category : 'stepping' }
FileListWindow >> stepTime [
	^ 1000 "milliseconds"
]

{ #category : 'updating' }
FileListWindow >> updateButtonRow [
	| row |
	row := self 
		findDeepSubmorphThat: [:m | m hasProperty: #buttonRow]
		ifAbsent: [^self].
		
	self updateButtonRow: row
]

{ #category : 'GUI building' }
FileListWindow >> updateButtonRow: row [
	"Dynamically update the contents of the button row, if any."

	| buttons |
	row removeAllMorphs.
	buttons := OrderedCollection new.
	model universalButtonServices do: [ :service |
		buttons add: (self buttonToTriggerIn: model for: service) ].
	model dynamicButtonServices do: [ :service | 
		buttons add: (self buttonToTriggerIn: model for: service).
		service when: #fileListChanged send: #updateFileList to: model ].
	row addMorphs: buttons
]

{ #category : 'menu commands' }
FileListWindow >> useCuisTopAsTreeRoot [
	Preferences at: #initialFileListDirectories put: #cuis.
	Preferences saveToDisk: #initialFileListDirectories.

	model changed: #topDirectoryWrappers.
	model updateDirectory.
	model updateFileList.
]

{ #category : 'menu commands' }
FileListWindow >> useOSrootsAsTreeRoot [
	Preferences at: #initialFileListDirectories put: #roots.
	Preferences saveToDisk: #initialFileListDirectories.

	model changed: #topDirectoryWrappers.
	model updateDirectory.
	model updateFileList.
]

{ #category : 'GUI building' }
FileListWindow >> windowColor [
	^ Theme current fileList
]
