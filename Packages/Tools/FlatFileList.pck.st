'From Cuis 6.0 [latest update: #5069] on 9 May 2022 at 5:59:48 pm'!
'Description Flat file browser tool. Alternative to Cuis default FileList tool.'!
!provides: 'FlatFileList' 1 42!
SystemOrganization addCategory: 'FlatFileList'!


!classDefinition: #FlatFileListWindow category: 'FlatFileList'!
SystemWindow subclass: #FlatFileListWindow
	instanceVariableNames: 'directoryList fileList directoryIndex fileIndex directoryInput'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FlatFileList'!
!classDefinition: 'FlatFileListWindow class' category: 'FlatFileList'!
FlatFileListWindow class
	instanceVariableNames: ''!

!classDefinition: #FlatFileContentsBrowserWindow category: 'FlatFileList'!
FlatFileListWindow subclass: #FlatFileContentsBrowserWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FlatFileList'!
!classDefinition: 'FlatFileContentsBrowserWindow class' category: 'FlatFileList'!
FlatFileContentsBrowserWindow class
	instanceVariableNames: ''!

!classDefinition: #FlatFileSelectionWindow category: 'FlatFileList'!
FlatFileListWindow subclass: #FlatFileSelectionWindow
	instanceVariableNames: 'selectionAction filenameFilter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FlatFileList'!
!classDefinition: 'FlatFileSelectionWindow class' category: 'FlatFileList'!
FlatFileSelectionWindow class
	instanceVariableNames: ''!

!classDefinition: #FlatFileList category: 'FlatFileList'!
Object subclass: #FlatFileList
	instanceVariableNames: 'currentDirectory selectedFile sortBy sortAscending'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FlatFileList'!
!classDefinition: 'FlatFileList class' category: 'FlatFileList'!
FlatFileList class
	instanceVariableNames: ''!


!FlatFileListWindow commentStamp: '<historical>' prior: 0!
A flat file browser.!

!FlatFileContentsBrowserWindow commentStamp: '<historical>' prior: 0!
A flat file list browser that displays file contents in a panel.!

!FlatFileSelectionWindow commentStamp: '<historical>' prior: 0!
A file selection morph.

selectionAction block is evaluated with the selected FileEntry object.
When set, filenameFilter is a block for filtering the type of files allowed. For example: [:fileEntry | fileEntry extension = 'txt']

FlatFileSelectionWindow withFileDo: [:fileEntry | fileEntry inspect]!

!FlatFileList commentStamp: '<historical>' prior: 0!
Model of FlatFileListWindow.!

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 23:12:52'!
addSortMenuItems: aMenu

	aMenu add: 'sort by name' action: #sortByName.
	aMenu add: 'sort by size' action: #sortBySize.
	aMenu add: 'sort by modification time' action: #sortByModificationTime.
	aMenu add: 'sort by extension' action: #sortByExtension.! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 23:08:52'!
browseFileChanges
	ChangeList browseRecentLogOn: model selectedFile! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 17:31:36'!
buildMorphicWindow

	| upperPanel directoryPanel toolbar gotoParentButton refreshButton statusBar |
	
	upperPanel _ LayoutMorph newRow.
	
	directoryPanel _ LayoutMorph newColumn.
	
	toolbar _ LayoutMorph newRow.
	
	gotoParentButton _ PluggableButtonMorph model: self 
						action: #gotoParentDirectory.
	gotoParentButton icon: Theme current goUpIcon.
	
	toolbar addMorph: gotoParentButton.
	
	refreshButton _ PluggableButtonMorph model: self action: #refreshDirectory.
	refreshButton icon: Theme current changesIcon .
	toolbar addMorph: refreshButton.
	
	directoryInput _ TextModelMorph textProvider: self 
					textGetter: #currentDirectoryName 
					textSetter: #currentDirectoryName:.
	directoryInput acceptOnCR: true.
	
	toolbar addMorph: directoryInput fixedHeight: 30.
	
	layoutMorph addMorph: toolbar fixedHeight: 30.
	
	directoryList _ PluggableListMorph model: self 
					listGetter: #directoryListNames
					indexGetter: #directoryIndex 
					indexSetter: #directoryIndex:.
	directoryList 	doubleClickSelector: #selectDirectory.
	
	directoryPanel addMorphUseAll: directoryList.
		
	upperPanel addMorph: directoryPanel proportionalWidth: 0.4.
	
	fileList _ PluggableListMorph model: self 
				listGetter: #fileListNames
				indexGetter: #fileIndex 
				indexSetter: #fileIndex:
				mainView: self
				menuGetter: #fileListMenu
				keystrokeAction: #fileListKey:from:.
	fileList 	doubleClickSelector: #doubleClickFile.
	
	upperPanel addAdjusterAndMorph: fileList layoutSpec: LayoutSpec useAll.
	
	layoutMorph addMorph: upperPanel.
	
	statusBar _ TextModelMorph textProvider: self textGetter: #statusMessage.
	
	layoutMorph addMorph: statusBar fixedHeight: 30.
	! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 18:37:14'!
currentDirectoryName

	^ model currentDirectory asString! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 19:59:23'!
currentDirectoryName: aDirectoryName

	model currentDirectory: aDirectoryName asString asDirectoryEntry.
	directoryInput hasUnacceptedEdits: false.
	self directoryChanged.! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:00:30'!
directoryChanged
	self changed: #directoryListNames.
	self changed: #fileListNames.
	self changed: #currentDirectoryName.
	fileIndex _ nil.
	self changed: #fileIndex.
	! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/8/2022 10:48:23'!
directoryList
	^ model currentDirectoryList! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/8/2022 10:52:02'!
directoryListNames
	^ model currentDirectoryList collect: [:dir | dir name]! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:17:52'!
doubleClickFile

	| selectedFile |
	selectedFile _ self fileList at: self fileIndex .
	model selectedFile: selectedFile.
	self viewSelectedFile! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:30:54'!
fileInFile
	ChangeSet fileIn: model selectedFile! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 20:21:58'!
fileListMenu
	^model selectedFile
		ifNil: [ self noFileSelectedMenu ]
		ifNotNil: [ self fileSelectedMenu ]! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 23:08:12'!
fileSelectedMenu

	| menu |
	
	menu _ MenuMorph entitled: model selectedFile name.
	menu defaultTarget: self.
	
	menu add: 'view' action: #viewSelectedFile.
	menu add: 'workspace with contents' action: #viewContentsInWorkspace.
	
	({'pck'. 'pck.st'} includes: model selectedFile extension)
		ifTrue: [menu add: 'install package' action: #installPackageFile].
		
	model selectedFile extension = 'st'
		ifTrue: [menu add: 'file in' action: #fileInFile].
		
	model selectedFile extension = 'cs.st'
		ifTrue: [menu add: 'install changeset' action: #installChangeSet].
		
	model selectedFile extension = 'changes'
		ifTrue: [menu add: 'browse changes in file' action: #browseFileChanges]. 
		
	menu addLine .
	self addSortMenuItems: menu.
	
	^ menu! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 17:35:20'!
fileSelectionChanged
	self changed: #statusMessage! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 09:52:10'!
gotoParentDirectory
	model gotoParentDirectory! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:31:22'!
installChangeSet
	ChangeSet install: model selectedFile! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 20:58:05'!
installPackageFile

	CodePackageFile installPackage: model selectedFile! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:20:02'!
noFileSelectedMenu

	| menu |
	
	menu _ MenuMorph entitled: model selectedFile name.
	menu defaultTarget: self.
	
	self addSortMenuItems: menu.
	
	^ menu! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 20:35:19'!
openSelectedFile
	
	model selectedFile fileContents editLabel: model selectedFile asString! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 19:57:04'!
refreshDirectory
	self directoryChanged.! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 09:50:06'!
selectDirectory

	| selectedDirectory |
	selectedDirectory _ self selectedDirectory.
	model currentDirectory: selectedDirectory! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/8/2022 10:49:48'!
selectedDirectory
	^ model currentDirectoryList at: self directoryIndex! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 23:13:03'!
sortByExtension

	model sortBy: #extension.
	self directoryChanged.! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:18:49'!
sortByModificationTime

	model sortBy: #modificationTime.
	self directoryChanged.! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:18:06'!
sortByName

	model sortBy: #name.
	self directoryChanged.! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:18:40'!
sortBySize

	model sortBy: #size.
	self directoryChanged.! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 17:58:56'!
statusMessage
	^ model selectedFile ifNotNil: [:file |
		file name , ' ° ', file fileSize printStringAsBytes , ' ° last modified: ', file modificationTime asString]
	ifNil: [model currentDirectory name, ' ° ', model currentDirectory children size asString, ' elements']! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 09:51:14'!
update: aSymbol
		
	aSymbol == #currentDirectory ifTrue: [	self directoryChanged].
	aSymbol == #selectedFile ifTrue: [self fileSelectionChanged].! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 20:31:11'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"
	
	| aString aName |


	model selectedFile readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		aString _ stream next: stream size.
		aName _ stream localName ].

	(Workspace new contents: aString) openLabel: 'Workspace from ', aName! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 21:24:46'!
viewSelectedFile
	
	model selectedFile fileContents editLabel: model selectedFile asString! !

!FlatFileListWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 18:30:53'!
windowColor
	^ Theme current fileList! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/7/2022 18:14:25'!
directoryIndex
	"Answer the value of directoryIndex"

	^ directoryIndex ifNil: [0]! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/7/2022 18:14:08'!
directoryIndex: anObject
	"Set the value of directoryIndex"

	directoryIndex _ anObject! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/7/2022 18:14:08'!
directoryList: anObject
	"Set the value of directoryList"

	directoryList _ anObject! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/7/2022 18:14:33'!
fileIndex
	"Answer the value of fileIndex"

	^ fileIndex ifNil: [0]! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/9/2022 17:58:08'!
fileIndex: anObject
	"Set the value of fileIndex"

	fileIndex _ anObject.
	
	fileIndex isZero ifFalse: [
		model selectedFile: (model currentDirectoryFiles at: fileIndex)]
		ifTrue: [model selectedFile: nil]! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/7/2022 21:33:22'!
fileList
	"Answer the value of fileList"

	^ model currentDirectoryFiles! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/7/2022 18:14:08'!
fileList: anObject
	"Set the value of fileList"

	fileList _ anObject! !

!FlatFileListWindow methodsFor: 'accessing' stamp: 'MM 5/8/2022 10:53:06'!
fileListNames
	"Answer the value of fileList"

	^ model currentDirectoryFiles collect: [:file | file name]! !

!FlatFileListWindow class methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:21:58'!
open

	^ self open: (FlatFileList new currentDirectory: DirectoryEntry currentDirectory) label: 'File browser'.! !

!FlatFileListWindow class methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 21:21:37'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		10.
			#label 			->			'Flat File List'.
			#object 			-> 		FlatFileListWindow.
			#selector 		-> 		#open.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'A flat explorer of the File System'.
		} asDictionary}`! !

!FlatFileContentsBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 09:45:51'!
buildMorphicWindow
	
	| fileContents |
	
	super buildMorphicWindow .
	
	fileContents _ TextModelMorph textProvider: self textGetter: #fileContents textSetter: #fileContents:.
	layoutMorph addAdjusterAndMorph: fileContents layoutSpec: LayoutSpec useAll. ! !

!FlatFileContentsBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 09:55:00'!
fileContents
	^ model selectedFile ifNotNil: [:file | file fileContents] ifNil: ['']! !

!FlatFileContentsBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 17:48:23'!
fileSelectionChanged
	super fileSelectionChanged.
	self changed: #fileContents.! !

!FlatFileSelectionWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:15:29'!
buildMorphicWindow
	
	| buttonsBar selectButton cancelButton |
	
	super buildMorphicWindow .
	
	buttonsBar _ LayoutMorph newRow.
	
	selectButton _ PluggableButtonMorph model: self action: #selectFile  label: 'select'.
	buttonsBar addMorph: selectButton.
	
	cancelButton _ PluggableButtonMorph model: self action: #cancel label: 'cancel'.
	buttonsBar addMorph: cancelButton.
	
	layoutMorph addMorph: buttonsBar fixedHeight: 30. ! !

!FlatFileSelectionWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:19:08'!
cancel

	self delete! !

!FlatFileSelectionWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:18:38'!
doubleClickFile
	self selectFile! !

!FlatFileSelectionWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:22:24'!
selectFile
	selectionAction value: model selectedFile.
	self delete! !

!FlatFileSelectionWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:21:04'!
selectionAction: aBlock
	selectionAction _ aBlock! !

!FlatFileSelectionWindow class methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 10:21:12'!
withFileDo: aBlock
	self open selectionAction: aBlock! !

!FlatFileList methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 09:55:34'!
currentDirectory: aDirectory

	currentDirectory _ aDirectory.
	self changed: #currentDirectory.
	self selectedFile: nil! !

!FlatFileList methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 23:12:28'!
currentDirectoryFiles

	|files|
	
	files _ self currentDirectory files.
	
	^ sortBy caseOf: {
		[#name] -> [|sorter| sorter _ sortAscending 
						ifTrue: [[:f1 :f2 | f1 name < f2 name]]
						ifFalse: [[:f1 :f2 | f1 name > f2 name]].
					files asSortedCollection: sorter].
		[#modificationTime] -> [|sorter| sorter _ sortAscending 
						ifTrue: [[:f1 :f2 | f1 modificationTime < f2 modificationTime]]
						ifFalse: [[:f1 :f2 | f1 modificationTime > f2 modificationTime]].
					files asSortedCollection: sorter].
		[#size] -> [|sorter| sorter _ sortAscending 
						ifTrue: [[:f1 :f2 | f1 fileSize < f2 fileSize]]
						ifFalse: [[:f1 :f2 | f1 fileSize > f2 fileSize]].
					files asSortedCollection: sorter].
		[#extension] -> [|sorter| sorter _ sortAscending 
						ifTrue: [[:f1 :f2 | f1 extension < f2 extension]]
						ifFalse: [[:f1 :f2 | f1 extension > f2 extension]].
					files asSortedCollection: sorter].
	}! !

!FlatFileList methodsFor: 'as yet unclassified' stamp: 'MM 5/8/2022 10:48:37'!
currentDirectoryList
	^ currentDirectory directories asSortedCollection: [:d1 :d2 | d1 name < d2 name]! !

!FlatFileList methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 21:11:20'!
defaultInitialDirectory

	^ Preferences initialFileListDirectories
		caseOf: {
			[ #roots ] -> [ 	DirectoryEntry roots first].
			[ #image ] -> [ DirectoryEntry smalltalkImageDirectory ].
			[ #vm  ] -> [ DirectoryEntry vmDirectory ].
			[ #current ] -> [DirectoryEntry currentDirectory]}
		otherwise: [DirectoryEntry currentDirectory]! !

!FlatFileList methodsFor: 'as yet unclassified' stamp: 'MM 5/9/2022 09:52:34'!
gotoParentDirectory
	currentDirectory parent ifNotNil: [:parent |
		self currentDirectory: parent]! !

!FlatFileList methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:15:06'!
initialize

	currentDirectory _ self defaultInitialDirectory.
	sortBy _ #name.
	sortAscending _ true.! !

!FlatFileList methodsFor: 'as yet unclassified' stamp: 'MM 5/7/2022 22:10:42'!
toggleSortDirection

	sortAscending _ sortAscending not! !

!FlatFileList methodsFor: 'accessing' stamp: 'MM 5/7/2022 18:12:53'!
currentDirectory
	"Answer the value of currentDirectory"

	^ currentDirectory! !

!FlatFileList methodsFor: 'accessing' stamp: 'MM 5/7/2022 18:12:53'!
selectedFile
	"Answer the value of selectedFile"

	^ selectedFile! !

!FlatFileList methodsFor: 'accessing' stamp: 'MM 5/9/2022 09:48:40'!
selectedFile: anObject
	"Set the value of selectedFile"

	selectedFile _ anObject.
	self changed: #selectedFile! !

!FlatFileList methodsFor: 'accessing' stamp: 'MM 5/7/2022 21:34:38'!
sortBy
	"Answer the value of sortBy"

	^ sortBy! !

!FlatFileList methodsFor: 'accessing' stamp: 'MM 5/7/2022 22:12:24'!
sortBy: anObject
	"Set the value of sortBy"

	sortBy == anObject ifTrue: [self toggleSortDirection ].
	sortBy _ anObject
	! !