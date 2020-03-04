'From Cuis 5.0 [latest update: #4047] on 26 February 2020 at 2:26:37 pm'!

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 2/26/2020 12:39:06'!
fileReaderServicesForFileEntry: filename suffix: suffix
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(#('st' 'cs' 'cs.st') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowsePackage } ].

	^#()! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'pb 2/26/2020 12:39:06'!
fileReaderServicesForFileEntry: filename suffix: suffix
	"FileList buttons that open a ChangeListWindow on contents"

	(#('st' 'cs' 'cs.st') includes: suffix)
		ifTrue: [ ^ {self serviceContents}  ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].

	suffix = 'changes'
		ifTrue: [ ^ {self serviceRecentChanges} ].

	^#()! !


!FileList methodsFor: 'file list menu' stamp: 'pb 2/26/2020 12:43:21'!
itemsForFileEntry: aFileEntry
	"Answer a list of services appropriate for a file of the given name"
	^ self class itemsForFileEntry: aFileEntry! !


!FileList class methodsFor: 'file reader registration' stamp: 'pb 2/26/2020 12:43:21'!
itemsForFileEntry: aFileEntry
	"Answer a list of services appropriate for a file of the given name"
	| services suffix classList |
	"
	suffix _ (FileIOAccessor default extensionFor: filename) asLowercase.
	"
	suffix := aFileEntry extension asLowercase.
	"Build the list dynamically for all implementers of the appropriate class method... registration no longer required"
	classList _ (Smalltalk allClassesImplementing: #fileReaderServicesForFileEntry:suffix:)
		collect: [ :item |
			item class == Metaclass ifTrue: [ item soleInstance ]]
		thenSelect: [ :item |
			item notNil ].
	services _ OrderedCollection new.
	classList do: [ :reader |
		services addAll:
			(reader
				fileReaderServicesForFileEntry: aFileEntry 
				suffix: suffix) ].
	services sort: [ :a :b | a sortOrder < b sortOrder ].
	^ services.! !


!FileEntry methodsFor: 'accessing' stamp: 'pb 2/26/2020 13:37:21'!
baseDirectory
	"The directory this file is located in"
	^ DirectoryEntry
		withPathComponents: self pathComponents allButLast
		drive: nil.! !


!ChangeSet class methodsFor: 'file list services' stamp: 'pb 2/26/2020 12:39:06'!
fileReaderServicesForFileEntry: filename suffix: suffix
	"These would better be done by ChangeSorter!!"

	(#('cs' 'cs.st') includes: suffix) ifTrue: [
		^{self serviceInstall} ].

	(#('st') includes: suffix) ifTrue: [
		^{self serviceFileIn} ].

	^#()! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'pb 2/26/2020 12:39:06'!
fileReaderServicesForFileEntry: filename suffix: suffix
	"FileList buttons that are serviced by us."

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ { self serviceInstallPackage } ].
	^#()! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 2/26/2020 14:25:31'!
serviceBrowseCode
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse code'
		selector: #browseCodeFileEntry:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code'
		icon: #editFindReplaceIcon)
			sortOrder: 10! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 2/26/2020 14:25:39'!
serviceBrowsePackage
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse package'
		selector: #browsePackageFileEntry:
		description: 'open a "package file browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'package browser'
		icon: #editFindReplaceIcon)
			sortOrder: 10! !


!ChangeList class methodsFor: 'public access' stamp: 'pb 2/26/2020 12:46:51'!
browseRecentLogOn: origChangesFileEntry
	"figure out where the last snapshot or quit was, then browse the recent entries."

	| end done block positions prevBlock |
	origChangesFileEntry readStreamDo: [ :changesFile |
		positions _ SortedCollection new.
		end _ changesFile size.
		prevBlock _ end.
		block _ end - 1024 max: 0.
		done _ false.
		[ done or: [ positions size > 0 ]]
			whileFalse: [
				changesFile position: block.
				"ignore first fragment"
				changesFile nextChunk.
				[ changesFile position < prevBlock ]
					whileTrue: [
						| pos chunk |
						pos _ changesFile position.
						chunk _ changesFile nextChunk.
						((Smalltalk isQuitRecord: chunk) or: [ Smalltalk isSnapshotRecord: chunk ])
							ifTrue: [ positions add: pos ]].
				block = 0
					ifTrue: [done _ true]
					ifFalse: [
						prevBlock _ block.
						block _ block - 1024 max: 0]].
	].
	self browseRecentLogOn: origChangesFileEntry startingFrom: (positions isEmpty ifTrue: [0] ifFalse: [positions last])! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'pb 2/26/2020 14:25:02'!
serviceContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'see code file contents'
		selector: #browseContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'contents'
		icon: #changesIcon)
			sortOrder: 20! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'pb 2/26/2020 14:25:10'!
servicePackageContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'see package file contents'
		selector: #browsePackageContents:
		description: 'open a code file contents tool on this package file'
		buttonLabel: 'pck contents'
		icon: #changesIcon)
			sortOrder: 20! !


!FileList methodsFor: 'initialization' stamp: 'pb 2/26/2020 12:43:21'!
dynamicButtonServices
	"Answer services for buttons that may come and go in the button pane, depending on selection"

	^ fileName isEmptyOrNil
		ifTrue:
			[#()]
		ifFalse:
			[ | toReject |
				toReject _ self buttonSelectorsToSuppress.
				(self itemsForFileEntry: self selectedFileEntry) reject:
					[:svc | toReject includes: svc selector]]! !


!SimpleServiceEntry methodsFor: 'performing service' stamp: 'pb 2/26/2020 14:24:39'!
getArgumentsFrom: aProvider

	argumentGetter ifNil: [^aProvider selectedFileEntry ].
	^argumentGetter value: aProvider! !


!FileListWindow methodsFor: 'menu building' stamp: 'pb 2/26/2020 12:43:21'!
fileSelectedMenu

	| itemsPart1 itemsPart2 itemsPart3 itemsPart4 n1 n2 n3 services aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	itemsPart1 _ model itemsForAnyFile1.
	itemsPart2 _ model itemsForFileEntry: model selectedFileEntry.
	itemsPart3 _ model itemsForAnyFile2.
	itemsPart4 _ model itemsForNoFile.
	n1 _ itemsPart1 size.
	n2 _ n1 + itemsPart2 size.
	n3 _ n2 + itemsPart3 size.
	services _ itemsPart1, itemsPart2, itemsPart3, itemsPart4.
	services do: [ :svc | svc when: #fileListChanged send: #updateFileList to: model ].
	^ aMenu 
		addServices: services 
		for: model
		extraLines:{ n1 . n2 . n3 }
! !


!DropFilesAction methodsFor: 'evaluating - private' stamp: 'pb 2/26/2020 12:43:21'!
fileNamedDropped: aFileName 

	| options selectionIndex menu |
	
	selectedFileEntry := aFileName asFileEntry.
	options := FileList itemsForFileEntry: selectedFileEntry.
	options isEmpty ifTrue: [ ^self inform: 'No action found for ', selectedFileEntry name ].
	menu := self createMenuFor: options.
		
	selectionIndex := menu startUpWithCaption: 'Select action for ', selectedFileEntry name.
	
	selectionIndex = 0 ifTrue: [ ^self ].
	(options isInBounds: selectionIndex) ifTrue: [ ^self performService: (options at: selectionIndex) ].
	"The only available option is 'stop here'. This could change if #createMenuFor: changes - Hernan"
	stopHereBlock value
	
! !


!ChangeSet class methodsFor: 'file list services' stamp: 'pb 2/26/2020 14:25:16'!
serviceFileIn
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'file in'
		selector: #fileIn:
		description: 'file in the entire contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'filein'
		icon: #saveIcon)
			sortOrder: 100! !

!ChangeSet class methodsFor: 'file list services' stamp: 'pb 2/26/2020 14:25:23'!
serviceInstall
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install change set'
		buttonLabel: 'install'
		icon: #saveIcon)
			sortOrder: 100! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'pb 2/26/2020 14:25:45'!
serviceInstallPackage
	"Answer the service of installing a CodePackageFile (a chunk format file for a CodePackage)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackage:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package'
		icon: #saveIcon)
			sortOrder: 100! !

!methodRemoval: CodePackageFile class #fileReaderServicesForFile:suffix: stamp: 'pb 2/26/2020 12:39:07'!
CodePackageFile class removeSelector: #fileReaderServicesForFile:suffix:!
!methodRemoval: ChangeSet class #fileReaderServicesForFile:suffix: stamp: 'pb 2/26/2020 12:39:07'!
ChangeSet class removeSelector: #fileReaderServicesForFile:suffix:!
!methodRemoval: FileList class #itemsForFile: stamp: 'pb 2/26/2020 12:43:21'!
FileList class removeSelector: #itemsForFile:!
!methodRemoval: FileList #itemsForFile: stamp: 'pb 2/26/2020 12:43:21'!
FileList removeSelector: #itemsForFile:!
!methodRemoval: ChangeList class #fileReaderServicesForFile:suffix: stamp: 'pb 2/26/2020 12:39:07'!
ChangeList class removeSelector: #fileReaderServicesForFile:suffix:!
!methodRemoval: CodeFileBrowser class #fileReaderServicesForFile:suffix: stamp: 'pb 2/26/2020 12:39:07'!
CodeFileBrowser class removeSelector: #fileReaderServicesForFile:suffix:!

!FileList reorganize!
('file list' fileList fileListIndex fileListIndex: fileName selectedFileEntry)
('user interface support' autoCompleterClassFor: editorClassFor:)
('file list menu' itemsForAnyFile1 itemsForAnyFile2 itemsForFileEntry: itemsForNoFile)
('file menu action' addNew:byEvaluating: addNewDirectory addNewFile deleteFile get getHex renameFile sortByDate sortByName sortBySize)
('initialization' buttonSelectorsToSuppress directory: dynamicButtonServices initialDirectoryList initialize labelString universalButtonServices updateDirectory)
('menu messages' copyName)
('own services' serviceAddNewDirectory serviceAddNewFile serviceCopyName serviceDeleteFile serviceGet serviceGetHex serviceRenameFile serviceSortByDate serviceSortByName serviceSortBySize serviceViewContentsInWorkspace viewContentsInWorkspace)
('volume list and pattern' deleteDirectory directory doesPattern:allow: fileNameFormattedFrom:namePad:sizePad:sizeWithCommasPad: listForSelectingPatterns:rejectingPatterns: pattern pattern:)
('private' acceptedStringOrText currentDirectorySelected defaultContents fileNameFromFormattedItem: folderString fullName isFileSmalltalkCode postOpen put: readContentsBrief: readContentsHex: resort: setSelectedDirectoryTo: sortBlock sortingByDate sortingByName sortingBySize updateFileList)
('accessing' brevityState)
!


!FileList class reorganize!
('file reader registration' itemsForFileEntry: registerFileReader: unregisterFileReader:)
!


!FileListWindow reorganize!
('GUI building' buildMorphicWindow buttonToTriggerIn:for: morphicDirectoryTreePane morphicFileContentsPane morphicFileListPane morphicPatternPane optionalButtonRow updateButtonRow: windowColor)
('menu building' fileListMenu fileSelectedMenu noFileSelectedMenu volumeMenu)
('updating' updateButtonRow)
('keyboard shortcuts' fileListKey:from:)
!


!DropFilesAction reorganize!
('initialization' initializeFor:)
('evaluating' value)
('evaluating - private' createMenuFor: fileNamedDropped: performService:)
('FileList protocol' fullName selectedFileEntry)
!

