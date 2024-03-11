'From Cuis6.3 [latest update: #6257] on 11 March 2024 at 2:52:01 pm'!

!FileList methodsFor: 'initialization' stamp: 'jmv 3/11/2024 14:45:13'!
topDirectoryWrappers

	topDirectories := (Preferences at: #initialFileListDirectories) 
		caseOf: {
			[ #cuis ]  		-> 	[ | topDirectoryToUse |
				topDirectoryToUse := directory. "must be inside the tree of some of the included"
				(topDirectoryToUse commonAncestorWith: DirectoryEntry projectBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonAncestorWith: DirectoryEntry userBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonAncestorWith: DirectoryEntry cuisBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonAncestorWith: DirectoryEntry currentDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				{ topDirectoryToUse} ].
			[ #roots ] -> [ DirectoryEntry roots ].
			}.
	^topDirectories collect: [ :dir |
		FileDirectoryWrapper with: dir name: (dir name ifNil: [ '/' ]) model: self ]! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 3/11/2024 14:45:52'!
directoryTreeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu := MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon :: setBalloonText: 'Delete the selected directory'.
	aMenu
		add: model showHidden asMenuItemTextPrefix, 'show hidden folders and files'
		action: #toggleShowHidden ::
		setBalloonText: 'show or hide files and directories that begin with a dot'.
	initialDirectoriesMenu := MenuMorph new.
	#(
		(cuis  		'Cuis top' 'Use the default Cuis top directory')
		(roots  		'OS roots' 'Use the host OS root directories. Drives on Windows; "/" on Unix')
	)
		do: [ :entry |
			initialDirectoriesMenu
				add: entry second
				target: Preferences
				action: #at:put:
				argumentList: {#initialFileListDirectories . entry first} ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


!FileList methodsFor: 'file menu action' stamp: 'jmv 3/11/2024 14:46:59'!
addNewDirectory

	self 
		addNew: 'directory'
		byEvaluating: [ :newName | (directory / newName) assureExistence ].
	self updateDirectory.
	self changed: #topDirectoryWrappers! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 3/11/2024 14:47:05'!
deleteDirectory
	"Remove the currently selected directory"

	| newSelection |
	directory children isEmpty ifFalse: [ ^self inform:'Directory must be empty' ].
	(self confirm: 'Really delete ' , directory name , '?') ifFalse: [ ^ self ].
	newSelection := directory parent.
	directory delete.
	self directory: newSelection.
	self updateFileList.
	self updateDirectory.
	self changed: #topDirectoryWrappers! !

!FileList methodsFor: 'private' stamp: 'jmv 3/11/2024 14:51:44'!
postOpen
	| pathRoot pathElements |
	directory ifNotNil: [
		pathRoot := topDirectories detect: [ :any | any isAncestorOf: directory ].
		pathElements := pathRoot pathComponents size = 0
			ifTrue: [ {directory drive}, directory pathComponents ]
			ifFalse: [ (directory pathComponents
					copyFrom: (pathRoot pathComponents size)
					to: directory pathComponents size) ].
		self changed: #(openPath), pathElements ]! !

!FileList methodsFor: 'user interface' stamp: 'jmv 3/11/2024 14:47:09'!
toggleShowHidden
	showHidden := showHidden not.
	self changed: #topDirectoryWrappers.
	self updateFileList.! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 3/11/2024 14:45:58'!
morphicDirectoryTreePane

	^(HierarchicalListMorph
		model: model
		listGetter: #topDirectoryWrappers
		indexGetter: #currentDirectorySelected
		indexSetter: #setSelectedDirectoryTo:
		mainView: self
		menuGetter: #directoryTreeMenu
		keystrokeAction: nil)
			autoDeselect: false;
			yourself! !

!methodRemoval: FileListWindow #volumeMenu stamp: 'jmv 3/11/2024 14:46:00'!
FileListWindow removeSelector: #volumeMenu!
!methodRemoval: FileList #initialDirectoryList stamp: 'jmv 3/11/2024 14:47:16'!
FileList removeSelector: #initialDirectoryList!
