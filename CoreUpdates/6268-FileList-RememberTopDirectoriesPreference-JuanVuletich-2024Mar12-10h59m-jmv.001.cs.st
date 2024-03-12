'From Cuis6.3 [latest update: #6265] on 12 March 2024 at 11:07:33 am'!

!FileListWindow methodsFor: 'preferences' stamp: 'jmv 3/12/2024 11:04:44'!
initialFileListDirectories: aSymbol
	Preferences at: #initialFileListDirectories put: aSymbol.
	Preferences saveToDisk: #initialFileListDirectories.! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 3/12/2024 11:05:45'!
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
				target: self
				action: #initialFileListDirectories:
				argument: entry first ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


!FileListWindow reorganize!
('GUI building' buildMorphicWindow buttonToTriggerIn:for: morphicDirectoryTreePane morphicFileContentsPane morphicFileListPane morphicPatternPane optionalButtonRow updateButtonRow: windowColor)
('menu building' directoryTreeMenu fileListMenu fileSelectedMenu noFileSelectedMenu)
('updating' updateButtonRow)
('keyboard shortcuts' fileListKey:from:)
('preferences' initialFileListDirectories:)
!

