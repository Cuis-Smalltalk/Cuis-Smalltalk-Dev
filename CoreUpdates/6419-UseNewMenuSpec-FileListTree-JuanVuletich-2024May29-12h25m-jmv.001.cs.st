'From Cuis7.1 [latest update: #6418] on 29 May 2024 at 12:27:18 pm'!

!FileListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/29/2024 12:05:06'!
directoryTreeMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	| showHiddenTogleAndLabel |
	showHiddenTogleAndLabel := model showHidden asMenuItemTextPrefix, 'show hidden folders and files'.
	^{
		{10. 'delete directory...'. 								nil. 	#model. 			#deleteDirectory. 							nil. 	#warningIcon.
			'Delete the selected directory'}.
		{20. 	showHiddenTogleAndLabel. 							nil. 	#model. 			#toggleShowHidden. 							nil. 	nil.
			'show or hide files and directories that begin with a dot'}.
		{30. 	'show Cuis top as tree root'. 							nil. 	#window. 			#useCuisTopAsTreeRoot. 							nil. 	nil.
			'use the default Cuis top directory'}.
		{40. 'show OS roots as tree root'. 								nil. 	#window. 			#useOSrootsAsTreeRoot. 							nil. 	nil.
			'use the host OS root directories. Drives on Windows; "/" on Unix'}
	}! !

!FileListWindow methodsFor: 'menu commands' stamp: 'jmv 5/29/2024 11:58:59'!
useCuisTopAsTreeRoot
	Preferences at: #initialFileListDirectories put: #cuis.
	Preferences saveToDisk: #initialFileListDirectories.

	model changed: #topDirectoryWrappers.
	model updateDirectory.
	model updateFileList.! !

!FileListWindow methodsFor: 'menu commands' stamp: 'jmv 5/29/2024 11:59:02'!
useOSrootsAsTreeRoot
	Preferences at: #initialFileListDirectories put: #roots.
	Preferences saveToDisk: #initialFileListDirectories.

	model changed: #topDirectoryWrappers.
	model updateDirectory.
	model updateFileList.! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 5/29/2024 11:48:54'!
morphicDirectoryTreePane

	^(HierarchicalListMorph
		withModel: model
		listGetter: #topDirectoryWrappers
		indexGetter: #currentDirectorySelected
		indexSetter: #setSelectedDirectoryTo:)
			menuNshortcutsKey: #directoryTreeMenuSpec;
			autoDeselect: false;
			yourself! !

!methodRemoval: FileListWindow #directoryTreeMenu stamp: 'jmv 5/29/2024 12:26:18'!
FileListWindow removeSelector: #directoryTreeMenu!
!methodRemoval: FileListWindow #initialFileListDirectories: stamp: 'jmv 5/29/2024 12:26:21'!
FileListWindow removeSelector: #initialFileListDirectories:!

!FileListWindow reorganize!
('GUI building' buildMorphicWindow buttonToTriggerIn:for: morphicDirectoryTreePane morphicFileContentsPane morphicFileListPane morphicPatternPane optionalButtonRow updateButtonRow: windowColor)
('menu building' fileListMenu fileSelectedMenu noFileSelectedMenu)
('updating' updateButtonRow)
('keyboard shortcuts' fileListKey:from:)
('menu & kbd shortcuts' directoryTreeMenuSpec)
('menu commands' useCuisTopAsTreeRoot useOSrootsAsTreeRoot)
!

