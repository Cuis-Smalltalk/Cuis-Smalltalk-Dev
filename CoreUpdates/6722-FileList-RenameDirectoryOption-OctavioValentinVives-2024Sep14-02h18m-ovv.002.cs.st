'From Cuis7.1 [latest update: #6719] on 14 September 2024 at 2:41:24 am'!

!FileList methodsFor: 'volume list and pattern' stamp: 'ovv 9/14/2024 02:19:34'!
renameDirectory
	"Rename the currently selected directory"

	self request: 'New directory name?'
	initialAnswer: directory 	name
	verifying: [:newDirectoryName | newDirectoryName notEmpty and: [newDirectoryName ~= directory name]]
	do: [:newDirectoryName |
			[directory rename: newDirectoryName]
			on: Error
			do: [:anError | PopUpMenu inform: self renameErrorMsg].
			self changed: #topDirectoryWrappers
		].! !

!FileList methodsFor: 'private' stamp: 'ovv 9/14/2024 02:21:35'!
renameErrorMsg
	^'An error occurred while renaming the directory.
Perhaps a directory with that name already exists in this path?'! !


!FileListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'ovv 9/14/2024 02:41:07'!
directoryTreeMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| showHiddenTogleAndLabel |
	showHiddenTogleAndLabel := model showHidden asMenuItemTextPrefix, 'show hidden folders and files'.
	^{
		{10. 'rename directory...'. 								nil. 	#model. 			#renameDirectory. 							#saveAsIcon.
			'Rename the selected directory'}.
		{20. 'delete directory...'. 								nil. 	#model. 			#deleteDirectory. 							#warningIcon.
			'Delete the selected directory'}.
		{30. 	showHiddenTogleAndLabel. 							nil. 	#model. 			#toggleShowHidden. 							nil.
			'show or hide files and directories that begin with a dot'}.
		{40. 	'show Cuis top as tree root'. 							nil. 	#window. 			#useCuisTopAsTreeRoot. 							nil.
			'use the default Cuis top directory'}.
		{50. 'show OS roots as tree root'. 								nil. 	#window. 			#useOSrootsAsTreeRoot. 							nil.
			'use the host OS root directories. Drives on Windows; "/" on Unix'}
	}! !

