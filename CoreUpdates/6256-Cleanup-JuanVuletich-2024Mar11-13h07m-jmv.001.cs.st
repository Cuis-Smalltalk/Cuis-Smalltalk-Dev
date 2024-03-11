'From Cuis6.3 [latest update: #6255] on 11 March 2024 at 1:09:46 pm'!

!FileListWindow methodsFor: 'menu building' stamp: 'jmv 3/11/2024 13:07:16'!
volumeMenu
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
		(roots  'default roots' 'Use the usual root directories. Drives on Windows; "/" on Unix')
	   	(image 'image directory' 'Use the directory with Smalltalk image')
		(vm 'VM directory' 'Use the virtual machine directory')
		(current 'current directory' 'Use the current directory; usually the directory the VM was started in')
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

!methodRemoval: PreferenceSet class #isInitialFileListDirectory: stamp: 'jmv 3/11/2024 13:07:31'!
PreferenceSet class removeSelector: #isInitialFileListDirectory:!
!methodRemoval: FileList #toggleInitialDirectory stamp: 'jmv 3/11/2024 13:07:22'!
FileList removeSelector: #toggleInitialDirectory!
