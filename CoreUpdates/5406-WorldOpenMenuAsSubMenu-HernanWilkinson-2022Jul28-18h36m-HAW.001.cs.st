'From Cuis 6.0 [latest update: #5401] on 28 July 2022 at 7:11:50 pm'!

!Workspace class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:44:56'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		20.
			#label 			->			'Workspace'.
			#object 			-> 		Workspace.
			#selector 		-> 		#openWorkspace.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary}`! !


!Transcripter class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:45:56'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		30.
			#itemOrder 		-> 		20.
			#label 			->			'Emergency Evaluator'.
			#object 			-> 		Transcripter.
			#selector 		-> 		#emergencyEvaluator.
			#icon 			-> 		#emblemImportantIcon.
			#balloonText 	-> 		'When all else fails...'.
		} asDictionary}`! !


!TextEditor class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:46:17'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		10.
			#label 			->			'Text Editor'.
			#object 			-> 		TextEditor.
			#selector 		-> 		#openTextEditor.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary}`! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:46:31'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		10.
			#label 			->			'Installed Packages'.
			#object 			-> 		CodePackageListWindow.
			#selector 		-> 		#openPackageList.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:46:36'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		30.
			#label 			->			'Browser'.
			#object 			-> 		BrowserWindow.
			#selector 		-> 		#openBrowser.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:46:44'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		40.
			#label 			->			'Message Names'.
			#object 			-> 		MessageNamesWindow.
			#selector 		-> 		#openMessageNames.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:46:50'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		20.
			#label 			->			'Change Sorter'.
			#object 			-> 		ChangeSorterWindow.
			#selector 		-> 		#openChangeSorter.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:46:55'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		10.
			#label 			->			'File List'.
			#object 			-> 		FileListWindow.
			#selector 		-> 		#openFileList.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary}`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:47:00'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		30.
			#itemOrder 		-> 		10.
			#label 			->			'Process Browser'.
			#object 			-> 		ProcessBrowserWindow.
			#selector 		-> 		#openProcessBrowser.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:47:04'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		20.
			#label 			->			'SUnit Test Runner'.
			#object 			-> 		TestRunnerWindow.
			#selector 		-> 		#openTestRunner.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary}`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'HAW 7/28/2022 18:47:07'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 				-> 	10.
			#itemOrder 				-> 	50.
			#label 				->	'Transcript'.
			#object 				-> 	TranscriptWindow.
			#selector 				-> 	#openTranscript.
			#icon 				-> 	#printerIcon.
			#balloonText 				-> 	'A window showing contents of the System Transcript'.
		} asDictionary}`! !


!PackageInstallerWindow class methodsFor: 'as yet unclassified' stamp: 'HAW 7/28/2022 18:47:11'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		10.
			#label 			->	'Package Installer'.
			#object 			-> 	PackageInstallerWindow.
			#selector 		-> 		#open.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for installing local Cuis packages.'.
		} asDictionary}`! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/28/2022 19:10:17'!
worldMenuOptions

	^`{{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		10.
		#label 				-> 		TheWorldMenu openLabel.
		#selector 				-> 		#yourself.
		#icon 				-> 		#openIcon.
		#balloonText				-> 		'Submenu to open various system tools'.
	} asDictionary.
	{
		#submenuOf  				-> 		TheWorldMenu openLabel.
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		10.
		#label 				->		'Package Downloader'.
		#selector 				-> 		#openPackageDownloader.
		#icon 				-> 		#packageIcon.
		#balloonText 				-> 		'A tool for downloading Cuis packages from the internet.'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		20.
		#label 				-> 		'New morph...'.
		#selector				-> 		#newMorph.
		#icon 				-> 		#morphsIcon.
		#balloonText 				-> 		'Offers a variety of ways to create new objects'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		30.
		#label 				-> 		'Preferences...'.
		#selector 				-> 		#openPreferencesMenu.
		#icon 				-> 		#preferencesIcon.
		#balloonText 				-> 		'Opens a menu offering many controls over appearance and system preferences.'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		40.
		#label 				-> 		'Windows...'.
		#selector 				-> 		#openWindowsMenu.
		#icon 				-> 		#windowIcon.
		#balloonText 				-> 		'Manage open windows'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		50.
		#label 				-> 		'Help...'.
		#selector 				-> 		#openHelpMenu.
		#icon 				-> 		#helpIcon.
		#balloonText 				-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
	} asDictionary.
	{
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		10.
		#label 				-> 		'Changes...'.
		#selector 				-> 		#openChangesMenu.
		#icon 				-> 		#changesIcon.
		#balloonText 				-> 		'Opens a menu of useful tools for dealing with changes'.
	} asDictionary.
	{
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		20.
		#label 				-> 		'Debug...'.
		#selector 				-> 		#openDebugMenu.
		#icon 				-> 		#debugIcon.
		#balloonText 				-> 		'a menu of debugging items'.
	} asDictionary.
	{
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		30.
		#label 				-> 		'Restore Display'.
		#object 				-> 		#myWorld.
		#selector 				-> 		#restoreDisplay.
		#icon 				-> 		#displayIcon.
		#balloonText 				-> 		'Repaint the screen -- useful for cleaning unwanted display artifacts.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		10.
		#label 				-> 		'Save Image'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveImage.
		#icon 				-> 		#saveIcon.
		#balloonText 				-> 		'Save the current state of the image on disk. Overwrite existing file.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		20.
		#label 				-> 		'Save Image as...'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveAs.
		#icon 				-> 		#saveAsIcon.
		#balloonText 				-> 		'Save the current state of the image on disk under a new name.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		30.
		#label 				-> 		'Save Release and Quit'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveAsNewReleaseAndQuit.
		#icon 				-> 		#saveAsNewVersionIcon.
		#balloonText 				-> 		('Save as a new release of Cuis.\',
								'Clear all user preferences and user state (class vars).\',
								'Use an updated version-stamped name\',
								'and save the image with that name on disk.\',
								'Quit Cuis.') withNewLines.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		40.
		#label 				-> 		'Save Image and Quit'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveAndQuit.
		#icon 				-> 		#saveAndQuitIcon.
		#balloonText 				-> 		'Save the image and quit out of Cuis. Overwrite existing file.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		50.
		#label 				-> 		'Quit without saving'.
		#selector 				-> 		#quitSession.
		#icon 				-> 		#quitIcon.
		#balloonText 				-> 		('Quit out of Cuis without saving the image.\',
								'Ask for confirmation if there are unsaved changes.') withNewLines.
	} asDictionary.
}`! !

!methodRemoval: TheWorldMenu class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:58'!
TheWorldMenu class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: TheWorldMenu #openLabel stamp: 'HAW 7/28/2022 18:41:30'!
TheWorldMenu removeSelector: #openLabel!
!methodRemoval: TheWorldMenu #openMenu stamp: 'HAW 7/28/2022 19:08:40'!
TheWorldMenu removeSelector: #openMenu!
!methodRemoval: TheWorldMenu #openOpenMenu stamp: 'HAW 7/28/2022 19:07:13'!
TheWorldMenu removeSelector: #openOpenMenu!
!methodRemoval: PackageInstallerWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:57'!
PackageInstallerWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: TranscriptWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:56'!
TranscriptWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: TestRunnerWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:56'!
TestRunnerWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: ProcessBrowserWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:56'!
ProcessBrowserWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: FileListWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:55'!
FileListWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: ChangeSorterWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:55'!
ChangeSorterWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: MessageNamesWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:54'!
MessageNamesWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: BrowserWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:54'!
BrowserWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: CodePackageListWindow class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:53'!
CodePackageListWindow class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: TextEditor class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:53'!
TextEditor class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: Transcripter class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:52'!
Transcripter class removeSelector: #worldMenuForOpenGroup!
!methodRemoval: Workspace class #worldMenuForOpenGroup stamp: 'HAW 7/28/2022 19:08:52'!
Workspace class removeSelector: #worldMenuForOpenGroup!

!TheWorldMenu class reorganize!
('menu building' openLabel worldMenuOptions)
('help menu' helpMenuOptions)
!

