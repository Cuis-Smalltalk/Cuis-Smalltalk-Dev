'From Cuis 5.0 of 7 November 2016 [latest update: #3348] on 5 July 2018 at 6:42:58 pm'!

!DynamicMenuBuilder methodsFor: 'testing' stamp: 'HAW 7/5/2018 18:26:14'!
hasTitle

	^title ~= self class noTitle ! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 18:41:03'!
classesProvidingMenuOptions

	^(Smalltalk allClassesImplementing: menuOptionsSelector) select: [ :aClass | aClass isMeta ]! !


!DynamicMenuBuilder class methodsFor: 'instance creation' stamp: 'HAW 7/5/2018 18:25:03'!
targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^self titled: self noTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

! !

!DynamicMenuBuilder class methodsFor: 'defaults' stamp: 'HAW 7/5/2018 18:25:09'!
noTitle

	^nil! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/5/2018 18:27:20'!
buildTargeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^(self targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector) build! !

!DynamicMenuBuilder class methodsFor: 'building' stamp: 'HAW 7/5/2018 18:27:45'!
buildTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector

	^(self titled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector) build! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'HAW 7/5/2018 18:37:46'!
worldMenuOptions

	^`{{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		10.
		#label 			-> 		'Open...'.
		#selector 		-> 		#openOpenMenu.
		#icon 			-> 		#openIcon.
		#balloonText 	-> 		'Submenu to open various system tools'.
	} asDictionary.
	{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		20.
		#label 			-> 		'New morph...'.
		#selector 		-> 		#newMorph.
		#icon 			-> 		#morphsIcon.
		#balloonText 	-> 		'Offers a variety of ways to create new objects'.
	} asDictionary.
	{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		30.
		#label 			-> 		'Preferences...'.
		#selector 		-> 		#openPreferencesMenu.
		#icon 			-> 		#preferencesIcon.
		#balloonText 	-> 		'Opens a menu offering many controls over appearance and system preferences.'.
	} asDictionary.
				{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		40.
		#label 			-> 		'Windows...'.
		#selector 		-> 		#openWindowsMenu.
		#icon 			-> 		#windowIcon.
		#balloonText 	-> 		'Manage open windows'.
	} asDictionary.
	{
		#itemGroup 	-> 		10.
		#itemOrder 		-> 		50.
		#label 			-> 		'Help...'.
		#selector 		-> 		#openHelpMenu.
		#icon 			-> 		#helpIcon.
		#balloonText 	-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
	} asDictionary.
	{
		#itemGroup 	-> 		20.
		#itemOrder 		-> 		10.
		#label 			-> 		'Changes...'.
		#selector 		-> 		#openChangesMenu.
		#icon 			-> 		#changesIcon.
		#balloonText 	-> 		'Opens a menu of useful tools for dealing with changes'.
	} asDictionary.
	{
		#itemGroup 	-> 		20.
		#itemOrder 		-> 		20.
		#label 			-> 		'Debug...'.
		#selector 		-> 		#openDebugMenu.
		#icon 			-> 		#debugIcon.
		#balloonText 	-> 		'a menu of debugging items'.
	} asDictionary.
	{
		#itemGroup 	-> 		20.
		#itemOrder 		-> 		30.
		#label 			-> 		'Restore Display (r)'.
		#object 			-> 		#myWorld.
		#selector 		-> 		#restoreDisplay.
		#icon 			-> 		#displayIcon.
		#balloonText 	-> 		'Repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		10.
		#label 			-> 		'Save'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveSession.
		#icon 			-> 		#saveIcon.
		#balloonText 	-> 		'Save the current version of the image on disk'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		20.
		#label 			-> 		'Save as...'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveAs.
		#icon 			-> 		#saveAsIcon.
		#balloonText 	-> 		'Save the current version of the image on disk under a new name.'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		30.
		#label 			-> 		'Save as New Version'.
		#object 			-> 		Smalltalk.
		#selector 		-> 		#saveAsNewVersion.
		#icon 			-> 		#saveAsNewVersionIcon.
		#balloonText 	-> 		('Save the current image a new version-stamped name\and save it under that name on disk.\',
									'Clear all user preferences and user state (class vars).') withNewLines.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		40.
		#label 			-> 		'Save and Quit'.
		#selector 		-> 		#saveAndQuit.
		#icon 			-> 		#saveAndQuitIcon.
		#balloonText 	-> 		'Save the image and quit out of Cuis.'.
	} asDictionary.
	{
		#itemGroup 	-> 		30.
		#itemOrder 		-> 		50.
		#label 			-> 		'Quit'.
		#selector 		-> 		#quitSession.
		#icon 			-> 		#quitIcon.
		#balloonText 	-> 		'Quit out of Cuis.'.
	} asDictionary.
}`! !


!Workspace class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:03:00'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		20.
			#label 			->		'Workspace'.
			#object 			-> 		Workspace.
			#selector 		-> 		#openWorkspace.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary}`! !


!Transcripter class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:02:53'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		20.
			#label 			->		'Emergency Evaluator'.
			#object 			-> 		Transcripter.
			#selector 		-> 		#emergencyEvaluator.
			#icon 			-> 		#emblemImportantIcon.
			#balloonText 	-> 		'When all else fails...'.
		} asDictionary}`! !


!TextEditor class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:02:41'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		10.
			#label 			->		'Text Editor'.
			#object 			-> 		TextEditor.
			#selector 		-> 		#openTextEditor.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary}`! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:01:57'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		10.
			#label 			->		'Installed Packages'.
			#object 			-> 		CodePackageListWindow.
			#selector 		-> 		#openPackageList.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:01:26'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		30.
			#label 			->		'Browser'.
			#object 			-> 		BrowserWindow.
			#selector 		-> 		#openBrowser.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:02:18'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		40.
			#label 			->		'Message Names'.
			#object 			-> 		MessageNamesWindow.
			#selector 		-> 		#openMessageNames.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:01:49'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		20.
			#label 			->		'Change Sorter'.
			#object 			-> 		ChangeSorterWindow.
			#selector 		-> 		#openChangeSorter.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:02:10'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		10.
			#label 			->		'File List'.
			#object 			-> 		FileListWindow.
			#selector 		-> 		#openFileList.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary}`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:02:24'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		10.
			#label 			->		'Process Browser'.
			#object 			-> 		ProcessBrowserWindow.
			#selector 		-> 		#openProcessBrowser.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:02:30'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		20.
			#label 			->		'SUnit Test Runner'.
			#object 			-> 		TestRunnerWindow.
			#selector 		-> 		#openTestRunner.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary}`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'HAW 7/5/2018 18:02:47'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		50.
			#label 			->		'Transcript'.
			#object 			-> 		TranscriptWindow.
			#selector 		-> 		#openTranscript.
			#icon 			-> 		#printerIcon.
			#balloonText 	-> 		'A window showing contents of the System Transcript'.
		} asDictionary}`! !


!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 18:41:32'!
collectMenuOptions

	items := OrderedCollection new.
	self classesProvidingMenuOptions do: [ :aClass | items addAll: (aClass soleInstance perform: menuOptionsSelector) ].
	! !

!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'HAW 7/5/2018 18:25:48'!
createMenu

	menu := self hasTitle ifTrue: [ MenuMorph entitled: title] ifFalse: [ MenuMorph new ].
	menu
		defaultTarget: defaultTarget;
		addStayUpIcons! !

!TheWorldMenu methodsFor: 'construction' stamp: 'HAW 7/5/2018 18:28:15'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	^DynamicMenuBuilder buildTargeting: self collectingMenuOptionsWith: #worldMenuOptions
	! !

!TheWorldMenu methodsFor: 'construction' stamp: 'HAW 7/5/2018 18:28:15'!
openMenu
	"Build the open window menu for the world."

	^DynamicMenuBuilder buildTitled: 'Open...' targeting: self collectingMenuOptionsWith: #worldMenuForOpenGroup
	! !


!DynamicMenuBuilder reorganize!
('initialization' initializeTitled:targeting:collectingMenuOptionsWith:)
('building' build)
('testing' hasTitle)
('building - private' addGroupSeparation addGroupedMenuOptionsToMenu addMenuOptionsOfGroup: classesProvidingMenuOptions collectMenuOptions createMenu)
!


!DynamicMenuBuilder class reorganize!
('instance creation' targeting:collectingMenuOptionsWith: titled:targeting:collectingMenuOptionsWith:)
('defaults' noTitle)
('building' buildTargeting:collectingMenuOptionsWith: buildTitled:targeting:collectingMenuOptionsWith:)
!


!TheWorldMenu class reorganize!
('menu building' worldMenuOptions)
!
