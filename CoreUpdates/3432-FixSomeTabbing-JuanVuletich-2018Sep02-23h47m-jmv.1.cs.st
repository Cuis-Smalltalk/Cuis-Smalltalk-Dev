'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 2 September 2018 at 11:54:19 pm'!

!Workspace class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:15:27'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		20.
			#label 			->			'Workspace'.
			#object 			-> 		Workspace.
			#selector 		-> 		#openWorkspace.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary}`! !


!Transcripter class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:15:19'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		30.
			#itemOrder 		-> 		20.
			#label 			->			'Emergency Evaluator'.
			#object 			-> 		Transcripter.
			#selector 		-> 		#emergencyEvaluator.
			#icon 			-> 		#emblemImportantIcon.
			#balloonText 	-> 		'When all else fails...'.
		} asDictionary}`! !


!TextEditor class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:14:50'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		10.
			#label 			->			'Text Editor'.
			#object 			-> 		TextEditor.
			#selector 		-> 		#openTextEditor.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary}`! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:13:05'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		10.
			#label 			->			'Installed Packages'.
			#object 			-> 		CodePackageListWindow.
			#selector 		-> 		#openPackageList.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/6/2018 11:53:52'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		30.
			#label 			->			'Browser'.
			#object 			-> 		BrowserWindow.
			#selector 		-> 		#openBrowser.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:13:59'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		40.
			#label 			->			'Message Names'.
			#object 			-> 		MessageNamesWindow.
			#selector 		-> 		#openMessageNames.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:12:50'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		20.
			#label 			->			'Change Sorter'.
			#object 			-> 		ChangeSorterWindow.
			#selector 		-> 		#openChangeSorter.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:13:27'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		10.
			#label 			->			'File List'.
			#object 			-> 		FileListWindow.
			#selector 		-> 		#openFileList.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary}`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:14:22'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		30.
			#itemOrder 		-> 		10.
			#label 			->			'Process Browser'.
			#object 			-> 		ProcessBrowserWindow.
			#selector 		-> 		#openProcessBrowser.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:14:27'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		20.
			#label 			->			'SUnit Test Runner'.
			#object 			-> 		TestRunnerWindow.
			#selector 		-> 		#openTestRunner.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary}`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'HAW 7/7/2018 19:15:13'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		50.
			#label 			->			'Transcript'.
			#object 			-> 		TranscriptWindow.
			#selector 		-> 		#openTranscript.
			#icon 			-> 		#printerIcon.
			#balloonText 	-> 		'A window showing contents of the System Transcript'.
		} asDictionary}`! !

