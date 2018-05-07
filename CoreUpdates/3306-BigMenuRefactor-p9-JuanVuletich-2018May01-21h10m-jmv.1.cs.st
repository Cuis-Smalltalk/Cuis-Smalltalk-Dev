'From Cuis 5.0 of 7 November 2016 [latest update: #3304] on 1 May 2018 at 9:11:14 pm'!

!Collection methodsFor: 'converting' stamp: 'jmv 5/1/2018 14:28:51'!
asDictionary
	"Answer a Dictionary. Assume our elements are Associations.
	{
			#itemGroup 	-> 	10.
			#itemOrder 	-> 	30.
			#label 			-> 	'Browser'.
			#object 		-> 	BrowserWindow.
			#selector 		-> 	#openBrowser.
			#icon 			-> #editFindReplaceIcon.
			#balloonText 	-> 	'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary
	"

	^ self as: Dictionary! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 14:17:12'!
setIcon: symbolOrFormOrNil
	"Argument can be a Form, a Symbol (to be sent to Theme current) or nil."

	icon _ symbolOrFormOrNil isSymbol
		ifTrue: [Theme current perform: symbolOrFormOrNil]
		ifFalse: [ symbolOrFormOrNil ]! !


!Workspace class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:07:43'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		20.
			#label 			->		'Workspace'.
			#object 			-> 		Workspace.
			#selector 		-> 		#openWorkspace.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary`! !


!Transcripter class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:06:41'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		20.
			#label 			->		'Emergency Evaluator'.
			#object 			-> 		Transcripter.
			#selector 		-> 		#emergencyEvaluator.
			#icon 			-> 		#emblemImportantIcon.
			#balloonText 	-> 		'When all else fails...'.
		} asDictionary`! !


!TextEditor class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:04:48'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		10.
			#label 			->		'Text Editor'.
			#object 			-> 		TextEditor.
			#selector 		-> 		#openTextEditor.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary`! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 14:38:08'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		10.
			#label 			->		'Installed Packages'.
			#object 			-> 		CodePackageListWindow.
			#selector 		-> 		#openPackageList.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary`! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 14:36:38'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		30.
			#label 			->		'Browser'.
			#object 			-> 		BrowserWindow.
			#selector 		-> 		#openBrowser.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary`! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:01:20'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		40.
			#label 			->		'Message Names'.
			#object 			-> 		MessageNamesWindow.
			#selector 		-> 		#openMessageNames.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary`! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 14:36:47'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		20.
			#label 			->		'Change Sorter'.
			#object 			-> 		ChangeSorterWindow.
			#selector 		-> 		#openChangeSorter.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:00:10'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		10.
			#label 			->		'File List'.
			#object 			-> 		FileListWindow.
			#selector 		-> 		#openFileList.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:02:47'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		10.
			#label 			->		'Process Browser'.
			#object 			-> 		ProcessBrowserWindow.
			#selector 		-> 		#openProcessBrowser.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:03:55'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		20.
			#label 			->		'SUnit Test Runner'.
			#object 			-> 		TestRunnerWindow.
			#selector 		-> 		#openTestRunner.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'jmv 5/1/2018 21:05:41'!
worldMenuForOpenGroup
	^ `{
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		50.
			#label 			->		'Transcript'.
			#object 			-> 		TranscriptWindow.
			#selector 		-> 		#openTranscript.
			#icon 			-> 		#printerIcon.
			#balloonText 	-> 		'A window showing contents of the System Transcript'.
		} asDictionary`! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 14:18:59'!
fillIn: aMenu fromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form

	note, nil elements will add a line."
	| item |
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ aMenu addLine ]
			ifNotNil: [
				item _ (dict at: #label) isSymbol
					ifTrue: [
						aMenu
							addUpdating: (dict at: #label)
							target: (dict at: #object)
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						aMenu
							add: (dict at: #label)
							target: (dict at: #object)
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				dict
					at: #icon
					ifPresent: [ :symbolOrFormOrNil |
						item setIcon: symbolOrFormOrNil ].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						item setBalloonText: balloonText ].
			]].
	^ aMenu! !

