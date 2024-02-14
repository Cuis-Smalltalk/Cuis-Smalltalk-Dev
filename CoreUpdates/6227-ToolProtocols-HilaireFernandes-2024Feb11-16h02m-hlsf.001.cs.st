'From Cuis6.3 [latest update: #6225] on 11 February 2024 at 4:25:48 pm'!

!Workspace class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 11:57:14'!
open
	^self new
		contents: '';
		openLabel: 'Workspace'! !


!Browser class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:00:45'!
open
	| browser |
	browser := self new.
	^ BrowserWindow open: browser label: browser defaultBrowserTitle! !


!HierarchyBrowser class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:15:30'!
open
	^ HierarchyBrowserWindow onClass: Object selector: nil! !


!MessageSet class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:22:06'!
open
	^ MessageSetWindow openFullProtocolForClass: Object! !


!MessageNames class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:09:25'!
open
"Bring a MessageNames window to the front"
	^ MessageNamesWindow 
		open: MessageNames new
		label: 'Message Names'! !


!ChangeSorter class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:36:17'!
open
	^ ChangeSorterWindow 
		open: self new
		label: nil! !


!FileList class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:53:16'!
open
	"
	FileList open
	"
	self open: DirectoryEntry userBaseDirectory! !

!FileList class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:52:49'!
open: aDirectory
	"
	FileList open: '/' asDirectoryEntry
	"
	| win |
	win := FileListWindow open: (self new directory: aDirectory) label: nil.
	win model when: #updateButtonRow send: #updateButtonRow to: win.
	^ win! !


!CodePackageList class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:32:11'!
open
	^ CodePackageListWindow open: self new! !


!TestRunner class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:55:12'!
open
	^ TestRunnerWindow open: self new label: nil! !


!ProcessBrowser class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 12:39:01'!
open
	"
	ProcessBrowser open
	"
	"Create and schedule a ProcessBrowser."
	Smalltalk garbageCollect.
	^ ProcessBrowserWindow open: self new label: nil! !


!TextEditor class methodsFor: 'ui' stamp: 'hlsf 2/11/2024 11:55:08'!
open

	^ SystemWindow editText: TextModel new label: 'Text Editor' wrap: true! !


!Transcript class methodsFor: 'ui' stamp: 'jmv 2/11/2024 16:04:23'!
open
	"
	Transcript open
	"
	| win m |
	win := TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	m := TranscriptMorph new.
	win layoutMorph addMorph: m proportionalHeight: 1.
	win model when: #redrawTranscriptArea send: #invalidateBounds to: win.
	^ win openInWorld! !


!Workspace class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 11:57:14'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		20.
			#label 			->			'Workspace'.
			#object 			-> 		Workspace.
			#selector 		-> 		#open.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary}`! !


!CPUWatcher methodsFor: 'porcine capture' stamp: 'hlsf 2/11/2024 12:39:12'!
openMorphicWindowForSuspendedProcess: aProcess
	| menu rule |
	menu := MenuMorph new.
	"nickname  allow-stop  allow-debug"
	rule := (ProcessBrowser rulesFor: aProcess) second.
	menu add: 'Dismiss this menu' target: menu action: #delete; addLine.
	menu add: 'Open Process Browser' target: ProcessBrowser action: #open.
	menu add: 'Resume'
		target: self
		action: #resumeProcess:fromMenu:
		argumentList: { aProcess . menu }.
	menu add: 'Terminate'
		target: self
		action: #terminateProcess:fromMenu:
		argumentList: { aProcess . menu }.
	rule ifTrue: [
		menu add: 'Debug at a lower priority'
			target: self
			action: #debugProcess:fromMenu:
			argumentList: { aProcess . menu }.
	].
	menu addTitle: aProcess identityHash printString,
		' ', aProcess name,
		' is taking too much time and has been suspended.
What do you want to do with it?'.
	menu stayUp.
	menu popUpInWorld! !


!Collection methodsFor: 'converting' stamp: 'hlsf 2/11/2024 12:03:41'!
asDictionary
	"Answer a Dictionary. Assume our elements are Associations.
	{
			#itemGroup 		-> 	10.
			#itemOrder 		-> 	30.
			#label 			-> 	'Browser'.
			#object 			-> 	Browser.
			#selector 		-> 	#open.
			#icon 			-> 	#editFindReplaceIcon.
			#balloonText 	-> 	'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary
	"

	^ self as: Dictionary! !


!TextEditor class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 11:55:08'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		10.
			#label 			->			'Text Editor'.
			#object 			-> 		TextEditor.
			#selector 		-> 		#open.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary}`! !


!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 2/11/2024 16:05:43'!
recreateDefaultDesktop
	"
	self runningWorld recreateDefaultDesktop
	"
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		Theme current useTaskbar ifTrue: [self showTaskbar].
		Transcript clear.
		Transcript open
			resizeBottomLeft
	].! !

!WorldMorph methodsFor: 'events' stamp: 'hlsf 2/11/2024 12:01:35'!
keyStroke: aKeyboardEvent
	self flag: #definesKeyboardShortcuts.
	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ Browser open ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MessageSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
	^ super keyStroke: aKeyboardEvent! !

!WorldMorph methodsFor: 'windows' stamp: 'hlsf 2/11/2024 12:27:45'!
findATranscript
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self
		findAWindowSatisfying: [ :aWindow | aWindow model == Transcript]
		orMakeOneUsing: [ Transcript open ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'hlsf 2/11/2024 12:53:47'!
findAFileList: evt
	"Locate a file list, open it, and bring it to the front.
	Create one if necessary, respecting the Preference."
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == FileList ]
		orMakeOneUsing: [
			FileList open ]! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:32:25'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		10.
			#label 			->			'Installed Packages'.
			#object 			-> 		CodePackageList.
			#selector 		-> 		#open.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:02:21'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		30.
			#label 			->			'Browser'.
			#object 			-> 		Browser.
			#selector 		-> 		#open.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'hlsf 2/11/2024 12:10:17'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"
	"MessageNames open"
	| selectorListView firstRow searchButton secondRow |
	textMorph := (TextModelMorph
		textProvider: model
		textGetter: #searchString
		textSetter: #searchString:
		selectionGetter: #contentsSelection)
		setBalloonText: 'See MessageNames class comment for search string options';
		emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.
	searchButton := PluggableButtonMorph new
		model: textMorph innerTextMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.
	firstRow := LayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
		
			addMorph: searchButton
			proportionalWidth: 0.25;
		
			addMorph: textMorph
			proportionalWidth: 0.75.
	selectorListView := PluggableListMorph
		model: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:
		mainView: self
		menuGetter: #selectorListMenu
		keystrokeAction: #selectorListKey:from:.
	secondRow := LayoutMorph newRow.
	secondRow
		
			addMorph: selectorListView
			proportionalWidth: 0.5;
		
			addAdjusterAndMorph: self buildMorphicMessageList
			proportionalWidth: 0.5.
	self layoutMorph
		
			addMorph: firstRow
			fixedHeight: self defaultButtonPaneHeight + 4;
		
			addAdjusterAndMorph: secondRow
			proportionalHeight: 0.5;
		
			addAdjusterAndMorph: self buildLowerPanes
			proportionalHeight: 0.5.
	model changed: #editSelection! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:09:42'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		40.
			#label 			->			'Message Names'.
			#object 			-> 		MessageNames.
			#selector 		-> 		#open.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:36:35'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		20.
			#label 			->			'Change Sorter'.
			#object 			-> 		ChangeSorter.
			#selector 		-> 		#open.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:53:58'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		10.
			#label 			->			'File List'.
			#object 			-> 		FileList.
			#selector 		-> 		#open.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary}`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:39:23'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		30.
			#itemOrder 		-> 		10.
			#label 			->			'Process Browser'.
			#object 			-> 		ProcessBrowser.
			#selector 		-> 		#open.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:55:26'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		20.
			#label 			->			'SUnit Test Runner'.
			#object 			-> 		TestRunner.
			#selector 		-> 		#open.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary}`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'hlsf 2/11/2024 12:28:46'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 				-> 	10.
			#itemOrder 				-> 	50.
			#label 				->	'Transcript'.
			#object 				-> 	Transcript.
			#selector 				-> 	#open.
			#icon 				-> 	#printerIcon.
			#balloonText 				-> 	'A window showing contents of the System Transcript'.
		} asDictionary}`! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'hlsf 2/11/2024 12:36:44'!
changesMenuOptions

	^`{
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'Change Sorter'.
				#object 			-> 		ChangeSorter.
				#selector 		-> 		#open.
				#icon 			-> 		#halfRefreshIcon.
				#balloonText 	-> 		'Open a 3-paned changed-set viewing tool'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'Install New Updates'.
				#object 			-> 		ChangeSet.
				#selector 		-> 		#installNewUpdates.
				#icon 			-> 		#updateIcon.
				#balloonText 	-> 		'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'Browse my Changes'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#browseMyChanges.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Browse all of my changes since the last time #condenseSources was run.'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'Recently logged Changes...'.
				#object 			-> 		ChangeList.
				#selector 		-> 		#browseRecentLog.
				#icon 			-> 		#clockIcon.
				#balloonText 	-> 		'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu changesLabel.
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'Save World as morph file'.
				#selector 		-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`! !

!methodRemoval: TranscriptWindow class #openTranscript stamp: 'jmv 2/11/2024 16:06:31'!
TranscriptWindow class removeSelector: #openTranscript!
!methodRemoval: TestRunnerWindow class #openTestRunner stamp: 'jmv 2/11/2024 16:06:49'!
TestRunnerWindow class removeSelector: #openTestRunner!
!methodRemoval: ProcessBrowserWindow class #openProcessBrowser stamp: 'jmv 2/11/2024 16:08:19'!
ProcessBrowserWindow class removeSelector: #openProcessBrowser!
!methodRemoval: FileListWindow class #openFileList stamp: 'jmv 2/11/2024 16:15:37'!
FileListWindow class removeSelector: #openFileList!
!methodRemoval: FileListWindow class #openFileList: stamp: 'jmv 2/11/2024 16:15:38'!
FileListWindow class removeSelector: #openFileList:!
!methodRemoval: ChangeSorterWindow class #openChangeSorter stamp: 'jmv 2/11/2024 16:20:00'!
ChangeSorterWindow class removeSelector: #openChangeSorter!
!methodRemoval: MessageNamesWindow class #openMessageNames stamp: 'jmv 2/11/2024 16:20:26'!
MessageNamesWindow class removeSelector: #openMessageNames!
!methodRemoval: BrowserWindow class #openBrowser stamp: 'jmv 2/11/2024 16:19:09'!
BrowserWindow class removeSelector: #openBrowser!
!methodRemoval: CodePackageListWindow class #openPackageList stamp: 'jmv 2/11/2024 16:16:40'!
CodePackageListWindow class removeSelector: #openPackageList!
!methodRemoval: TextEditor class #openTextEditor stamp: 'jmv 2/11/2024 16:17:12'!
TextEditor class removeSelector: #openTextEditor!
!methodRemoval: Workspace class #openWorkspace stamp: 'jmv 2/11/2024 16:03:22'!
Workspace class removeSelector: #openWorkspace!

!CodePackageListWindow class reorganize!
('menu-world' worldMenuOptions)
!


!ChangeSorterWindow class reorganize!
('services' browseChangeSetsWithClass:selector:)
('menu-world' worldMenuOptions)
!


!FileListWindow class reorganize!
('menu-world' worldMenuOptions)
!


!TestRunnerWindow class reorganize!
('menu-world' worldMenuOptions)
!

