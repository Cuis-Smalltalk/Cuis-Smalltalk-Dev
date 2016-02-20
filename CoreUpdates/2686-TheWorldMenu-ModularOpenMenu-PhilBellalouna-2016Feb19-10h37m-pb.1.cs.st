'From Cuis 4.2 of 25 July 2013 [latest update: #2679] on 19 February 2016 at 2:34:31.508843 pm'!

!Workspace class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 12:51'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 10;
			
				at: #itemOrder
				put: 20;
			
				at: #label
				put: 'Workspace';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openWorkspace;
			
				at: #balloonText
				put: 'A window for evaluating Smalltalk expressions';
			yourself ].! !

!Workspace class methodsFor: 'instance creation' stamp: 'pb 2/19/2016 12:50'!
openWorkspace
	Workspace new
		contents: '';
		openLabel: 'Workspace'.
! !


!TextEditor class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 12:28'!
worldMenu: aMenu menuGroup: aGroupSymbol
	"
	^ (aGroupSymbol = #worldOpenMenu ifTrue: [{'Text Editor' . {self . #openTextEditor}. 'A window for composing text' }.])
	{'Text Editor' . {self . [self openTextEditor]}. 'A window for composing text' }.
	"
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			at: #itemGroup
			put: 10;
				at: #itemOrder
				put: 10;
			
				at: #label
				put: 'Text Editor';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openTextEditor;
			
				at: #balloonText
				put: 'A window for composing text';
			yourself ].! !

!TextEditor class methodsFor: 'instance creation' stamp: 'pb 2/19/2016 11:11'!
openTextEditor

	SystemWindow editText: TextModel new label: 'Text Editor' wrap: true! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 13:03'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 20;
			
				at: #itemOrder
				put: 10;
			
				at: #label
				put: 'Installed Packages';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openPackageList;
			
				at: #balloonText
				put: 'A tool for managing Packages (optional units of code) installed in the system';
			yourself ].! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 12:55'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 10;
			
				at: #itemOrder
				put: 30;
			
				at: #label
				put: 'Browser';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openBrowser;
			
				at: #balloonText
				put: 'A Smalltalk code browser, for studying and modifying the system';
			yourself ].! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 12:59'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 10;
			
				at: #itemOrder
				put: 40;
			
				at: #label
				put: 'Message Names';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openMessageNames;
			
				at: #balloonText
				put: 'A tool for finding and editing methods that contain any given keyword in their names.';
			yourself ].! !

!MessageNamesWindow class methodsFor: 'instance creation' stamp: 'pb 2/19/2016 12:58'!
openMessageNames
	"Bring a MessageNames tool to the front"
	self
		open: MessageNames new
		label: 'Message Names'.! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 13:07'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 20;
			
				at: #itemOrder
				put: 20;
			
				at: #label
				put: 'Change Sorter';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openChangeSorter;
			
				at: #balloonText
				put: 'A tool allowing you to view the methods in a Change Set, especially changes to the Base System' ;
			yourself ].! !

!ChangeSorterWindow class methodsFor: 'instance creation' stamp: 'pb 2/19/2016 13:48'!
openChangeSorter
	"Was called openChangeSorter1 in TheWorldMenu... delete that method once the changes menu is migrated"
	self
		open: ChangeSorter new
		label: nil.! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 13:13'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 40;
			
				at: #itemOrder
				put: 10;
			
				at: #label
				put: 'File List';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openFileList;
			
				at: #balloonText
				put: 'An explorer of the File System';
			yourself ].! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 13:09'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 30;
			
				at: #itemOrder
				put: 10;
			
				at: #label
				put: 'Process Browser';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openProcessBrowser;
			
				at: #balloonText
				put: 'A tool to monitor and manage Smalltalk processes';
			yourself ].! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 13:16'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 40;
			
				at: #itemOrder
				put: 20;
			
				at: #label
				put: 'SUnit Test Runner';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openTestRunner;
			
				at: #balloonText
				put: 'Smalltalk''s testing framework';
			yourself ].! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 13:01'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 10;
			
				at: #itemOrder
				put: 50;
			
				at: #label
				put: 'Transcript';
			
				at: #object
				put: self;
			
				at: #selector
				put: #openTranscript;
			
				at: #balloonText
				put:'A window showing contents of the System Transcript';
			yourself ].! !


!Transcripter class methodsFor: 'menu-world' stamp: 'pb 2/19/2016 13:11'!
worldMenu: aMenu menuGroup: aGroupSymbol
	^ aGroupSymbol = #worldOpenMenu ifTrue: [
		Dictionary new
			
				at: #itemGroup
				put: 30;
			
				at: #itemOrder
				put: 20;
			
				at: #label
				put: 'Emergency Evaluator';
			
				at: #object
				put: self;
			
				at: #selector
				put: #emergencyEvaluator;
			
				at: #balloonText
				put: 'When all else fails...';
			yourself ].! !


!TheWorldMenu methodsFor: 'construction' stamp: 'pb 2/19/2016 13:44'!
fillIn: aMenu fromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text

	note, nil elements will add a line."
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ aMenu addLine ]
			ifNotNil: [
				(dict at: #label) isSymbol
					ifTrue: [
						aMenu
							addUpdating: (dict at: #label)
							target: (dict at: #object)
							selector: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						aMenu
							add: (dict at: #label)
							target: (dict at: #object)
							selector: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						aMenu balloonTextForLastItem: balloonText ]]].
	^ aMenu.! !

!TheWorldMenu methodsFor: 'construction' stamp: 'pb 2/19/2016 14:23'!
openMenu
	"Build the open window menu for the world."
	| menu items groups firstGroup itemsSorted itemsBase |
	menu _ self menu: 'Open...'.
	"FIXME - should this functionality be removed since commands can now be added dynamically?
	ExtraOpenCommands ifNotNil: [
		self fillIn: menu from: ExtraOpenCommands ].
	"
	itemsBase _ (Smalltalk allClassesImplementing: #worldMenu:menuGroup:)
		collect: [ :item |
			item class = Metaclass ifTrue: [
				item soleInstance
					worldMenu: menu
					menuGroup: #worldOpenMenu ] ]
		thenSelect: [ :item |
			item notNil ].
	items _ OrderedCollection new.
	"A single class may add more than one item to a menu"
	itemsBase do: [ :item |
		item class = Dictionary
			ifTrue: [ items add: item ]
			ifFalse: [ items addAll: item ]].
	groups _ (items collect: [ :item |
		item at: #itemGroup ]) asSet asSortedCollection.
	itemsSorted _ OrderedCollection new.
	firstGroup _ true.
	groups do: [ :group |
		firstGroup
			ifTrue: [ firstGroup _ false ]
			ifFalse: [ itemsSorted add: nil ].
		((items select: [ :item |
			(item at: #itemGroup) = group ]) sort: [ :item1 :item2 |
			(item1 at: #itemOrder) < (item2 at: #itemOrder) ]) do: [ :item |
			itemsSorted add: item ]].
	self
		fillIn: menu
		fromDictionaries: itemsSorted.
	^ menu.! !

!methodRemoval: TheWorldMenu #fillIn:fromDictionary:!
TheWorldMenu removeSelector: #fillIn:fromDictionary:!
!methodRemoval: TheWorldMenu #openBrowser!
TheWorldMenu removeSelector: #openBrowser!
!methodRemoval: TheWorldMenu #openFileList!
TheWorldMenu removeSelector: #openFileList!
!methodRemoval: TheWorldMenu #openMessageNames!
TheWorldMenu removeSelector: #openMessageNames!
!methodRemoval: TheWorldMenu #openPackageList!
TheWorldMenu removeSelector: #openPackageList!
!methodRemoval: TheWorldMenu #openTextEditor!
TheWorldMenu removeSelector: #openTextEditor!
!methodRemoval: TheWorldMenu #openTranscript!
TheWorldMenu removeSelector: #openTranscript!
!methodRemoval: TheWorldMenu #openWorkspace!
TheWorldMenu removeSelector: #openWorkspace!

!Workspace class reorganize!
('window color' windowColor)
('menu-world' worldMenu:menuGroup:)
('instance creation' openWorkspace)
!


!TextEditor class reorganize!
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcutsSpec initializeMenu)
('class initialization' abandonChangeText basicInitialize)
('menu' paneMenu:)
('misc' openHelp)
('cached state access' changeText findText menu releaseClassCachedState)
('menu-world' worldMenu:menuGroup:)
('instance creation' openTextEditor)
!


!CodePackageListWindow class reorganize!
('instance creation' openPackageList)
('menu-world' worldMenu:menuGroup:)
!


!BrowserWindow class reorganize!
('GUI building' fullOnClass:selector: openBrowser)
('menu-world' worldMenu:menuGroup:)
!


!MessageNamesWindow class reorganize!
('menu-world' worldMenu:menuGroup:)
('instance creation' openMessageNames)
!


!ChangeSorterWindow class reorganize!
('services' browseChangeSetsWithClass:selector:)
('menu-world' worldMenu:menuGroup:)
('instance creation' openChangeSorter)
!


!FileListWindow class reorganize!
('instance creation' openFileList)
('menu-world' worldMenu:menuGroup:)
!


!ProcessBrowserWindow class reorganize!
('instance creation' openProcessBrowser)
('menu-world' worldMenu:menuGroup:)
!


!TestRunnerWindow class reorganize!
('instance creation' openTestRunner)
('menu-world' worldMenu:menuGroup:)
!


!TranscriptWindow class reorganize!
('GUI building' openTranscript)
('menu-world' worldMenu:menuGroup:)
!


!Transcripter class reorganize!
('instance creation' newInFrame: startTranscriptProcess)
('utilities' emergencyEvaluator)
('menu-world' worldMenu:menuGroup:)
!

