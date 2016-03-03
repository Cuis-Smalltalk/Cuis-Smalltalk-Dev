'From Cuis 4.2 of 25 July 2013 [latest update: #2696] on 29 February 2016 at 1:10:07.777087 am'!

!Workspace class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:07'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!TextEditor class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:06'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:02'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:02'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:06'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:01'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
			put: 'A tool allowing you to view the methods in a Change Set, especially changes to the Base System';
		yourself.! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:06'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:06'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:06'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:07'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
			put: 'A window showing contents of the System Transcript';
		yourself.! !


!Transcripter class methodsFor: 'menu-world' stamp: 'pb 2/29/2016 01:06'!
worldMenuForOpenGroup
	^ Dictionary new
		
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
		yourself.! !


!TheWorldMenu methodsFor: 'construction' stamp: 'pb 2/29/2016 00:59'!
openMenu
	"Build the open window menu for the world."
	| menu items groups firstGroup itemsSorted itemsBase |
	menu _ self menu: 'Open...'.
	itemsBase _ (Smalltalk allClassesImplementing: #worldMenuForOpenGroup)
		collect: [ :item |
			item class = Metaclass ifTrue: [
				item soleInstance
					worldMenuForOpenGroup ] ]
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

!methodRemoval: Transcripter class #worldMenu:menuGroup:!
Transcripter class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: TranscriptWindow class #worldMenu:menuGroup:!
TranscriptWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: TestRunnerWindow class #worldMenu:menuGroup:!
TestRunnerWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: ProcessBrowserWindow class #worldMenu:menuGroup:!
ProcessBrowserWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: FileListWindow class #worldMenu:menuGroup:!
FileListWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: ChangeSorterWindow class #worldMenu:menuGroup:!
ChangeSorterWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: MessageNamesWindow class #worldMenu:menuGroup:!
MessageNamesWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: BrowserWindow class #worldMenu:menuGroup:!
BrowserWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: CodePackageListWindow class #worldMenu:menuGroup:!
CodePackageListWindow class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: TextEditor class #worldMenu:menuGroup:!
TextEditor class removeSelector: #worldMenu:menuGroup:!
!methodRemoval: Workspace class #worldMenu:menuGroup:!
Workspace class removeSelector: #worldMenu:menuGroup:!
