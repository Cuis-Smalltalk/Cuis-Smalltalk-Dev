'From Cuis 3.3 of 2 June 2011 [latest update: #1024] on 28 March 2012 at 4:55:02 pm'!
!classDefinition: #CodePackage category: #'Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges description sourceSystem '
	classVariableNames: 'InstalledPackages '
	poolDictionaries: ''
	category: 'Package Support'!
!classDefinition: #CodePackageFile category: #'Package Support'!
CodeFile subclass: #CodePackageFile
	instanceVariableNames: 'packageName classesToRemove methodsToRemove description '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!
!classDefinition: #CodePackageList category: #'Package Support'!
ActiveModel subclass: #CodePackageList
	instanceVariableNames: 'selection packages '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!CodeFile methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:58'!
                      sourceSystem
	^sourceSystem! !


!CodePackage methodsFor: 'testing' stamp: 'jmv 3/28/2012 15:23'!
                              includesAnyCode
	^self classes notEmpty or: [ self methods notEmpty ]! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/28/2012 16:10'!
                   description

	^description! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/28/2012 16:18'!
                              description: aString

	description _ aString! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:59'!
            sourceSystem
	^sourceSystem ifNil: ['']! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/28/2012 16:00'!
                 sourceSystem: aString

	sourceSystem _ aString! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 3/28/2012 16:36'!
    initialize
	super initialize.
	description _ ''! !

!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 3/28/2012 16:34'!
                           doIt: chgRec
	| string |
	string := chgRec string.
	('''Description *'
		match: string) ifTrue:[^self possibleDescription: chgRec].
	super doIt: chgRec! !

!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 3/28/2012 16:35'!
                   possibleDescription: chgRec
	| tokens |
	description isEmpty ifTrue:[
		tokens := Smalltalk actualScannerClass new scanTokens: chgRec string.
		(tokens size = 1 and:[tokens first class == String]) ifTrue:[
			description := tokens first.
			^self]].
	doIts add: chgRec.! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 16:18'!
      description: aText

	selection ifNotNil: [
		selection description: aText string ].
	^true! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 16:09'!
                          editorClass
	^SmalltalkEditor! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 16:44'!
                       packageFullNames

	^ packages collect: [ :each | each fullFileName ]! !

!CodePackageList methodsFor: 'initialize-release' stamp: 'jmv 3/28/2012 15:07'!
       initialize
	self updatePackageList.
	CodePackage
		when: #installedPackagesChanged
		send: #updatePackageList
		to: self! !

!CodePackageList methodsFor: 'events' stamp: 'jmv 3/28/2012 16:44'!
                               updateDirtyFlags
	self
		changed: #packageDirtyFlags;
		changed: #packageFullNames	"if it was never saved and now it is, fileName changes"! !

!CodePackageList methodsFor: 'events' stamp: 'jmv 3/28/2012 16:44'!
             updatePackageList
	
	| newPackages oldPackages |
	oldPackages _ (packages ifNil: [#()]) asIdentitySet.
	newPackages _ CodePackage installedPackages asIdentitySet.
	oldPackages do: [ :old |
		(newPackages includes: old) ifFalse: [
			old removeActionsWithReceiver: self ]].
	newPackages do: [ :new |
		(oldPackages includes: new) ifFalse: [
			new when: #dirtyFlagChanged send:#updateDirtyFlags to: self ]].
	newPackages _ newPackages asArray sort: [ :a :b |
		 a packageName < b packageName ].
	packages _ newPackages.
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary! !

!CodePackageList methodsFor: 'commands' stamp: 'jmv 3/28/2012 16:40'!
     save

	selection ifNotNil: [
		selection save ]! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/28/2012 10:04'!
                           initialExtent
	^720@480! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/28/2012 09:32'!
                    windowColor
	^ Theme current packageList! !


!CodePackageListWindow class methodsFor: 'instance creation' stamp: 'jmv 3/28/2012 09:15'!
                       openPackageList
	CodePackageListWindow open: CodePackageList new! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 3/28/2012 09:14'!
                       openPackageList
	CodePackageListWindow openPackageList! !


!Theme methodsFor: 'tool colors' stamp: 'jmv 3/28/2012 09:41'!
     packageList
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.63 g: 0.47 b: 0.08 ]! !


!DarkTheme methodsFor: 'tool colors' stamp: 'jmv 3/28/2012 09:40'!
packageList
	^ Color r: 0.2 g: 0.16 b: 0.04! !


!CodeFile methodsFor: 'reading' stamp: 'jmv 3/28/2012 15:58'!
                 buildFrom: aStream
	| chgRec changes |
	changes _ (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	('Processing ', self name) 
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :bar |
			1 to: changes size do:[:i|
				bar value: i.
				chgRec := changes at: i.
				chgRec class == MethodDeletionChangeRecord
					ifTrue: [ self removedMethod: chgRec command with: chgRec ]
					ifFalse: [ self perform: (chgRec type copyWith: $:) asSymbol with: chgRec ].
			].
		]! !


!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/28/2012 15:27'!
                 externalBehaviors
	^self externalClasses" , self externalTraits"! !

!CodePackage methodsFor: 'naming' stamp: 'jmv 3/28/2012 15:45'!
                           packageName: aString
	packageName _ aString.
	self hasUnsavedChanges: self includesAnyCode.
	description _ 'Please enter a description for this package '! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/28/2012 16:46'!
  save
	| nameToUse |
	fullFileName ifNil: [
		fullFileName _
			(ChangeSet defaultChangeSetDirectory pathName, FileDirectory slash, 
			self packageName, FileDirectory dot, 'pck')
				asFileName ].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck') asFileName ]."
	Cursor write
		showWhile: [
			| file |
			file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]].
	self hasUnsavedChanges: false! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/28/2012 16:28'!
                      writeOnStream: aStream
	
"	self writeSystemCategoriesOn: aStream."
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self
		writeClassDefinitionsOn: aStream;
		writeClassCommentsOn: aStream;
		writeMethodsOn: aStream;
		writeInitializersOn: aStream! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:24'!
  fullFileName

	^fullFileName ifNil: '---Never saved yet'! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:14'!
hasUnsavedChanges: aBoolean

	hasUnsavedChanges _ aBoolean.
	self triggerEvent: #dirtyFlagChanged! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 3/28/2012 15:16'!
          register: aCodePackage
	"
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	"
	InstalledPackages at: aCodePackage packageName put: aCodePackage.
	self triggerEvent: #installedPackagesChanged! !

!CodePackage class methodsFor: 'class initialization' stamp: 'jmv 3/28/2012 15:04'!
                             initialize
	"
	CodePackage initialize
	"
	InstalledPackages _ Dictionary new.
	self triggerEvent: #installedPackagesChanged! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 3/28/2012 16:37'!
                         install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage _ CodePackage newNamed: packageName.
	newCodePackage
		fullFileName: fullName;
		sourceSystem: sourceSystem;
		description: description.
	CodePackage register: newCodePackage.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared print.

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:44'!
                           description

	selection ifNil: [ ^'' ].
	^selection description! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:08'!
                     packageDirtyFlags

	^ packages collect: [ :each |
		each hasUnsavedChanges
			ifTrue: [ '     --->']
			ifFalse: [ '       -' ]]! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:08'!
                    packageNames

	^ packages collect: [ :each | each packageName ]! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:08'!
                     selectionIndex

	^ packages indexOf: selection! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 16:44'!
      selectionIndex: anInteger

	selection _ anInteger = 0 ifFalse: [ packages at: anInteger ].
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/28/2012 15:59'!
                summary

	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem;
			nextPutAll: ' -- Number of classes: '.
		selection classes size printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		selection methods size printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		selection linesOfCode printOn: strm.
		strm nextPutAll: '.' ]! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/28/2012 16:49'!
                           buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames  upperRow description summary buttonRow diffsButton saveButton browseButton |
	dirtyFlags _ PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: Color white.
	dirtyFlags _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	names _ PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: Color white.
	names _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: 16;
		addMorphUseAll: names.

	fileNames _ PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: Color white.
	fileNames _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: 16;
		addMorphUseAll: fileNames.

	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.1;
		addAdjusterAndMorph: names proportionalWidth: 0.2;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.7.

	description _ TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.

	summary _ TextModelMorph
		textProvider: model
		textGetter: #summary.

	saveButton _ PluggableButtonMorph model: model action: #save label: 'Save (overwrite if exists)'.
	diffsButton _ PluggableButtonMorph model: self action: #diffs label: 'Diffs with saved'.
	browseButton _ PluggableButtonMorph model: self action: #browse label: 'Browse package code'.
	buttonRow _ LayoutMorph newRow.
	buttonRow
		color: self windowColor quiteWhiter;
		addMorph: (Morph new color: Color transparent) proportionalWidth: 0.1;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: (Morph new color: Color transparent)proportionalWidth: 0.1;
		addMorph: diffsButton proportionalWidth: 0.6;
		addMorph: (Morph new color: Color transparent) proportionalWidth: 0.1;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: (Morph new color: Color transparent) proportionalWidth: 0.1.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: summary proportionalHeight: 0.13;
		addAdjusterAndMorph: description proportionalHeight: 0.3;
		addAdjusterAndMorph: buttonRow proportionalHeight: 0.07.
	self setLabel: 'Installed Packages'! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 3/28/2012 09:15'!
                    openMenu
	"Build the open window menu for the world."

	| menu |
	menu _ self menu: 'Open...'.

	ExtraOpenCommands ifNotNil: [
		self fillIn: menu from: ExtraOpenCommands ].

	self fillIn: menu from: {
		{'Installed Packages' . { self . #openPackageList}. 'A tool for managing Packages (optional units of code) installed in the system'}.
		{'Browser' . { self . #openBrowser}. 'A Smalltalk code browser, for studying and modifying the system'}.
		{'Workspace' . {self . #openWorkspace}. 'A window for evaluating Smalltalk expressions' }.
		{'Text Editor' . {self . #openTextEditor}. 'A window for composing text' }.
		{'File List' . {self . #openFileList} . 'An explorer of the File System' }.
		{'Transcript' . {self . #openTranscript}. 'A window showing contents of the System Transcript' }.
		{ 'Process Browser' . { ProcessBrowserWindow . #openProcessBrowser } }.
		{ 'Emergency Evaluator'. { Transcripter. #emergencyEvaluator } }.
		nil.
		{'Message Names' . { self . #openMessageNames} . 'A tool for finding and editing methods that contain any given keyword in their names.'}.
		nil.
		{'Change Sorter' . {self . #openChangeSorter1} . 'A tool allowing you to view the methods in a Change Set, especially changes to the Base System' }.
		nil.
		{'SUnit Test Runner' . {TestRunnerWindow . #openTestRunner} . 'A tool allowing you to compare and manipulate two change sets concurrently' }.
	}.
	^menu! !


!Theme methodsFor: 'tool colors' stamp: 'jmv 3/28/2012 09:33'!
changeSorter
	^ self packageList! !

!Theme methodsFor: 'menus' stamp: 'jmv 3/28/2012 09:30'!
  miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history') -> #changesIcon.
		#('print PS to file...' ) -> #printIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' ) -> #cancelIcon.
		#('debug...' 'debug it' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'expand all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner') -> #weatherFewCloudsIcon.
		#('system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'what to show...') -> #preferencesIcon.
		#('command-key help') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set author initials...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'file out (o)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'profile messages (m)') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance' 'inspect Pointers (P)') -> #instanceIcon.
		#('Use Selection for Find (h)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('update' 'turn on auto-update (a)' 'update list (u)') -> #updateIcon.
		#('find changed windows...') -> #newWindowIcon.
		#('make undraggable') -> #pushPinIcon.
		#('Utilities saveScreenshot.') -> #stillCameraIcon.
		#('add new directory') -> #newFolderIcon.
		#('select all' 'deselect all') -> #selectAllIcon.
		#('sort by date') -> #dateIcon.
		#('justified') -> #formatJustifyFillIcon.
		#('centered') -> #formatJustifyCenterIcon.
		#('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon.
		#('rightFlush') -> #formatJustifyRightIcon.
		#('signal Semaphore (S)') -> #haloHelpIcon.
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...') -> #fontXGenericIcon.
	}! !

!methodRemoval: DarkTheme #changeSorter!
DarkTheme removeSelector: #changeSorter!

!CodePackageListWindow class reorganize!
('instance creation' openPackageList)
!

!methodRemoval: CodePackageList #installedPackagesChanged!
CodePackageList removeSelector: #installedPackagesChanged!
!methodRemoval: CodePackageList #packageRepositories!
CodePackageList removeSelector: #packageRepositories!
!classDefinition: #CodePackageList category: #'Package Support'!
ActiveModel subclass: #CodePackageList
	instanceVariableNames: 'packages selection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackageList reorganize!
('accessing' description description: editorClass packageDirtyFlags packageFullNames packageNames packages selectionIndex selectionIndex: summary)
('initialize-release' initialize)
('events' updateDirtyFlags updatePackageList)
('commands' save)
!

!classDefinition: #CodePackageFile category: #'Package Support'!
CodeFile subclass: #CodePackageFile
	instanceVariableNames: 'packageName description classesToRemove methodsToRemove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackageFile reorganize!
('initialize' buildFileStream:packageName:fullName: initialize installFileStream:packageName:fullName:)
('services' install:)
('accessing' classesToRemove methodsToRemove)
('change record types' doIt: possibleDescription:)
!

CodePackage initialize!
!methodRemoval: CodePackage #externalTraits!
CodePackage removeSelector: #externalTraits!
!methodRemoval: CodePackage #savedFrom!
CodePackage removeSelector: #savedFrom!
!classDefinition: #CodePackage category: #'Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges description sourceSystem'
	classVariableNames: 'InstalledPackages'
	poolDictionaries: ''
	category: 'Package Support'!
