'From Cuis7.3 [latest update: #7157] on 13 May 2025 at 4:05:00 pm'!

!LayoutSpec commentStamp: 'jmv 5/13/2025 15:52:09' prior: 0!
LayoutSpecs are the basis for the layout mechanism. Any Morph can be given a LayoutSpec, but in order to honor it, its owner must be a LayoutMorph.

A LayoutSpec specifies how a morph wants to be layed out. It can specify either a fixed width or a fraction of some available owner width. Same goes for height. If a fraction is specified, a minimum extent is also possible.


Alternatives:
	- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
	- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
	- proportionalWidth isNil, fixedWidth notNil	->		Use fixedWidth
	- proportionalWidth notNil, fixedWidth isNil	->		NOT VALID

Same goes for proportionalHeight and fixedHeight
!


!Form methodsFor: 'scaling, rotation' stamp: 'jmv 5/13/2025 15:41:55'!
flippedBy: direction
	"Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation)
	ImageMorph defaultForm display.
	(ImageMorph defaultForm flippedBy: #vertical) display.
	(ImageMorph defaultForm flippedBy: #horizontal) display.
	(ImageMorph defaultForm flippedBy: #both) display.
	"
	| newForm quad |
	newForm := self class extent: self extent depth: depth.
	quad := self boundingBox innerCorners.
	quad := (
		direction caseOf: {
			[ #vertical ] 		-> [#(2 1 4 3)].
			[ #horizontal ] 		-> [#(4 3 2 1)].
			[ #both ] 			-> [#(3 4 1 2)]})
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
"	newForm offset: (self offset flippedBy: direction centerAt: aPoint)."
	^ newForm
"
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p |
[ Sensor isAnyButtonPressed ] whileFalse: [
	f := Form fromDisplay: ((p := Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 := f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p ]
"! !


!Theme methodsFor: 'shout' stamp: 'jmv 5/13/2025 15:42:30'!
shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	^ {
		#selector  						-> nil.
		#arguments  					-> #(cyan muchDarker).
		#comment 						-> #(green muchDarker).
		#tempBar 						-> #gray.
		#tempVars 					-> #(gray muchDarker).
		#instVar 						-> #(magenta muchDarker).
		#pseudoVariables 				-> #(red muchDarker).
		#literals 						-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#incompleteMessages 			-> #(gray veryMuchDarker).
		#blockLevelZero 				-> #black.
		#blockLevelOne 				-> #brown.
		#blockLevelTwo 				-> #magenta.
		#blockLevelThree 				-> #red.
		#blockLevelFour 				-> #(orange darker).
		#blockLevelFive 				-> #(orange muchDarker).
		#blockLevelSix 				-> #(green muchDarker).
		#blockLevelSeven 				-> #blue.
		#defaults 						-> #black.
		#undefined 					-> #red.
		#methodTags 					-> #(green muchDarker).
	}! !


!DarkTheme methodsFor: 'colors' stamp: 'jmv 5/13/2025 15:43:05'!
shout
	"Color symbols as an association list."

	^ {
		#selector  						-> '#B59F60'.
		#arguments 					-> '#289078'.
		#comment 						-> #(green duller).
		#tempBar 						-> '#707070'.
		#tempVars 					-> '#767836'.
		#instVar 						-> '#B3695A'.
		#pseudoVariables 				-> '#2070E0'.
		#literals 						-> #green.
		#messages 					-> '#6FB3BD'.
		#incompleteMessages  		-> '#F08060'.
		#blockLevelZero 				-> '#6FB3BD'.
		#blockLevelOne 				-> '#FFB0B0'.
		#blockLevelTwo 				-> '#B0FFB0'.
		#blockLevelThree 				-> '#B0B0FF'.
		#blockLevelFour 				-> '#00B0B0'.
		#blockLevelFive 				-> '#E03030'.
		#blockLevelSix 				-> '#30E030'.
		#blockLevelSeven 				-> '#3030E0'.
		#defaults 						-> '#A1AFBF'.
		#undefined 					-> '#E04020'.
		#methodTags 					-> #green.
	}! !


!Workspace class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:49:45'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		20.
			#label 			-> 		'Workspace'.
			#object 		-> 		Workspace.
			#selector 		-> 		#open.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary}`! !


!FileList methodsFor: 'initialization' stamp: 'jmv 5/13/2025 15:45:27'!
topDirectoryWrappers

	topDirectories := (Preferences at: #initialFileListDirectories) 
		caseOf: {
			[ #cuis ]  	-> 	[ | topDirectoryToUse |
				topDirectoryToUse := directory. "must be inside the tree of some of the included"
				(topDirectoryToUse commonPathWith: DirectoryEntry projectBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry userBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry cuisBaseDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				(topDirectoryToUse commonPathWith: DirectoryEntry currentDirectory) ifNotNil: [ :ancestor |
					topDirectoryToUse := ancestor ].
				{ topDirectoryToUse} ].
			[ #roots ] 	-> [ DirectoryEntry roots ].
			}.
	^topDirectories collect: [ :dir |
		FileDirectoryWrapper with: dir name: (dir name ifNil: [ '/' ]) model: self ]! !


!Collection methodsFor: 'converting' stamp: 'jmv 5/13/2025 15:45:38'!
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


!Transcripter class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:50:02'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		20.
			#label 			-> 		'Emergency Evaluator'.
			#object 		-> 		Transcripter.
			#selector 		-> 		#emergencyEvaluator.
			#icon 			-> 		#emblemImportantIcon.
			#balloonText 	-> 		'When all else fails...'.
		} asDictionary}`! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 5/13/2025 15:46:14'!
smoothStep
	"A Function that is
		x <= -1 	-> 		1
		-1 < x < 1 	->		smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
Compiler evaluate: '
	| g |
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: ''graph'') openInWorld'
	"
	self isNaN ifTrue: [ ^self ].
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5! !


!Morph methodsFor: 'fileIn/out' stamp: 'jmv 5/13/2025 15:46:23'!
storeDataOn: aDataStream
	"Let all Morphs be written out.  All owners are weak references.  They only go out if the owner is in the tree being written."
	| cntInstVars cntIndexedVars ti instVarNames |

	"block my owner unless he is written out by someone else"
	cntInstVars := self class instSize.
	cntIndexedVars := self basicSize.
	instVarNames := self class allInstVarNames.
	ti := 1.  
	((instVarNames at: ti) = 'owner') & (Morph superclass == Object) ifFalse: [
		self error: 'this method is out of date'].
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do: [ :i |
		(instVarNames at: i) caseOf: {
				['owner']  				-> [aDataStream nextPutWeak: owner]. 	"owner only written if in our tree"
				['id']  					-> [ aDataStream nextPut: (id bitAnd: 255) ].  "Clear id, but keep flags."
				['privateDisplayBounds']	-> [ aDataStream nextPut: nil ].  "Lazy initialized."
			}
			otherwise: [ aDataStream nextPut: (self instVarAt: i)]].
	1 to: cntIndexedVars do: [ :i |
		aDataStream nextPut: (self basicAt: i)]! !


!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 5/13/2025 15:46:31'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 		-> 	[ angle := angle + 0.02. self redrawNeeded ].
			[ #down ] 	-> 	[ angle := (angle - 0.02) max: 0. self redrawNeeded ].
			[ #left ] 	-> 	[ depthLimit := depthLimit - 1 max: 1. self redrawNeeded].
			[ #right ] 	-> 	[ depthLimit := depthLimit + 1 min: 15. self redrawNeeded]
		}! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 5/13/2025 15:46:39'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 		-> 		[ scrollBar scrollUp: 1 ].
			[ #down ] 	-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 	-> 		[ hScrollBar scrollUp: 1 ].
			[ #right ] 	-> 		[ hScrollBar scrollDown: 1 ] }! !


!CodePackageListWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:48:05'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		10.
			#label 			-> 		'Installed Packages'.
			#object 		-> 		CodePackageList.
			#selector 		-> 		#open.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:47:09'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		30.
			#label 			-> 		'Browser'.
			#object 		-> 		Smalltalk.
			#selector 		-> 		#browse.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !


!ChangeSorterWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:47:19'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		20.
			#label 			-> 		'Change Sorter'.
			#object 		-> 		ChangeSorter.
			#selector 		-> 		#open.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`! !


!MessageNamesWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:47:35'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		40.
			#label 			-> 		'Message Names'.
			#object 		-> 		MessageNames.
			#selector 		-> 		#open.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`! !


!FileListWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:50:18'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		10.
			#label 			-> 		'File List'.
			#object 		-> 		FileList.
			#selector 		-> 		#open.
			#icon 			-> 		#systemFileManagerIcon.
			#balloonText 	-> 		'An explorer of the File System'.
		} asDictionary}`! !


!ProcessBrowserWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:50:33'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		10.
			#label 			-> 		'Process Browser'.
			#object 		-> 		ProcessBrowser.
			#selector 		-> 		#open.
			#icon 			-> 		#systemMonitorIcon.
			#balloonText 	-> 		'A tool to monitor and manage Smalltalk processes'.
		} asDictionary}`! !


!TestRunnerWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:50:43'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		20.
			#label 			-> 		'SUnit Test Runner'.
			#object 		-> 		TestRunner.
			#selector 		-> 		#open.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary}`! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:51:34'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		50.
			#label 			-> 		'Transcript'.
			#object 		-> 		Transcript.
			#selector 		-> 		#open.
			#icon 			-> 		#printerIcon.
			#balloonText 	-> 		'A window showing contents of the System Transcript'.
		} asDictionary}`! !


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 5/13/2025 15:52:16'!
proportionalWidth: aNumberOrNil minimum: otherNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil 	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil  		->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil 	->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil 	->		NOT VALID"
	proportionalWidth := aNumberOrNil.
	fixedWidth := otherNumberOrNil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 5/13/2025 15:52:21'!
setProportionalWidth: aNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil	->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil	->		NOT VALID"
	proportionalWidth := aNumberOrNil.
	self updateContainingLayout.! !


!DynamicMenuBuilder class methodsFor: 'examples' stamp: 'jmv 5/13/2025 15:52:34'!
exampleItemWithoutSubMenu

	^ {
		#itemGroup 	-> 		10.
		#itemOrder 	-> 		10.
		#label 			-> 		'item without submenu'.
		#selector 		-> 		#printString.
		#icon 			-> 		#editFindReplaceIcon
	} asDictionary! !

!DynamicMenuBuilder class methodsFor: 'examples' stamp: 'jmv 5/13/2025 15:52:47'!
exampleWithSubMenusDefinition

	| topItemWithSubMenus subItemWithSubMenu |
	
	topItemWithSubMenus := 'top item with submenu...'.
	subItemWithSubMenu := 'subitem with submenu...'.
	
	^ {
			self exampleItemWithoutSubMenu.
			{
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		10.
				#label 			-> 		topItemWithSubMenus.
				#selector 		-> 		nil.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus .
				#itemGroup 	-> 		10.
				#itemOrder 	-> 		10.
				#label 			-> 		'subitem without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus .
				#itemGroup 	-> 		10.
				#itemOrder 	-> 		20.
				#label 			-> 		subItemWithSubMenu.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> topItemWithSubMenus.
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		10.
				#label 			-> 		'another subitem without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> subItemWithSubMenu.
				#itemGroup 	-> 		10.
				#itemOrder 	-> 	10.
				#label 			-> 		'sub sub item without submenu'.
				#selector 		-> 		#printString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		}
	! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/13/2025 15:53:05'!
additionalMenuOptions

	^`{
		{
			#submenuOf  		-> 		TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		30.
			#label 				-> 		'Package Installer'.
			#selector 			-> 		#openPackageInstaller.
			#icon 				-> 		#packageIcon.
			#balloonText 		-> 		'A tool for installing Cuis packages from the local file system.'.
		} asDictionary.
		{
			#submenuOf  		-> 		TheWorldMenu openLabel.
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		40.
			#label 				-> 		'Package Downloader'.
			#selector 			-> 		#openPackageDownloader.
			#icon 				-> 		#packageIcon.
			#balloonText 		-> 		'A tool for downloading Cuis packages from the internet.'.
		} asDictionary.
	}`	
	! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/13/2025 15:53:46'!
changesMenuOptions

	^`{
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		10.
				#itemOrder 	-> 		10.
				#label 			-> 		'Change Sorter'.
				#object 		-> 		ChangeSorter.
				#selector 		-> 		#open.
				#icon 			-> 		#halfRefreshIcon.
				#balloonText 	-> 		'Open a 3-paned changed-set viewing tool'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		20.
				#itemOrder 	-> 		10.
				#label 			-> 		'Install New Updates'.
				#object 		-> 		ChangeSet.
				#selector 		-> 		#installNewUpdates.
				#icon 			-> 		#updateIcon.
				#balloonText 	-> 		'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		10.
				#label 			-> 		'Browse my Changes'.
				#object 		-> 		Smalltalk.
				#selector 		-> 		#browseMyChanges.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Browse all of my changes since the last time #condenseSources was run.'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		30.
				#itemOrder 	-> 		20.
				#label 			-> 		'Recently logged Changes...'.
				#object 		-> 		ChangeList.
				#selector 		-> 		#browseRecentLog.
				#icon 			-> 		#clockIcon.
				#balloonText 	-> 		'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'.
			} asDictionary.
			{
				#submenuOf  	-> 		TheWorldMenu changesLabel.
				#itemGroup 	-> 		40.
				#itemOrder 	-> 		10.
				#label 			-> 		'Save World as morph file'.
				#selector 		-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/13/2025 15:55:20'!
helpMenuOptions

	^ `{
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 			-> 		#aboutThisSystem.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'current version information.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Terse Guide to Cuis'.
				#selector 			-> 		#openTerseGuide.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 				-> 		'Class Comment Browser'.
				#selector 			-> 		#openCommentGuide.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'Search & explore Cuis Class Comments. Requires cloning "Cuis-Smalltalk-Tools" git repo.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		40.
				#label 				-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCodeManagementInCuis.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		50.
				#label 				-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openCuisAndGitHub.
				#icon 				-> 		#helpIcon.
				#balloonText 		-> 		'GitHub usage pattern.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 			-> 		#openHelp.
				#icon 				-> 		#keyboardShortcutsIcon.
				#balloonText 		-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 			-> 		#openUsefulExpressions.
				#icon 				-> 		#chatIcon.
				#balloonText 		-> 		'a window full of useful expressions.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 				-> 		'VM Statistics'.
				#selector 			-> 		#vmStatistics.
				#icon 				-> 		#systemMonitorIcon.
				#balloonText 		-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#submenuOf 		-> 		TheWorldMenu helpLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 				-> 		'Space Left'.
				#selector 			-> 		#garbageCollect.
				#icon 				-> 		#removableMediaIcon.
				#balloonText 		-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/13/2025 15:57:42'!
preferencesMenuOptions

	^`{
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'Focus follows Mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Focus when Click'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.

			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Show ST-80 assignments'.
				#selector 			-> 		#enableShowAssignmentAsLeftArrow.
				#icon 				-> 		#editFindReplaceIcon.
				#balloonText 		-> 		'Show variable assignment in Smalltalk-80 style (left arrow).'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Show ANSI assignments'.
				#selector 			-> 		#disableShowAssignmentAsLeftArrow.
				#icon 				-> 		#editFindReplaceIcon.
				#balloonText 		-> 		'Show variable assignment in ANSI style ( := ).'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 				-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 				-> 		#preferencesDesktopFontIcon.
				#balloonText 		-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label				-> 		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 				-> 		#preferencesDesktopFontIcon.
				#balloonText 		-> 		'change the current system font family.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		60.
				#label				-> 		'Load all TrueType Fonts'.
				#object 			-> 		TrueTypeFontFamily.
				#selector 			-> 		#readAdditionalFonts.
				#icon 				-> 		#preferencesDesktopFontIcon.
				#balloonText 		-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		70.
				#label 				-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 				-> 		#worldIcon.
				#balloonText 		-> 		'show more or less icons.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		80.
				#label 				-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 				-> 		#appearanceIcon.
				#balloonText 		-> 		'switch to another theme.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 				-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 				-> 		#expandIcon.
				#balloonText 		-> 		'show the taskbar'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 				-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 				-> 		#collapseIcon.
				#balloonText 		-> 		'hide the taskbar'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		10.
				#label 				-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 				-> 		#viewFullscreenIcon.
				#balloonText 		-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		50.
				#itemOrder 		-> 		20.
				#label 				-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 				-> 		#exitFullscreenIcon.
				#balloonText 		-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		10.
				#label 				-> 		'Save Prefs in UserPrefs.txt'.
				#selector 			-> 		#enableUsePrefsFile.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Preferences are saved without need to save the Smalltalk Image.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		20.
				#label 				-> 		'Save Prefs in the Image'.
				#selector 			-> 		#disableUsePrefsFile.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Preferences are lost unless the Smalltalk Image is saved.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		30.
				#label 				-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 				-> 		#usersIcon.
				#balloonText 		-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu preferencesLabel.
				#itemGroup 		-> 		60.
				#itemOrder 		-> 		40.
				#label 				-> 		'All preferences...'.
				#object 			-> 		PreferenceSet.
				#selector 			-> 		#openPreferencesInspector.
				#icon 				-> 		#preferencesIcon.
				#balloonText 		-> 		'view and change various options.'
			} asDictionary.
		}`.! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/13/2025 16:02:20'!
windowsMenuOptions

	^`{
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findWindow:.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyBrowsers:.
				#icon 				-> 		#editFindReplaceIcon.
				#balloonText 		-> 		'Presents a list of browsers that have unsaved changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 				-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyWindows:.
				#icon 				-> 		#newWindowIcon.
				#balloonText 		-> 		'Presents a list of all windows that have unsaved changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  		->  					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findATranscript:.
				#icon 				-> 		#printerIcon.
				#balloonText 		-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAFileList:.
				#icon 				-> 		#systemFileManagerIcon.
				#balloonText 		-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 				-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAChangeSorter:.
				#icon 				-> 		#changesIcon.
				#balloonText 		-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		40.
				#label 				-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAMessageNamesWindow:.
				#icon 				-> 		#inspectIcon.
				#balloonText 		-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		10.
				#label 				-> 		'Tile open windows'.
				#object          		-> 		TileResizeMorph.
				#selector 			-> 		#tileOpenWindows.
				#icon 				-> 		#windowIcon.
				#balloonText 		-> 		'Tile open windows'.
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		20.
				#label 				-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#collapseAll.
				#icon 				-> 		#collapseIcon.
				#balloonText 		-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		30.
				#label 				-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#restoreAll.
				#icon 				-> 		#expandIcon.
				#balloonText 		-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		40.
				#label 				-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#closeTopWindow.
				#icon 				-> 		#closeIcon.
				#balloonText 		-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		50.
				#label 				-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#sendTopWindowToBack.
				#icon 				-> 		#goBottomIcon.
				#balloonText 		-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		30.
				#itemOrder 		-> 		60.
				#label 				-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#bringWindowsFullOnscreen.
				#icon 				-> 		#displayIcon.
				#balloonText 		-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		10.
				#label 				-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#closeUnchangedWindows.
				#icon 				-> 		#warningIcon.
				#balloonText 		-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		20.
				#label 				-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#deleteNonWindows.
				#icon 				-> 		#warningIcon.
				#balloonText 		-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#submenuOf  		-> 					TheWorldMenu windowsLabel.
				#itemGroup 		-> 		40.
				#itemOrder 		-> 		30.
				#label 				-> 		'Delete Both of the Above'.
				#selector 			-> 		#cleanUpWorld.
				#icon 				-> 		#warningIcon.
				#balloonText 		-> 		'Deletes all unchanged windows and all non-window morphs on the world.'
			} asDictionary.
		}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/13/2025 16:03:49'!
worldMainMenuOptions

	^`{
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		10.
			#label 					-> 		TheWorldMenu openLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#openIcon.
			#balloonText			-> 		'Submenu to open various system tools'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		20.
			#label 					-> 		'New morph...'.
			#selector				-> 		#newMorph.
			#icon 					-> 		#morphsIcon.
			#balloonText 			-> 		'Offers a variety of ways to create new objects'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		30.
			#label 					-> 		TheWorldMenu preferencesLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#preferencesIcon.
			#balloonText 			-> 		'Opens a menu offering many controls over appearance and system preferences.'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		40.
			#label 					-> 		TheWorldMenu windowsLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#windowIcon.
			#balloonText 			-> 		'Manage open windows'.
		} asDictionary.
		{
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		50.
			#label 					-> 		TheWorldMenu helpLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#helpIcon.
			#balloonText 			-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
		} asDictionary.
		{
			#itemGroup 			-> 		20.
			#itemOrder 			-> 		10.
			#label 					-> 		TheWorldMenu changesLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#changesIcon.
			#balloonText 			-> 		'Opens a menu of useful tools for dealing with changes'.
		} asDictionary.
		{
			#itemGroup 			-> 		20.
			#itemOrder 			-> 		20.
			#label 					-> 		TheWorldMenu debugLabel.
			#selector 				-> 		#yourself.
			#icon 					-> 		#debugIcon.
			#balloonText 			-> 		'a menu of debugging items'.
		} asDictionary.
		{
			#itemGroup 			-> 		20.
			#itemOrder 			-> 		30.
			#label 					-> 		'Restore Display'.
			#object 				-> 		#myWorld.
			#selector 				-> 		#restoreDisplay.
			#icon 					-> 		#displayIcon.
			#balloonText 			-> 		'Repaint the screen -- useful for cleaning unwanted display artifacts.'.
		} asDictionary.
		{
			#itemGroup 			-> 		30.
			#itemOrder 			-> 		10.
			#label 					-> 		'Save Image'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveImage.
			#icon 					-> 		#saveIcon.
			#balloonText 			-> 		'Save the current state of the image on disk. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 			-> 		30.
			#itemOrder 			-> 		20.
			#label 					-> 		'Save Image as...'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAs.
			#icon 					-> 		#saveAsIcon.
			#balloonText 			-> 		'Save the current state of the image on disk under a new name.'.
		} asDictionary.
		{
			#itemGroup 			-> 		30.
			#itemOrder 			-> 		30.
			#label 					-> 		'Save Image and Quit'.
			#object 				-> 		Smalltalk.
			#selector 				-> 		#saveAndQuit.
			#icon 					-> 		#saveAndQuitIcon.
			#balloonText 			-> 		'Save the image and quit out of Cuis. Overwrite existing file.'.
		} asDictionary.
		{
			#itemGroup 			-> 		50.
			#itemOrder 			-> 		40.
			#label 					-> 		'Quit without saving'.
			#selector 				-> 		#quitSession.
			#icon 					-> 		#quitIcon.
			#balloonText 			-> 		('Quit out of Cuis without saving the image.\',
									'Ask for confirmation if there are unsaved changes.') withNewLines.
		} asDictionary.
	}`! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 5/13/2025 16:04:20'!
worldSaveReleaseImagesMenuOptions
	"
	Preferences
		name: #saveReleaseOptionsInWorldMenu
		description: 'Include options for saving Stable and Rolling Release images'
		category: #system type: Boolean value: false
	"

	^`{
				{
					#itemGroup 		-> 		40.
					#itemOrder 		-> 		10.
					#label 				-> 		'Save Stable Release'.
					#object 			-> 		Smalltalk.
					#selector 			-> 		#saveStableRelease.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 		-> 		('Save as a new official Stable Release of Cuis.\',
												'See naming in #saveStableRelease.\',
												'Clear all user preferences and class vars.\',
												'Write new compacted Sources file, an empty\',
												'Changes file, and save the Image on disk.\') withNewLines.
				} asDictionary.
				{
					#itemGroup 		-> 		40.
					#itemOrder 		-> 		20.
					#label 				-> 		'Save Rolling Release, Quit'.
					#object 			-> 		Smalltalk.
					#selector 			-> 		#saveRollingReleaseAndQuit.
					#icon 				-> 		#saveAsNewVersionIcon.
					#balloonText 		-> 		('Save as update of the Rolling Release of Cuis.\',
												'Clear all user preferences and class vars.\',
												'Save Image and Changes with update number.\',
												'Quit Cuis.\') withNewLines.
				} asDictionary.
			}`! !


!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 5/13/2025 16:04:28'!
machine: machinePref
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray selector |
	selector := machinePref caseOf: {
			[#default]		 		-> [#machineDefault].
			[#slow] 				-> [#machineSlow].
			[#smalltalk80] 			-> [#machineSmalltalk80] }
		otherwise: [ #machineDefault ].
	prefArray := self perform: selector.
	prefArray do: [:array | 		self sysPreferences at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil].! !


!TextEditor class methodsFor: 'menu-world' stamp: 'jmv 5/13/2025 15:51:20'!
worldMenuOptions
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		10.
			#label 			-> 		'Text Editor'.
			#object 		-> 		TextEditor.
			#selector 		-> 		#open.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary}`! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 5/13/2025 16:04:42'!
notInstallOrTestRun

	^Installing isNil or: [
		(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]]
	
	"
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name = 'TestRunner' ]	-> Test
	(Installing beginsWith: Install-')															-> Install
	Installing isNil 																			-> Normal
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]	-> Normal
	"! !

