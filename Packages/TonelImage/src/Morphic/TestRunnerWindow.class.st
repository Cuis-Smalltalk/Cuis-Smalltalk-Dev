"
Morphic view for TestRunner models. See category 'GUI building'.
"
Class {
	#name : 'TestRunnerWindow',
	#superclass : 'SystemWindow',
	#instVars : [
		'passFailText',
		'detailsText',
		'progress'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu-world' }
TestRunnerWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		40.
			#itemOrder 	-> 		20.
			#label 			-> 		'SUnit Test Runner'.
			#object 		-> 		TestRunner.
			#selector 		-> 		#open.
			#icon 			-> 		#weatherFewCloudsIcon.
			#balloonText 	-> 		'Smalltalk''s testing framework'.
		} asDictionary}`
]

{ #category : 'menu commands' }
TestRunnerWindow >> browseSelected [
	| cls |
	cls := (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
	cls := cls asSymbol.
	cls := (Smalltalk at: cls ifAbsent: nil).
	cls ifNotNil: [
		Smalltalk browseFull: cls selector: nil ]
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildDetailsText [
	detailsText := (TextModelMorph
		textProvider: model
		textGetter: #details) emptyTextDisplayMessage: 'Test run details (no results to display)'.
	detailsText hideScrollBarsIndefinitely.
	^detailsText
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildErrorsList [
	^ PluggableListMorph
		withModel: model
		listGetter: #errorsList
		indexGetter: #selectedErrorTest
		indexSetter: #debugErrorTest:
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildFailuresList [
	^ PluggableListMorph
		withModel: model
		listGetter: #failuresList
		indexGetter: #selectedFailureTest
		indexSetter: #debugFailureTest:
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildFilterButton [
	| filterButton |
	self flag: #assumesKeyboardShortcuts.
	filterButton := PluggableButtonMorph 
				model: model
				action: #setFilter
				label: 'Filter'.
	filterButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'select tests matching a string pattern (f)'.
	^filterButton
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildLowerPanes [
	| failuresList errorsList column |
	progress := ProgressMorph label: '' subLabel: ''.
	self buildPassFailText.
	self buildDetailsText.
	failuresList := self buildFailuresList.
	errorsList := self buildErrorsList.
	column := LinearLayoutMorph newColumn.
	column
		addMorph: progress fixedHeight: 50;
		addMorph: passFailText fixedHeight: 26;
		addMorph: detailsText fixedHeight: 52;
		addMorph: failuresList proportionalHeight: 0.6;
		addAdjusterAndMorph: errorsList proportionalHeight: 0.6.
	^ column
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildMorphicWindow [

	self layoutMorph
		addMorph: self buildUpperControls proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.75.
	self setLabel: 'SUnit Test Runner'.
	self refreshWindow.
	self morphExtent: `460 @ 400`
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildPassFailText [
	passFailText := (TextModelMorph
		textProvider: model
		textGetter: #passFail) emptyTextDisplayMessage: 'Pass/Fail summary (no results to display)'.
	passFailText hideScrollBarsIndefinitely.
	^ passFailText
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildRefreshButton [
	| refreshButton |
	self flag: #assumesKeyboardShortcuts.
	refreshButton := PluggableButtonMorph 
				model: model
				action: #refreshTests
				label: 'Refresh'.
	refreshButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'clear and update the tests list (u)'.
	^refreshButton
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildRunButton [
	| runButton |
	self flag: #assumesKeyboardShortcuts.
	runButton := PluggableButtonMorph 
				model: model
				action: #runTests
				label: 'Run'.
	runButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run all tests selected (r)'.
	^runButton
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildRunOneButton [
	| runOneButton |
	self flag: #assumesKeyboardShortcuts.
	runOneButton := PluggableButtonMorph 
				model: model
				action: #runOneTest
				label: 'Run One'.
	runOneButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run one selected test (o)'.
	^runOneButton
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildRunProfiledButton [
	| runProfiledButton |
	self flag: #assumesKeyboardShortcuts.
	runProfiledButton := PluggableButtonMorph 
				model: model
				action: #runProfiledTests
				label: 'Run Profiled'.
	runProfiledButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run and profile all tests selected (p)'.
	^runProfiledButton
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildStopButton [
	| stopButton |
	self flag: #assumesKeyboardShortcuts.
	stopButton := PluggableButtonMorph 
				model: model
				action: #interruptRun
				label: 'Stop'.
	stopButton
		enableSelector: #isRunning;
		color: self runButtonColor;
		setBalloonText: 'interrupt and debug the currently running test (l)'.
	^stopButton
]

{ #category : 'GUI building' }
TestRunnerWindow >> buildUpperControls [
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton buttonsRow theTestsList column |

	runOneButton := self buildRunOneButton.
	runButton := self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	

	refreshButton := self buildRefreshButton.
	filterButton := self buildFilterButton.
	stopButton := self buildStopButton.

	theTestsList := (PluggableListMorphOfMany
				withModel: model
				listGetter: #tests
				primarySelectionGetter: #selectedSuite
				primarySelectionSetter: #selectedSuite:
				listSelectionGetter: #listSelectionAt:
				listSelectionSetter: #listSelectionAt:put:)
					menuNshortcutsKey: #listMenuSpec.
	theTestsList autoDeselect: false.
	theTestsList color: self textBackgroundColor.

	buttonsRow := LinearLayoutMorph newRow.
	buttonsRow doAdoptWidgetsColor.
	buttonsRow color: self buttonColor.
	buttonsRow
		addMorphs: {runButton. runOneButton. runProfiledButton};
		addMorphs: {refreshButton. filterButton. stopButton}.

	column := LinearLayoutMorph newColumn.
	column
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: buttonsRow fixedHeight: self defaultButtonPaneHeight.

	^ column
]

{ #category : 'updating' }
TestRunnerWindow >> isNotRunning [

	^ model notNil and: [ model isNotRunning ]
]

{ #category : 'menu & kbd shortcuts' }
TestRunnerWindow >> listMenuSpec [
	"Spec for the menu and shortcut keys for the list of test classes.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(0 		'Test Cases Set' 		false)
		#(10	'Select all (a)' 		$a 	model 		selectAll 			selectAllIcon)
		#(20	'Deselect all (A)' 		$A 	model 		deselectAll 			selectAllIcon)
		#(30	'Invert selections' 	nil 	model 		invertSelections 	switchIcon)
		#(40	'Filter (f)' 			$f 	model 		setFilter 			findIcon)
		#(50	'Refresh (u)' 			$u 	model 		refreshTests 		refreshIcon)
		#(60 	'-----')).
	model isRunning ifTrue: [ answer := answer, #(
		#(70	'Stop (l)' 			$l 	model 		interruptRun 		stopIcon))]
	ifFalse: [ answer := answer, #(
		#(70	'Run (r)' 				$r 	model 		runTests 			playIcon)
		#(80	'Run One (o)' 		$o 	model 		runOneTest 		playIcon)
		#(90	'Run Profiled (p)' 		$p 	model 		runProfiledTests 	playIcon))].
	model selectedSuite > 0 ifTrue: [ answer := answer, #(
		#(100 	'-----')
		#(110	'Browse' 			nil 	window 	browseSelected 	editFindReplaceIcon))].
	answer := answer, #(
		#(100 	'-----')
		#(110	'Log to Transcript' 	nil 	model 		showResult 		printerIcon)).
	^answer
]

{ #category : 'updating' }
TestRunnerWindow >> refreshTests [

	model refreshTests.
	self refreshWindow
]

{ #category : 'updating' }
TestRunnerWindow >> refreshWindow [
	| pc |
	pc := self widgetsColor.
	passFailText color: pc.
	detailsText color: pc.
	self resetProgress.
	model refreshTestRunner
]

{ #category : 'updating' }
TestRunnerWindow >> resetProgress [
	progress
		label: '' subLabel: '';
		done: 0.0
]

{ #category : 'constants' }
TestRunnerWindow >> runButtonColor [
	^ `Color green lighter duller`
]

{ #category : 'updating' }
TestRunnerWindow >> update: aSymbol [
	super update: aSymbol.
	aSymbol == #runTests ifTrue: [ self updateColors; resetProgress ].
	aSymbol == #progress ifTrue: [
		model ifNotNil: [
			progress label: model completedTests printString, '/', model totalTests printString subLabel: model progressLabel.
			progress done: model completedTests / model totalTests asFloat ]]
]

{ #category : 'updating' }
TestRunnerWindow >> updateColors [
	| aTestResult theColor |
	theColor := self widgetsColor.
	model ifNotNil: [
		model isRunning ifFalse: [
			aTestResult := model result.
			theColor := aTestResult errors size + aTestResult failures size = 0
				ifTrue: [ Theme current successColor ]
				ifFalse: [
					aTestResult errors size > 0
						ifTrue: [ Theme current errorColor ]
						ifFalse: [ Theme current failureColor ]]].
		self updatePartColors: theColor ]
]

{ #category : 'updating' }
TestRunnerWindow >> updatePartColors: aColor [

	passFailText color: aColor.
	 detailsText color: aColor
]

{ #category : 'GUI building' }
TestRunnerWindow >> windowColor [
	^ Theme current testRunner
]
