'From Cuis 6.0 [latest update: #5718] on 26 March 2023 at 6:46:59 pm'!

!TestRunner methodsFor: 'updating' stamp: 'bp 3/26/2023 18:03:34'!
refreshTestRunner
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: ''.
	self displayDetails: ''! !

!TestRunner methodsFor: 'testing' stamp: 'bp 3/26/2023 17:23:07'!
isNotRunning
	^ running isNil! !

!TestRunner methodsFor: 'testing' stamp: 'bp 3/26/2023 17:16:29'!
isRunning
	^ running notNil! !


!TestRunnerWindow methodsFor: 'updating' stamp: 'bp 3/26/2023 18:27:20'!
isNotRunning

	^ model notNil and: [ model isNotRunning ]! !

!TestRunnerWindow methodsFor: 'updating' stamp: 'bp 3/26/2023 18:12:10'!
resetProgress
	progress
		label: '' subLabel: '';
		done: 0.0! !


!TestRunner methodsFor: 'accessing' stamp: 'bp 3/26/2023 18:45:36'!
timeSinceLastPassAsString: aResult
        (lastPass isNil or: [aResult hasPassed not]) ifTrue: [^ ''].
        ^ String newLineString , (DateAndTime now - lastPass) printString , ' since last Pass'! !

!TestRunner methodsFor: 'processing' stamp: 'bp 3/26/2023 18:11:03'!
runSuite: suite
	running ifNotNil: [ ^ self inform: 'already running' ].
	suite addDependent: self.
	totalTests := suite tests size.
	completedTests := 0.
	running := [
	[ result := suite run ] ensure: [
		running := nil.
		suite removeDependent: self.
		runSemaphore signal.
		UISupervisor whenUIinSafeState: [
			self updateWindow: result.
			self changed: #runTests ]]] newProcess.
	self runWindow.
	self changed: #runTests.
	running
		name: 'TestRunner';
		priority: Processor userSchedulingPriority;
		resume.! !

!TestRunner methodsFor: 'processing' stamp: 'bp 3/26/2023 18:11:17'!
runSuiteProfiled: suite
	running ifNotNil: [ ^ self inform: 'already running' ].
	suite addDependent: self.
	totalTests := suite tests size.
	completedTests := 0.
	running := [
	[ result := TimeProfileBrowser onBlock: [ suite run ]] ensure: [
		running := nil.
		suite removeDependent: self.
		runSemaphore signal.
		UISupervisor whenUIinSafeState: [
			self updateWindow: result.
			self changed: #runTests ]]] newProcess.
	self runWindow.
	self changed: #runTests.
	running
		name: 'TestRunner';
		priority: Processor userSchedulingPriority;
		resume.! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:32:14'!
buildDetailsText
	detailsText _ (TextModelMorph
		textProvider: model
		textGetter: #details) emptyTextDisplayMessage: 'Test run details (no results to display)'.
	detailsText hideScrollBarsIndefinitely.
	^detailsText! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 17:38:47'!
buildFilterButton
	| filterButton |
	filterButton := PluggableButtonMorph 
				model: model
				action: #setFilter
				label: 'Filter'.
	filterButton
		enableSelector: #isNotRunning;
		color: self runButtonColor.
	^filterButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 18:35:12'!
buildLowerPanes
	| failuresList errorsList column |
	progress := ProgressMorph label: '' subLabel: ''.
	self buildPassFailText.
	self buildDetailsText.
	failuresList := self buildFailuresList.
	errorsList := self buildErrorsList.
	column := LayoutMorph newColumn.
	column
		addMorph: progress fixedHeight: 50;
		addMorph: passFailText fixedHeight: 26;
		addMorph: detailsText fixedHeight: 52;
		addMorph: failuresList proportionalHeight: 0.6;
		addAdjusterAndMorph: errorsList proportionalHeight: 0.6.
	^ column! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 17:38:34'!
buildRefreshButton
	| refreshButton |
	refreshButton := PluggableButtonMorph 
				model: self
				action: #refreshTests
				label: 'Refresh'.
	refreshButton
		enableSelector: #isNotRunning;
		color: self runButtonColor.
	^refreshButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 17:30:12'!
buildRunButton
	| runButton |
	runButton := PluggableButtonMorph 
				model: model
				action: #runTests
				label: 'Run all selected'.
	runButton
		enableSelector: #isNotRunning;
		color: self runButtonColor.
	^runButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 17:30:07'!
buildRunOneButton
	| runOneButton |
	runOneButton := PluggableButtonMorph 
				model: model
				action: #runOneTest
				label: 'Run last selected'.
	runOneButton
		enableSelector: #isNotRunning;
		color: self runButtonColor.
	^runOneButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 17:29:59'!
buildRunProfiledButton
	| runProfiledButton |
	runProfiledButton := PluggableButtonMorph 
				model: model
				action: #runProfiledTests
				label: 'Run Profiled'.
	runProfiledButton
		enableSelector: #isNotRunning;
		color: self runButtonColor.
	^runProfiledButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 17:32:17'!
buildStopButton
	| stopButton |
	stopButton := PluggableButtonMorph 
				model: model
				action: #terminateRun
				label: 'Stop'.
	stopButton
		enableSelector: #isRunning;
		color: self runButtonColor.
	^stopButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'bp 3/26/2023 17:37:02'!
optionalButtonRow
	| row button buttons widths buttonColor |

	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | 
		widths add: tuple first.
		button := PluggableButtonMorph 
			model: model
			action: tuple third.
		button
			enableSelector: #isNotRunning;
			color: buttonColor;
			label: tuple second.
		buttons add: button ].
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !

!TestRunnerWindow methodsFor: 'updating' stamp: 'bp 3/26/2023 18:12:22'!
refreshWindow
	| pc |
	pc := self widgetsColor.
	passFailText color: pc.
	detailsText color: pc.
	self resetProgress.
	model refreshTestRunner! !

!TestRunnerWindow methodsFor: 'updating' stamp: 'bp 3/26/2023 18:22:27'!
update: aSymbol
	super update: aSymbol.
	aSymbol == #runTests ifTrue: [
		self
			updateColors;
			resetProgress ].
	aSymbol == #progress ifTrue: [
		model ifNotNil: [
			progress label: 'Currently running...' subLabel: model progressLabel.
			progress done: model completedFraction asFloat ]]! !

!TestRunnerWindow methodsFor: 'updating' stamp: 'bp 3/26/2023 17:39:40'!
updateColors
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
		self updatePartColors: theColor ]! !

!TestRunnerWindow methodsFor: 'menu building' stamp: 'bp 3/26/2023 17:39:30'!
listMenu
	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Test Cases'.
	aMenu add: 'select all' target: model action: #selectAll icon: #selectAllIcon.
	aMenu add: 'deselect all' target: model action: #deselectAll icon: #selectAllIcon.
	aMenu add: 'toggle selections' target: model action: #invertSelections icon: #switchIcon.
	aMenu add: 'filter' target: model action: #setFilter icon: #findIcon.
	model isRunning ifTrue: [
		aMenu add: 'stop' target: model action: #terminateRun icon: #cancelIcon ].
	model selectedSuite > 0 ifTrue: [ | cls |
		cls := (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
		cls := cls asSymbol.
		cls := (Smalltalk at: cls ifAbsent: nil).
		cls ifNotNil: [
			aMenu addLine.
			(aMenu add: 'browse' target: self action: #browse: argument: cls)
				setIcon: #editFindReplaceIcon.
		].
	].
	aMenu addLine.
	aMenu add: 'log to Transcript' target: model action: #showResult icon: #printerIcon.
	^aMenu! !

!methodRemoval: TestRunnerWindow #isRunning stamp: 'bp 3/26/2023 17:43:06'!
TestRunnerWindow removeSelector: #isRunning!
!methodRemoval: TestRunner #runButtonState stamp: 'bp 3/26/2023 17:39:56'!
TestRunner removeSelector: #runButtonState!
!methodRemoval: TestRunner #refreshTR stamp: 'bp 3/26/2023 18:03:37'!
TestRunner removeSelector: #refreshTR!
!methodRemoval: TestRunner #stopButtonState stamp: 'bp 3/26/2023 17:39:52'!
TestRunner removeSelector: #stopButtonState!

!TestRunner reorganize!
('accessing' completedFraction details errors errorsList failures failuresList listSelectionAt: listSelectionAt:put: passFail progressLabel result selectedTests tests timeSinceLastPassAsString:)
('menus' deselectAll invertSelections selectAll setFilter terminateRun)
('processing' addTestsFor:toSuite: debugErrorTest: debugFailureTest: refreshTests runOneTest runProfiledTests runSuite: runSuiteProfiled: runTests selectedErrorTest selectedFailureTest selectedSuite selectedSuite:)
('updating' displayDetails: displayErrors: displayFailures: displayPassFail: refreshTestRunner runWindow update: updateDetails: updateErrors: updateFailures: updatePassFail: updateWindow:)
('initialization' gatherTestNames initialize testCases)
('logging' errorLog showResult showResultDefects showResultSummary)
('testing' isNotRunning isRunning)
!

