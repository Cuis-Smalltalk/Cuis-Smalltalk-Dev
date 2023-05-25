'From Cuis 6.0 [latest update: #5801] on 25 May 2023 at 5:22:55 am'!

!TimeProfileBrowser methodsFor: 'private' stamp: 'HAW 5/24/2023 16:56:33'!
initialize

	super initialize.
	shouldAutoHighlight := false.
	! !


!TestRunner methodsFor: 'accessing' stamp: 'len 5/23/2023 23:41:02'!
completedTests
	^ completedTests! !

!TestRunner methodsFor: 'accessing' stamp: 'len 5/23/2023 23:41:16'!
totalTests
	^ totalTests! !

!TestRunner methodsFor: 'menus' stamp: 'len 5/24/2023 14:35:35'!
interruptRun
	running ifNotNil: [:aProcess| aProcess debugWithTitle: 'Interrupted Test']! !

!TestRunner methodsFor: 'processing' stamp: 'HAW 5/24/2023 17:14:41'!
changedRunning
	
	self changed: #isRunning; changed: #isNotRunning! !

!TestRunner methodsFor: 'processing' stamp: 'HAW 5/24/2023 17:12:53'!
runSuite: suite with: aBlock
	running ifNotNil: [ ^ self inform: 'already running' ].
	suite addDependent: self.
	totalTests := suite tests size.
	completedTests := 0.
	running := [
	[ result := aBlock value ] ensure: [
		running := nil.
		suite removeDependent: self.
		runSemaphore signal.
		UISupervisor whenUIinSafeState: [
			self updateWindow: result.
			self suiteRan. ]]] newProcess.
	self runWindow.
	self suiteRan.
	running
		name: 'TestRunner';
		priority: Processor userSchedulingPriority;
		resume.! !

!TestRunner methodsFor: 'processing' stamp: 'HAW 5/24/2023 17:14:34'!
suiteRan

	self changed: #runTests; changedRunning
	! !


!TestRunnerWindow methodsFor: 'keyboard shortcuts' stamp: 'len 5/24/2023 15:03:00'!
listKey: aChar from: view 
	aChar = $a ifTrue: [^ model selectAll].
	aChar = $A ifTrue: [^ model deselectAll].
	aChar = $u ifTrue: [^ self refreshTests].
	aChar = $f ifTrue: [^ model setFilter].
	aChar = $r ifTrue: [^ model runTests].
	aChar = $o ifTrue: [^ model runOneTest].
	aChar = $p ifTrue: [^ model runProfiledTests].
	aChar = $l ifTrue: [^ model interruptRun]! !


!Theme methodsFor: 'icons' stamp: 'len 5/24/2023 15:10:56'!
playIcon
	^ self fetch: #( '16x16' 'actions' 'media-playback-start' )! !

!Theme methodsFor: 'icons' stamp: 'len 5/24/2023 15:07:40'!
refreshIcon
	^ self fetch: #( '16x16' 'actions' 'view-refresh')! !

!Theme methodsFor: 'icons' stamp: 'len 5/24/2023 15:11:04'!
stopIcon
	^ self fetch: #( '16x16' 'actions' 'media-playback-stop' )! !


!TestCase methodsFor: 'copying' stamp: 'len 5/25/2023 05:19:35'!
copy
	^ self class selector: testSelector! !


!TestResult methodsFor: 'testing' stamp: 'len 5/23/2023 18:37:09'!
is: aSymbol
	^aSymbol == #TestResult or: [ super is: aSymbol ]! !


!TestRunner methodsFor: 'menus' stamp: 'len 5/24/2023 14:47:45'!
setFilter
	self
		request: 'Add test cases by pattern (# and * allowed)'
		initialAnswer: '*'
		do: [:aString|
			filter := aString.
			(filter endsWith: '*') ifFalse: [ filter := filter, '*' ].
			selectedSuites := (tests asOrderedCollection with: selectedSuites collect: [ :ea :sel |
				sel or: [ filter match: ea asString ]
			]).
			selectedSuite := selectedSuites indexOf: true ifAbsent: [0].
			self changed: #allSelections]! !

!TestRunner methodsFor: 'menus' stamp: 'HAW 5/24/2023 17:14:22'!
terminateRun
	running ifNotNil: [:aProcess|
		aProcess terminate. running := nil.
		self changedRunning]! !

!TestRunner methodsFor: 'processing' stamp: 'len 5/23/2023 18:46:29'!
debugErrorTest: anInteger
	self terminateRun.
	selectedErrorTest := anInteger.  "added rew"
	selectedFailureTest := 0.                        "added rew"
	self changed: #selectedFailureTest.             "added rew"
	self changed: #selectedErrorTest.               "added rew"
	anInteger ~= 0 ifTrue: [(result errors at: anInteger) debug]! !

!TestRunner methodsFor: 'processing' stamp: 'len 5/23/2023 18:46:42'!
debugFailureTest: anInteger
	self terminateRun.
	anInteger ~= 0 ifTrue: [(self failures at: anInteger) debugAsFailure].
	selectedFailureTest := anInteger.
	selectedErrorTest := 0.
	self changed: #selectedErrorTest.
	self changed: #selectedFailureTest! !

!TestRunner methodsFor: 'processing' stamp: 'HAW 5/24/2023 17:10:08'!
runSuite: suite
	
	self runSuite: suite with: [ suite run ]! !

!TestRunner methodsFor: 'processing' stamp: 'HAW 5/24/2023 17:09:05'!
runSuiteProfiled: suite
	
	self runSuite: suite with: [ TimeProfileBrowser onBlock: [ suite run ]]! !

!TestRunner methodsFor: 'updating' stamp: 'len 5/23/2023 23:47:25'!
runWindow
	
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: 'Running...'.
	self displayDetails: ' '! !

!TestRunner methodsFor: 'updating' stamp: 'len 5/23/2023 18:49:30'!
update: aParameter 
	"updates come in from another thread"
	(aParameter is: #TestResult) ifTrue:
		[UISupervisor whenUIinSafeState:
			[result := aParameter.
			self updateErrors: aParameter.
			self updateFailures: aParameter].
		^ self].
	(aParameter is: #TestCase) ifTrue:
		[UISupervisor whenUIinSafeState:
			[completedTests := completedTests + 1.
			progressLabel := aParameter printString.
			self changed: #progress].
		^ self].
	super update: aParameter! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/24/2023 14:53:07'!
buildFilterButton
	| filterButton |
	filterButton := PluggableButtonMorph 
				model: model
				action: #setFilter
				label: 'Filter'.
	filterButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'select tests matching a string pattern (f)'.
	^filterButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/23/2023 10:06:50'!
buildMorphicWindow

	self layoutMorph
		addMorph: self buildUpperControls proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.75.
	self setLabel: 'Test Runner'.
	self refreshWindow.
	self morphExtent: `460 @ 400`! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/24/2023 14:53:29'!
buildRefreshButton
	| refreshButton |
	refreshButton := PluggableButtonMorph 
				model: model
				action: #refreshTests
				label: 'Refresh'.
	refreshButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'clear and update the tests list (u)'.
	^refreshButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/24/2023 14:53:38'!
buildRunButton
	| runButton |
	runButton := PluggableButtonMorph 
				model: model
				action: #runTests
				label: 'Run'.
	runButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run all tests selected (r)'.
	^runButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/24/2023 14:53:48'!
buildRunOneButton
	| runOneButton |
	runOneButton := PluggableButtonMorph 
				model: model
				action: #runOneTest
				label: 'Run One'.
	runOneButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run one selected test (o)'.
	^runOneButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/24/2023 14:53:59'!
buildRunProfiledButton
	| runProfiledButton |
	runProfiledButton := PluggableButtonMorph 
				model: model
				action: #runProfiledTests
				label: 'Run Profiled'.
	runProfiledButton
		enableSelector: #isNotRunning;
		color: self runButtonColor;
		setBalloonText: 'run and profile all tests selected (p)'.
	^runProfiledButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/24/2023 14:54:07'!
buildStopButton
	| stopButton |
	stopButton := PluggableButtonMorph 
				model: model
				action: #interruptRun
				label: 'Stop'.
	stopButton
		enableSelector: #isRunning;
		color: self runButtonColor;
		setBalloonText: 'interrupt and debug the currently running test (l)'.
	^stopButton! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'len 5/23/2023 10:02:31'!
buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton buttonsRow theTestsList column |

	runOneButton := self buildRunOneButton.
	runButton := self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	

	refreshButton := self buildRefreshButton.
	filterButton := self buildFilterButton.
	stopButton := self buildStopButton.

	theTestsList := PluggableListMorphOfMany
				model: model
				listGetter: #tests
				primarySelectionGetter: #selectedSuite
				primarySelectionSetter: #selectedSuite:
				listSelectionGetter: #listSelectionAt:
				listSelectionSetter: #listSelectionAt:put:
				mainView: self
				menuGetter: #listMenu
				keystrokeAction: #listKey:from:.
	theTestsList autoDeselect: false.
	theTestsList color: self textBackgroundColor.

	buttonsRow := LayoutMorph newRow.
	buttonsRow doAdoptWidgetsColor.
	buttonsRow color: self buttonColor.
	buttonsRow
		addMorphs: {runButton. runOneButton. runProfiledButton};
		addMorphs: {refreshButton. filterButton. stopButton}.

	column := LayoutMorph newColumn.
	column
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: buttonsRow fixedHeight: self defaultButtonPaneHeight.

	^ column! !

!TestRunnerWindow methodsFor: 'updating' stamp: 'len 5/23/2023 23:45:28'!
update: aSymbol
	super update: aSymbol.
	aSymbol == #runTests ifTrue: [ self updateColors; resetProgress ].
	aSymbol == #progress ifTrue: [
		model ifNotNil: [
			progress label: model completedTests printString, '/', model totalTests printString subLabel: model progressLabel.
			progress done: model completedTests / model totalTests asFloat ]]! !

!TestRunnerWindow methodsFor: 'menu building' stamp: 'len 5/24/2023 15:11:45'!
listMenu
	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Test Cases';
		add: 'select all (a)' target: model action: #selectAll icon: #selectAllIcon;
		add: 'deselect all (A)' target: model action: #deselectAll icon: #selectAllIcon;
		add: 'invert selections' target: model action: #invertSelections icon: #switchIcon;
		add: 'filter (f)' target: model action: #setFilter icon: #findIcon;
		add: 'refresh (u)' target: model action: #refreshTests icon: #refreshIcon;
		addLine.
	model isRunning
		ifTrue:
			[aMenu add: 'stop (l)' target: model action: #interruptRun icon: #stopIcon]
		ifFalse:
			[aMenu add: 'run (r)' target: model action: #runTests icon: #playIcon;
				add: 'run one (o)' target: model action: #runOneTest icon: #playIcon;
				add: 'run profiled (p)' target: model action: #runProfiledTests icon: #playIcon].
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


!TestCase methodsFor: 'debugging' stamp: 'len 5/25/2023 05:20:04'!
debug
	self setUpResources.
	[self copy runCase] ensure: [self tearDownResources]
			! !

!TestCase methodsFor: 'debugging' stamp: 'len 5/25/2023 05:20:25'!
debugAsFailureIfCanNot: handler
	self ifCanNotDebugDo: [^ handler value].
	self copy runCaseAsFailure! !


!TestSuite methodsFor: 'running' stamp: 'len 5/23/2023 18:31:17'!
run: aResult

	| skipFailures t1 t2 |
	t1 := Time localMillisecondClock.
	skipFailures := self tests size > 1.
	self tests do: [:each | 
		self changed: each.
		each run: aResult skipExpectedFailures: skipFailures.
		self changed: aResult.
		"Allow for some responsivenes of the UI, or any other stuff that may need running"
		t2 := Time localMillisecondClock.
		t2-t1 > 200 ifTrue: [
			Processor yield. 
			t1 := t2 ]]! !

!methodRemoval: TestRunnerWindow #classListKey:from: stamp: 'len 5/23/2023 09:45:03'!
TestRunnerWindow removeSelector: #classListKey:from:!
!methodRemoval: TestRunnerWindow #optionalModelButtonTuples stamp: 'len 5/23/2023 10:22:10'!
TestRunnerWindow removeSelector: #optionalModelButtonTuples!
!methodRemoval: TestRunnerWindow #optionalButtonRow stamp: 'len 5/23/2023 10:22:18'!
TestRunnerWindow removeSelector: #optionalButtonRow!
!methodRemoval: TestRunner #terminateRunAndDebug stamp: 'len 5/23/2023 23:53:37'!
TestRunner removeSelector: #terminateRunAndDebug!
!methodRemoval: TestRunner #completedFraction stamp: 'len 5/23/2023 23:44:52'!
TestRunner removeSelector: #completedFraction!

!TimeProfileBrowser reorganize!
('message list' messageListIndex messageListIndex:)
('private' initialize initializeMessageList:talliesList: runBlock:)
!


!TestRunner reorganize!
('accessing' completedTests details errors errorsList failures failuresList listSelectionAt: listSelectionAt:put: passFail progressLabel result selectedTests tests timeSinceLastPassAsString: totalTests)
('initialization' gatherTestNames initialize testCases)
('logging' errorLog showResult showResultDefects showResultSummary)
('menus' deselectAll interruptRun invertSelections selectAll setFilter terminateRun)
('processing' addTestsFor:toSuite: changedRunning debugErrorTest: debugFailureTest: refreshTests runOneTest runProfiledTests runSuite: runSuite:with: runSuiteProfiled: runTests selectedErrorTest selectedFailureTest selectedSuite selectedSuite: suiteRan)
('testing' isNotRunning isRunning)
('updating' displayDetails: displayErrors: displayFailures: displayPassFail: refreshTestRunner runWindow update: updateDetails: updateErrors: updateFailures: updatePassFail: updateWindow:)
!


!PluggableListMorphOfMany reorganize!
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('initialization' model:listGetter:primarySelectionGetter:primarySelectionSetter:listSelectionGetter:listSelectionSetter:mainView:menuGetter:keystrokeAction:)
('updating' update:)
('model access' itemSelectedAmongMultiple: listSelectionAt: listSelectionAt:put:)
('testing' allowsMultipleSelection)
!


!TestRunnerWindow reorganize!
('GUI building' buildDetailsText buildErrorsList buildFailuresList buildFilterButton buildLowerPanes buildMorphicWindow buildPassFailText buildRefreshButton buildRunButton buildRunOneButton buildRunProfiledButton buildStopButton buildUpperControls windowColor)
('constants' runButtonColor)
('updating' isNotRunning refreshTests refreshWindow resetProgress update: updateColors updatePartColors:)
('menu building' listMenu)
('menu commands' browse:)
('keyboard shortcuts' listKey:from:)
!


!TestCase reorganize!
('accessing' errored: methodForTest raisedError resources selector setTestSelector:)
('assertions' assert: assert:changes: assert:changes:by: assert:changes:from:to: assert:description: assert:description:resumable: assert:doesNotChange: assert:equals: assert:equalsExpected: assert:includes: assert:isCloseTo: assert:isCloseTo:withPrecision: assert:isNotCloseTo: assert:isNotCloseTo:withPrecision: comparingStringBetween:and: defaultPrecision deny: deny:description: deny:description:resumable: is:closeTo:withPrecision: should: should:description: should:notTakeMoreThan: should:raise: should:raise:description: should:raise:withExceptionDo: should:raise:withExceptionDo:description: should:raise:withMessageText: shouldFail: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description: shouldntFail:)
('copying' copy)
('debugging' debug debugAsFailure debugAsFailureIfCanNot:)
('debugging - private' canNotDebugMethodErrorDescription ifCanNotDebugDo: openDebuggerOnFailingTestMethod signalCanNotDebugMethod)
('dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('failing' defaultFailDescription expectedFailures fail failWith: failureString signalFailure:)
('log' failureLog logFailure:)
('printing' printOn:)
('running' run run:skipExpectedFailures: runCase runCaseAsFailure)
('running - private' performTest)
('setUp/tearDown' setUp setUpResources tearDown tearDownResources)
('testing' is: isLogging isSameAs:)
!


!TestResult reorganize!
('accessing' correctCount defects errorCount errors failureCount failures passed passedCount removeFromDefectsAndAddToPassed: runCount tests)
('logging' reportAboutToRun: reportError:because: reportFailed:because: reportPassed:)
('printing' printOn:)
('running' runCase:)
('testing' hasErrors hasFailures hasPassed is: isError: isFailure: isPassed:)
!

