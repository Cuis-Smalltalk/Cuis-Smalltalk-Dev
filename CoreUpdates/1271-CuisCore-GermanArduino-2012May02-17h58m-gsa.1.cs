'From Cuis 4.0 of 21 April 2012 [latest update: #1270] on 2 May 2012 at 6:42:19 pm'!

!TestRunner methodsFor: 'processing' stamp: 'gsa 5/2/2012 18:39'!
runProfiledTests
	| suite |
	Cursor execute showWhile: [
		suite _ TestSuite new name: 'TestRunner Suite'.
		self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
		self runSuiteProfiled: suite.
	]
! !

!TestRunner methodsFor: 'processing' stamp: 'gsa 5/2/2012 18:40'!
runSuiteProfiled: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	runSemaphore initSignals.
	running _ [
            [ result _ MessageTally spyOn: [suite run] ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				WorldState addDeferredUIMessage: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
	      priority: Processor userBackgroundPriority;
	      resume.
! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'gsa 5/2/2012 18:06'!
buildRunProfiledButton
	| runProfiledButton |
	runProfiledButton := PluggableButtonMorph 
				model: model
				stateGetter: #runButtonState
				action: #runProfiledTests
				label: 'Run Profiled'.
	runProfiledButton
		color: self runButtonColor.
	^runProfiledButton! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'gsa 5/2/2012 18:34'!
buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton row column1 column2 column3 theTestsList |

	refreshButton _ self buildRefreshButton.
	filterButton _ self buildFilterButton.
	stopButton _ self buildStopButton.
	column1 _ LayoutMorph newColumn.
	column1 addMorphs: { refreshButton . filterButton . stopButton }.

	theTestsList _ PluggableListMorphOfMany
				model: model
				listGetter: #tests
				primarySelectionGetter: #selectedSuite
				primarySelectionSetter: #selectedSuite:
				listSelectionGetter: #listSelectionAt:
				listSelectionSetter: #listSelectionAt:put:
				mainView: self
				menuGetter: #listMenu
				keystrokeAction: nil.
	theTestsList autoDeselect: false.
	theTestsList color: Color veryVeryLightGray.
	column2 _ LayoutMorph newColumn.
	column2
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight.

	runOneButton _ self buildRunOneButton.
	runButton _ self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	
	column3 _ LayoutMorph newColumn.
	column3 addMorphs: { runOneButton . runButton . runProfiledButton }.
	
	row _ LayoutMorph newRow.
	row
		addMorph: column1 fixedWidth: 80;
		addMorph: column2 proportionalWidth: 1;
		addMorph: column3 fixedWidth: 120.

	^row

! !

