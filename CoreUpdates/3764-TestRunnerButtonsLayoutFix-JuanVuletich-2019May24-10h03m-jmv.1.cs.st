'From Cuis 5.0 of 7 November 2016 [latest update: #3763] on 24 May 2019 at 10:05:57 am'!

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 5/24/2019 10:04:35'!
buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton row column1 column2 column3 theTestsList |

	refreshButton _ self buildRefreshButton.
	filterButton _ self buildFilterButton.
	stopButton _ self buildStopButton.
	column1 _ LayoutMorph newColumn.
	column1 doAdoptWidgetsColor.
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
	theTestsList color: self textBackgroundColor.
	column2 _ LayoutMorph newColumn.
	column2
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight.

	runOneButton _ self buildRunOneButton.
	runButton _ self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	
	column3 _ LayoutMorph newColumn.
	column3 doAdoptWidgetsColor.
	column3 addMorphs: { runOneButton . runButton . runProfiledButton }.
	
	row _ LayoutMorph newRow.
	row
		addMorph: column1 proportionalWidth: 0.1;
		addMorph: column2 proportionalWidth: 0.7;
		addMorph: column3 proportionalWidth: 0.2.

	^row

! !

