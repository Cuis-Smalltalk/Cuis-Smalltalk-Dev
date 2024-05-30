'From Cuis7.1 [latest update: #6418] on 29 May 2024 at 4:15:20 pm'!

!TestRunnerWindow methodsFor: 'menu commands' stamp: 'jmv 5/29/2024 16:09:21'!
browseSelected
	| cls |
	cls := (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
	cls := cls asSymbol.
	cls := (Smalltalk at: cls ifAbsent: nil).
	cls ifNotNil: [
		BrowserWindow fullOnClass: cls selector: nil ]! !

!TestRunnerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/29/2024 16:13:10'!
listMenuSpec
	"Spec for the menu and shortcut keys for the list of test classes.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	| answer |
	answer := #(
		#(0 		'Test Cases Set' 						false)
		#(10		'select all (a)' 					$a 	model 			selectAll 					nil 	selectAllIcon)
		#(20		'deselect all (A)' 					$A 	model 			deselectAll 					nil 	selectAllIcon)
		#(30		'invert selections' 					nil 	model 			invertSelections 					nil 	switchIcon)
		#(40		'filter (f)' 					$f 	model 			setFilter 					nil 	findIcon)
		#(50		'refresh (u)' 					$u 	model 			refreshTests 					nil 	refreshIcon)
		#(60 		'-----')).
	model isRunning ifTrue: [ answer := answer, #(
		#(70		'stop (l)' 					$l 	model 			interruptRun 					nil 	stopIcon))]
	ifFalse: [ answer := answer, #(
		#(70		'run (r)' 					$r 	model 			runTests 					nil 	playIcon)
		#(80		'run one (o)' 					$o 	model 			runOneTest 					nil 	playIcon)
		#(90		'run profiled (p)' 					$p 	model 			runProfiledTests 					nil 	playIcon))].
	model selectedSuite > 0 ifTrue: [ answer := answer, #(
		#(100 		'-----')
		#(110		'browse' 					nil 	window 			browseSelected 					nil 	editFindReplaceIcon))].
	answer := answer, #(
		#(100 		'-----')
		#(110		'log to Transcript' 					nil 	model 			showResult 					nil 	printerIcon)).
	^answer! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 5/29/2024 16:12:07'!
buildUpperControls
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

!methodRemoval: TestRunnerWindow #listMenu stamp: 'jmv 5/29/2024 16:14:14'!
TestRunnerWindow removeSelector: #listMenu!
!methodRemoval: TestRunnerWindow #listKey:from: stamp: 'jmv 5/29/2024 16:13:49'!
TestRunnerWindow removeSelector: #listKey:from:!
!methodRemoval: TestRunnerWindow #browse: stamp: 'jmv 5/29/2024 16:14:19'!
TestRunnerWindow removeSelector: #browse:!

!TestRunnerWindow reorganize!
('GUI building' buildDetailsText buildErrorsList buildFailuresList buildFilterButton buildLowerPanes buildMorphicWindow buildPassFailText buildRefreshButton buildRunButton buildRunOneButton buildRunProfiledButton buildStopButton buildUpperControls windowColor)
('constants' runButtonColor)
('updating' isNotRunning refreshTests refreshWindow resetProgress update: updateColors updatePartColors:)
('menu commands' browseSelected)
('menu & kbd shortcuts' listMenuSpec)
!

