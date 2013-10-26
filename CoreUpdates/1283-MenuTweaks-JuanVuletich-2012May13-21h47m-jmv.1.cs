'From Cuis 4.0 of 21 April 2012 [latest update: #1282] on 13 May 2012 at 10:08:55 pm'!

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2012 22:07'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu title: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep.
	aMenu addLine.

	aMenu add: 'Rename change set (r)' 			action: #rename.
	aMenu add: 'Destroy change set (x)' 			action: #remove.
	aMenu addLine.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble.
				aMenu add: 'Remove preamble' 	action: #removePreamble]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 		action: #addPreamble].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'Edit postscript...' 		action: #editPostscript.
				aMenu add: 'Remove postscript' 	action: #removePostscript]
			ifFalse: [
				aMenu add: 'Add postscript...' 		action: #editPostscript].
	].
	aMenu addLine.

	"CONFLICTS SECTION"
	aMenu add: 'conflicts with other change sets' action: #browseMethodConflicts.
	aMenu balloonTextForLastItem: 'Browse all methods that occur both in this change set and in at least one other change set.'.
	aMenu addLine.

	"CHECKS SECTION"
	aMenu add: 'trim history' target: model action: #trimHistory.
	aMenu balloonTextForLastItem: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes. ',
				'NOTE: can cause confusion if later filed in over an earlier version of these changes'.

	aMenu add: 'view affected class categories' target: model action: #viewAffectedClassCategories.
	aMenu balloonTextForLastItem: ' Show class categories affected by any contained change'.

	^ aMenu! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 5/13/2012 22:00'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
	)! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 5/13/2012 22:00'!
slowMachine

	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating false)
		(subPixelRenderFonts false)
		(thoroughSenders false)
	).
	ClassicTheme beCurrent! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 5/13/2012 22:00'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak in recent years. Caution: this theme removes the standard Squeak flaps, turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more."

	self setPreferencesFrom:

	#(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating false)
		(thoroughSenders false)
	)! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/13/2012 22:03'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self colorForDebugging: menu.
	menu addStayUpIcons.
	self fillIn: menu
		from: {
				{ 'Open...'. { self. #openWindow}}.
				{ 'New morph...'. { self. #newMorph}.
					'Offers a variety of ways to create new objects'}.
				{ 'Appearance...'. { self. #appearanceDo}.
					'put up a menu offering many controls over appearance.'}.
				{ 'Windows...'. { self. #windowsDo}}.
				{ 'Help...'. { self. #helpDo}.
					'puts up a menu of useful items for updating the system, determining what version you are running, and much else'}.
				nil.
				{ 'Changes...'. { self. #changesDo}}.
				{ 'Debug...'. { self. #debugDo}.
					'a menu of debugging items'}.
				{ 'Do...'. { Utilities. #offerCommonRequestsInMorphic}.
					'put up an editible list of convenient expressions, and evaluate the one selected.'}.
				{ 'Restore Display (r)'. { World. #restoreMorphicDisplay}.
					'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'}.
				nil.
				{ 'Save'. { Smalltalk . #saveSession}.
					'save the current version of the image on disk'}.
				{ 'Save as...'. { Smalltalk . #saveAs}.
					'save the current version of the image on disk under a new name.'}.
				{ 'Save as New Version'. { Smalltalk . #saveAsNewVersion}.
					'give the current image a new version-stamped name and save it under that name on disk.'}.
				{ 'Save and Quit'. { self. #saveAndQuit}.
					'save the current image on disk, and quit out of Cuis.'}.
				{ 'Quit'. { self. #quitSession}.
					'quit out of Cuis.'}}.
	^menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/13/2012 21:51'!
openMenu
	"Build the open window menu for the world."

	| menu |
	menu _ self menu: 'Open...'.

	ExtraOpenCommands ifNotNil: [
		self fillIn: menu from: ExtraOpenCommands ].

	self fillIn: menu from: {
		{'Text Editor' . {self . #openTextEditor}. 'A window for composing text' }.
		{'Workspace' . {self . #openWorkspace}. 'A window for evaluating Smalltalk expressions' }.
		{'Browser' . { self . #openBrowser}. 'A Smalltalk code browser, for studying and modifying the system'}.
		{'Message Names' . { self . #openMessageNames} . 'A tool for finding and editing methods that contain any given keyword in their names.'}.
		{'Transcript' . {self . #openTranscript}. 'A window showing contents of the System Transcript' }.
		nil.
		{'Installed Packages' . { self . #openPackageList}. 'A tool for managing Packages (optional units of code) installed in the system'}.
		{'Change Sorter' . {self . #openChangeSorter1} . 'A tool allowing you to view the methods in a Change Set, especially changes to the Base System' }.
		nil.
		{ 'Process Browser' . { ProcessBrowserWindow . #openProcessBrowser } }.
		{ 'Emergency Evaluator'. { Transcripter. #emergencyEvaluator } }.
		nil.
		{'File List' . {self . #openFileList} . 'An explorer of the File System' }.
		{'SUnit Test Runner' . {TestRunnerWindow . #openTestRunner} . 'A tool allowing you to compare and manipulate two change sets concurrently' }.
	}.
	^menu! !

!methodRemoval: Preferences class #simpleMenus!
Preferences class removeSelector: #simpleMenus!
!methodRemoval: ChangeSorterWindow #offerShiftedChangeSetMenu!
ChangeSorterWindow removeSelector: #offerShiftedChangeSetMenu!
!methodRemoval: ChangeSorterWindow #offerUnshiftedChangeSetMenu!
ChangeSorterWindow removeSelector: #offerUnshiftedChangeSetMenu!
!methodRemoval: ChangeSorterWindow #shiftedChangeSetMenu!
ChangeSorterWindow removeSelector: #shiftedChangeSetMenu!
