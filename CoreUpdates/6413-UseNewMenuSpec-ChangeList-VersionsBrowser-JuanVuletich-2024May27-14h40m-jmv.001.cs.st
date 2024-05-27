'From Cuis7.1 [latest update: #6411] on 27 May 2024 at 2:43:19 pm'!

!ChangeListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/27/2024 11:52:37'!
listMenuSpec
	"Spec for the menu and shortcut keys the changes list.
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
	^#(
		#(0 		'Change List' 									true)
		#(10		'fileIn selections' 									nil 	model 			fileInSelections 										nil 		nil
					'import the selected items into the image')
		#(20		'fileOut selections...	' 								nil 	model 			fileOutSelections 										nil 		nil
		 			'create a new file containing the selected items')
		#(30		'fileOut current version of selections...' 					nil 					model 			fileOutCurrentVersionsOfSelections 										nil 		nil
 					'create a new file containing the current (in-image) counterparts of the selected methods')
		#(40 		'-----')
		#(50		'compare to current' 									nil 	model 			compareToCurrentVersion 										nil 		nil
 					'open a separate window which shows the text differences between the on-file version and the in-image version.')
		#(60		'toggle diffing (D)' 									$D 	model 			toggleDiffing 										nil 		nil
 					'start or stop showing diffs in the code pane.')
		#(70 		'-----')
		#(80		'select new methods' 									nil 	model 			selectNewMethods 										nil 		nil
 					'select methods in the file that do not currently exist in the image')
		#(90		'select changes for absent classes' 									nil 	model 			selectAllForAbsentClasses 										nil 		nil
					'select methods in the file for classes that are not defined in the image')
		#(100		'select all changes for this class' 									nil 	model 			selectAllForThisClass 										nil 		nil
					'select all methods in the file that belong to the currently-selected class')
		#(110		'select unchanged methods' 									nil 	model 			selectUnchangedMethods 										nil 		nil
					'select methods in the file whose in-image versions are the same as their in-file counterparts')
		#(120		'select methods equivalent to current' 									nil 	model 			selectEquivalentMethods 										nil 		nil
					'select methods in the file whose in-image versions have the same behavior as their in-file counterparts')
		#(130		'select methods older than current' 									nil 	model 			selectMethodsOlderThanCurrent 										nil 		nil
					'select methods in the file that are older than the one currently in the image')
		#(140		'select removals of sent methods' 									nil 	model 			selectRemovalsOfSent 										nil 		nil
					'select all method removals of methods that have some sender in the image')
		#(150 		'-----')
		#(160		'select all (a)' 									$a 	model 			selectAll 										nil 		nil
					'select all the items in the list')
		#(170		'deselect all (A)' 									$A 	model 			deselectAll 										nil 		nil
					'deselect all the items in the list')
		#(180		'invert selections' 									nil 	model 			invertSelections 										nil 		nil
					'select every item that is not currently selected, and deselect every item that *is* currently selected')
		#(190 		'-----')
		#(200		'browse class and method' 									nil 	window 			browseMethodFull 										nil 		nil
					'open a full browser showing the selected method')
		#(210		'browse all versions of single selection' 									nil 	window 			browseVersions 										nil 		nil
					'open a version browser showing the versions of the currently selected method')
		#(220		'browse current versions of selections' 									nil 	window 			browseCurrentVersionsOfSelections 										nil 		nil
					'open a message-list browser showing the current (in-image) counterparts of the selected methods')
		#(230		'destroy current methods of selections' 									nil 	model 			destroyCurrentCodeOfSelections 										nil 		nil
					'remove (*destroy*) the in-image counterparts of all selected methods')
		#(240 		'-----')
		#(250		'remove doIts' 									nil 	model 			removeDoIts 										nil 		nil
					'remove all items that are doIts rather than definitions')
		#(260		'remove older versions' 									nil 	model 			removeOlderMethodVersions 										nil 		nil
					'remove all but the most recent versions of methods in the list')
		#(270		'remove up-to-date versions' 									nil 	model 			removeUpToDate 										nil 		nil
					'remove all items whose code is the same as the counterpart in-image code')
		#(280		'remove empty class comments' 									nil 	model 			removeEmptyClassComments 										nil 		nil
					'remove all empty class comments')
		#(290		'remove selected items' 									nil 	model 			removeSelections 										nil 		nil
					'remove the selected items from the change-list')
		#(300		'remove unselected items' 									nil 	model 			removeNonSelections 										nil 		nil
					'remove all the items not currently selected from the change-list')
	)! !


!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/27/2024 11:53:55'!
classCommentVersionsMenuSpec
	"Spec for the menu and shortcut keys for a class comment version in the versions list.
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
	^#(
		#(0 		'Versions' 							true)
		#(10		'compare to current' 							nil 	model 			compareToCurrentVersion 										nil 		nil
					'compare selected version to the current version')
		#(20		'revert to selected version' 							nil 	model 			fileInSelections 										nil 		nil
					'resubmit the selected version, so that it becomes the current version')
		#(30 		'-----')
		#(40		'toggle diffing (D)' 							$D 	model 			toggleDiffing 										nil 		nil
					'toggle whether or not diffs should be shown here')
		#(50		'update list' 							nil 	model 			reformulateList 										nil 		nil
					'reformulate the list of versions, in case it somehow got out of synch with reality')
		#(60 		'-----')
		#(70		'help...' 							nil 	model 			offerVersionsHelp 										nil 		nil
					'provide an explanation of the use of this tool')
	)! !

!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/27/2024 11:01:52'!
listMenuSpec
	"Spec for the menu and shortcut keys the versions list."
	^model classCommentIndicated
		ifTrue: [ self classCommentVersionsMenuSpec ]
		ifFalse: [ self methodVersionsMenuSpec ]! !

!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/27/2024 11:54:09'!
methodVersionsMenuSpec
	"Spec for the menu and shortcut keys for a method version in the versions list.
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
				#(0 		'Versions' 									true)
	).
	model listIndex > 0 ifTrue: [
		(model list size > 1 ) ifTrue: [
			answer := answer, #(
				#(10		'compare to current' 									nil 	model 			compareToCurrentVersion 									nil 		nil
							'compare selected version to the current version')
				#(20		'compare to version...' 									nil 	model 			compareToOtherVersion 									nil 		nil
							'compare selected version to another selected version'))].
		answer := answer, #(
				#(30		'revert to selected version (z)' 									$z 	model 			fileInSelections 									nil 		nil
							'resubmit the selected version, so that it becomes the current version'))].
	answer := answer, #(
				#(40		'edit current method (O)' 									$O 	window 			openSingleMessageBrowser 									nil 		nil
							'open a single-message browser on the current version of this method')
				#(50 		'-----')
				#(60		'toggle diffing (D)' 									$D 	model 			toggleDiffing 									nil 		nil
							'toggle whether or not diffs should be shown here')
				#(70		'update list' 									nil 	model 			reformulateList 									nil 		nil
							'reformulate the list of versions, in case it somehow got out of synch with reality')
				#(80 		'-----')
				#(90		'senders (n)' 									$n 	window 			browseSenders 									nil 		nil
							'browse all senders of this selector')
				#(100		'implementors (m)' 									$m 	window 			browseImplementors 									nil 		nil
							'browse all implementors of this selector')
				#(110 		'-----')
				#(120		'help...' 									nil 	model 			offerVersionsHelp 									nil 		nil
							'provide an explanation of the use of this tool')
	).
	^answer! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 5/27/2024 11:01:18'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorphOfMany
		withModel: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:)
			menuNshortcutsKey: #listMenuSpec.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/27/2024 11:01:29'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorph
		withModel: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:)
			menuNshortcutsKey: #listMenuSpec.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !

!methodRemoval: VersionsBrowserWindow #classCommentVersionsMenu stamp: 'jmv 5/27/2024 14:42:18'!
VersionsBrowserWindow removeSelector: #classCommentVersionsMenu!
!methodRemoval: VersionsBrowserWindow #changeListKey:from: stamp: 'jmv 5/27/2024 14:42:18'!
VersionsBrowserWindow removeSelector: #changeListKey:from:!
!methodRemoval: VersionsBrowserWindow #listMenu stamp: 'jmv 5/27/2024 14:42:18'!
VersionsBrowserWindow removeSelector: #listMenu!
!methodRemoval: VersionsBrowserWindow #methodVersionsMenu stamp: 'jmv 5/27/2024 14:42:18'!
VersionsBrowserWindow removeSelector: #methodVersionsMenu!
!methodRemoval: ChangeListWindow #changeListKey:from: stamp: 'jmv 5/27/2024 14:42:18'!
ChangeListWindow removeSelector: #changeListKey:from:!
!methodRemoval: ChangeListWindow #listMenu stamp: 'jmv 5/27/2024 14:42:18'!
ChangeListWindow removeSelector: #listMenu!

!ChangeListWindow reorganize!
('GUI building' buildMorphicWindow createCodePaneMorph optionalButtonRow optionalModelButtonTuples windowColor)
('menu commands' browseCurrentVersionsOfSelections browseVersions)
('menu & kbd shortcuts' listMenuSpec)
!


!VersionsBrowserWindow reorganize!
('GUI building' buildMorphicWindow lineDiffButtonHelp optionalModelButtonTuples prettyLineDiffButtonHelp prettyWordDiffButtonHelp windowColor wordDiffButtonHelp)
('updating' updateListsAndCodeNow)
('menu & kbd shortcuts' classCommentVersionsMenuSpec listMenuSpec methodVersionsMenuSpec)
!

