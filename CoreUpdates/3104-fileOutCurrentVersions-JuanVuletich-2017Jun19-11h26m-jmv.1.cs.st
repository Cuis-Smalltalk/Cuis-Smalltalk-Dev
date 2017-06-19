'From Cuis 5.0 of 7 November 2016 [latest update: #3103] on 19 June 2017 at 11:46:49 am'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/19/2017 11:45:52'!
fileOutCurrentVersionsOfSelections
	
	(FillInTheBlankMorph
		request: 'Enter file name'
		initialAnswer: 'Filename.st'
		onCancel: [^nil])

			asFileEntry writeStreamDo: [ :stream |
				stream timeStamp.
				self currentVersionsOfSelections do: [ :methodRef |
					methodRef actualClass
						printMethodChunk: methodRef methodSymbol
						withPreamble: true
						on: stream
						moveSource: false
						toFile: 0 ]]! !


!ChangeListWindow methodsFor: 'menu building' stamp: 'jmv 6/19/2017 11:39:03'!
listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'change list'.
	aMenu addStayUpIcons.
	aMenu addList: #(
	('fileIn selections'							fileInSelections
			'import the selected items into the image'																			model)
	('fileOut selections...	'					fileOutSelections
			'create a new file containing the selected items'																	model)
	('fileOut current version of selections...'	fileOutCurrentVersionsOfSelections
			'create a new file containing the current (in-image) counterparts of the selected methods'					model)
	-
	('compare to current'						compareToCurrentVersion
			'open a separate window which shows the text differences between the on-file version and the in-image version.' model)
	('toggle diffing (D)'							toggleDiffing							
			'start or stop showing diffs in the code pane.'																		model)
	-
	('select new methods'						selectNewMethods
			'select methods in the file that do not currently exist in the image'												model)
	('select changes for absent classes'		selectAllForAbsentClasses
			'select methods in the file for classes that are not defined in the image'										model)
	('select all changes for this class'			selectAllForThisClass
			'select all methods in the file that belong to the currently-selected class'										model)
	('select unchanged methods'				selectUnchangedMethods	
			'select methods in the file whose in-image versions are the same as their in-file counterparts'				model)
	('select methods equivalent to current'	selectEquivalentMethods
			'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'	model)
	('select methods older than current'		selectMethodsOlderThanCurrent
			'select methods in the file that are older than the one currently in the image'									model)
	('select removals of sent methods'		selectRemovalsOfSent
			'select all method removals of methods that have some sender in the image'									model)
	-
	('select all (a)'								selectAll
			'select all the items in the list'																							model)
	('deselect all'									deselectAll
			'deselect all the items in the list'																						model)
	('invert selections'							invertSelections
			'select every item that is not currently selected, and deselect every item that *is* currently selected'		model)
	-
	('browse class and method'				browseMethodFull
			'open a full browser showing the selected method')
	('browse all versions of single selection'	browseVersions
			'open a version browser showing the versions of the currently selected method')
	('browse current versions of selections'	browseCurrentVersionsOfSelections
			'open a message-list browser showing the current (in-image) counterparts of the selected methods')
	('destroy current methods of selections'	destroyCurrentCodeOfSelections
			'remove (*destroy*) the in-image counterparts of all selected methods'										model)
	-
	('remove doIts'								removeDoIts
			'remove all items that are doIts rather than definitions'																model)
	('remove older versions'					removeOlderMethodVersions
			'remove all but the most recent versions of methods in the list'													model)
	('remove up-to-date versions'				removeUpToDate
			'remove all items whose code is the same as the counterpart in-image code'									model)
	('remove empty class comments'			removeEmptyClassComments
			'remove all empty class comments'																					model)
	('remove selected items'					removeSelections
			'remove the selected items from the change-list'																	model)
	('remove unselected items'					removeNonSelections
			'remove all the items not currently selected from the change-list'												model)).
	^ aMenu! !

