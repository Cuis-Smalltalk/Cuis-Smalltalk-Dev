'From Cuis7.3 [latest update: #7003] on 23 January 2025 at 5:15:27 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'eem 1/23/2025 17:08:14'!
selectContentsMatching
	| pattern |
	pattern := self request: 'pattern to match' initialAnswer: ''.
	pattern isEmpty ifTrue: [^self].
	^self selectSuchThat: ((pattern includesAnyOf: '?*')
							ifTrue: [[ :change | pattern match: change string]]
							ifFalse: [[ :change | change string includesSubString: pattern]])! !


!ChangeListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'eem 1/23/2025 17:14:42'!
listMenuSpec
	"Spec for the menu and shortcut keys for the changes list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Change List' 									true)
		#(10	'File In selections' 									nil 	model 	fileInSelections 						nil
					'import the selected items into the image')
		#(20	'File Out selections...	' 							nil 	model 	fileOutSelections 						nil
		 			'create a new file containing the selected items')
		#(30	'File Out current version of selections...' 			nil 	model 	fileOutCurrentVersionsOfSelections 	nil
 					'create a new file containing the current (in-image) counterparts of the selected methods')
		#(40 	'-----')
		#(50	'Compare to Current' 								nil 	model 	compareToCurrentVersion 				nil
 					'open a separate window which shows the text differences between the on-file version and the in-image version.')
		#(60 	'-----')
		#(70	'Select new methods' 								nil 	model 	selectNewMethods 					nil
 					'select methods in the file that do not currently exist in the image')
		#(80	'Select changes for Absent Classes' 				nil 	model 	selectAllForAbsentClasses 				nil
					'select methods in the file for classes that are not defined in the image')
		#(90	'Select all changes for this Class' 					nil 	model 	selectAllForThisClass 					nil
					'select all methods in the file that belong to the currently-selected class')
		#(100	'Select Unchanged methods' 						nil 	model 	selectUnchangedMethods 				nil
					'select methods in the file whose in-image versions are the same as their in-file counterparts')
		#(105	'Select changes with contents matching'			nil	model	selectContentsMatching				nil
					'select all changes in the file whose text matches a pattern or includes a string')

		#(110	'Select methods Equivalent to current' 				nil 	model 	selectEquivalentMethods 				nil
					'select methods in the file whose in-image versions have the same behavior as their in-file counterparts')
		#(120	'Select methods Older than current' 				nil 	model 	selectMethodsOlderThanCurrent 		nil
					'select methods in the file that are older than the one currently in the image')
		#(130	'Select removals of Sent methods' 					nil 	model 	selectRemovalsOfSent 					nil
					'select all method removals of methods that have some sender in the image')
		#(140 	'-----')
		#(150	'Select All (a)' 									$a 	model 	selectAll 								nil
					'select all the items in the list')
		#(160	'Deselect All (A)' 									$A 	model 	deselectAll 								nil
					'deselect all the items in the list')
		#(170	'Invert Selections' 								nil 	model 	invertSelections 						nil
					'select every item that is not currently selected, and deselect every item that *is* currently selected')
		#(180 	'-----')
		#(190	'Browse class and method' 						nil 	window browseMethodFull 					nil
					'open a full browser showing the selected method')
		#(200	'Browse all versions of single selection' 			nil 	window browseVersions 						nil
					'open a version browser showing the versions of the currently selected method')
		#(210	'Browse current versions of selections' 				nil 	window browseCurrentVersionsOfSelections 	nil
					'open a message-list browser showing the current (in-image) counterparts of the selected methods')
		#(220	'Destroy current methods of selections' 			nil 	model 	destroyCurrentCodeOfSelections 		nil
					'remove (*destroy*) the in-image counterparts of all selected methods')
		#(230 	'-----')
		#(240	'Remove doIts' 									nil 	model 	removeDoIts 							nil
					'remove all items that are doIts rather than definitions')
		#(250	'Remove older versions' 							nil 	model 	removeOlderMethodVersions 			nil
					'remove all but the most recent versions of methods in the list')
		#(260	'Remove up-to-date versions' 					nil 	model 	removeUpToDate 						nil
					'remove all items whose code is the same as the counterpart in-image code')
		#(270	'Remove empty class comments' 					nil 	model 	removeEmptyClassComments 			nil
					'remove all empty class comments')
		#(280	'Remove selected items' 							nil 	model 	removeSelections 						nil
					'remove the selected items from the change-list')
		#(290	'Remove unselected items' 						nil 	model 	removeNonSelections 					nil
					'remove all the items not currently selected from the change-list')
	)! !

