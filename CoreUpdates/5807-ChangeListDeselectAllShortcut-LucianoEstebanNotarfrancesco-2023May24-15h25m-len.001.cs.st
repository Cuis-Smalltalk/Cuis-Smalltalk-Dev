'From Cuis 6.0 [latest update: #5802] on 24 May 2023 at 3:26:36 pm'!

!ChangeListWindow methodsFor: 'menu building' stamp: 'len 5/23/2023 15:57:13'!
listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Change List'.
	aMenu addStayUpIcons.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileIn selections'.
				#object 			-> 		#model.
				#selector 		-> 		#fileInSelections.
				#balloonText 	-> 		'import the selected items into the image'.
			} asDictionary.
			{
				#label 			-> 		'fileOut selections...	'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSelections.
				#balloonText 	-> 		'create a new file containing the selected items'.
			} asDictionary.
			{
				#label 			-> 		'fileOut current version of selections...'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutCurrentVersionsOfSelections.
				#balloonText 	-> 		'create a new file containing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'compare to current'.
				#object 			-> 		#model.
				#selector 		-> 		#compareToCurrentVersion.
				#balloonText 	-> 		'open a separate window which shows the text differences between the on-file version and the in-image version.'.
			} asDictionary.
			{
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#balloonText 	-> 		'start or stop showing diffs in the code pane.'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select new methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectNewMethods.
				#balloonText 	-> 		'select methods in the file that do not currently exist in the image'.
			} asDictionary.
			{
				#label 			-> 		'select changes for absent classes'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForAbsentClasses.
				#balloonText 	-> 		'select methods in the file for classes that are not defined in the image'.
			} asDictionary.
			{
				#label 			-> 		'select all changes for this class'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForThisClass.
				#balloonText 	-> 		'select all methods in the file that belong to the currently-selected class'.
			} asDictionary.
			{
				#label 			-> 		'select unchanged methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectUnchangedMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions are the same as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods equivalent to current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectEquivalentMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods older than current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectMethodsOlderThanCurrent.
				#balloonText 	-> 		'select methods in the file that are older than the one currently in the image'.
			} asDictionary.
			{
				#label 			-> 		'select removals of sent methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectRemovalsOfSent.
				#balloonText 	-> 		'select all method removals of methods that have some sender in the image'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select all (a)'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAll.
				#balloonText 	-> 		'select all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'deselect all (A)'.
				#object 			-> 		#model.
				#selector 		-> 		#deselectAll.
				#balloonText 	-> 		'deselect all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'invert selections'.
				#object 			-> 		#model.
				#selector 		-> 		#invertSelections.
				#balloonText 	-> 		'select every item that is not currently selected, and deselect every item that *is* currently selected'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse class and method'.
				#selector 		-> 		#browseMethodFull.
				#balloonText 	-> 		'open a full browser showing the selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse all versions of single selection'.
				#selector 		-> 		#browseVersions.
				#balloonText 	-> 		'open a version browser showing the versions of the currently selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse current versions of selections'.
				#selector 		-> 		#browseCurrentVersionsOfSelections.
				#balloonText 	-> 		'open a message-list browser showing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			{
				#label 			-> 		'destroy current methods of selections'.
				#object 			-> 		#model.
				#selector 		-> 		#destroyCurrentCodeOfSelections.
				#balloonText 	-> 		'remove (*destroy*) the in-image counterparts of all selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove doIts'.
				#object 			-> 		#model.
				#selector 		-> 		#removeDoIts.
				#balloonText 	-> 		'remove all items that are doIts rather than definitions'.
			} asDictionary.
			{
				#label 			-> 		'remove older versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeOlderMethodVersions.
				#balloonText 	-> 		'remove all but the most recent versions of methods in the list'.
			} asDictionary.
			{
				#label 			-> 		'remove up-to-date versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeUpToDate.
				#balloonText 	-> 		'remove all items whose code is the same as the counterpart in-image code'.
			} asDictionary.
			{
				#label 			-> 		'remove empty class comments'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyClassComments.
				#balloonText 	-> 		'remove all empty class comments'.
			} asDictionary.
			{
				#label 			-> 		'remove selected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSelections.
				#balloonText 	-> 		'remove the selected items from the change-list'.
			} asDictionary.
			{
				#label 			-> 		'remove unselected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeNonSelections.
				#balloonText 	-> 		'remove all the items not currently selected from the change-list'.
			} asDictionary.
		}`.
	^ aMenu! !

!ChangeListWindow methodsFor: 'keyboard shortcuts' stamp: 'len 5/23/2023 15:57:41'!
changeListKey: aChar from: view
	"Respond to a Command key in the list pane."

	aChar = $D ifTrue: [^ model toggleDiffing].
	aChar = $a ifTrue: [^ model selectAll].
	aChar = $A ifTrue: [^ model deselectAll]! !

