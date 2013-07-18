'From Cuis 4.1 of 12 December 2012 [latest update: #1548] on 5 January 2013 at 10:54:03 pm'!

!MenuMorph methodsFor: 'construction' stamp: 'jmv 1/5/2013 22:46'!
add: aString action: aSymbol enabled: aBoolean
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	(self add: aString
		target: defaultTarget
		selector: aSymbol
		argumentList: #())
			isEnabled: aBoolean! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 1/5/2013 22:52'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	isForBaseSystem _ model changeSet isForBaseSystem.
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu title: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove 	enabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep.
	aMenu addLine.

	aMenu add: 'Rename change set (r)' 			action: #rename 					enabled: isForBaseSystem.
	aMenu add: 'Destroy change set (x)' 			action: #remove 					enabled: isForBaseSystem.
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
	aMenu add: 'conflicts with other change sets' target: self action: #browseMethodConflicts.
	aMenu balloonTextForLastItem: 'Browse all methods that occur both in this change set and in at least one other change set.'.
	aMenu addLine.

	"CHECKS SECTION"
	aMenu add: 'trim history' action: #trimHistory 								enabled: isForBaseSystem.
	aMenu balloonTextForLastItem: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes. ',
				'NOTE: can cause confusion if later filed in over an earlier version of these changes'.

	aMenu add: 'view affected class categories' action: #viewAffectedClassCategories.
	aMenu balloonTextForLastItem: ' Show class categories affected by any contained change'.

	^ aMenu! !

