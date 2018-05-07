'From Cuis 5.0 of 7 November 2016 [latest update: #3300] on 1 May 2018 at 10:51:01 am'!

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 5/1/2018 10:35:14'!
changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	isForBaseSystem _ model changeSet isForBaseSystem.
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu title: 'Change Set'.
	aMenu addStayUpIcons.

	(aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove) 	isEnabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep.
	aMenu addLine.

	(aMenu add: 'Rename change set (r)' 			action: #rename) 					isEnabled: isForBaseSystem.
	(aMenu add: 'Destroy change set (x)' 			action: #remove) 					isEnabled: isForBaseSystem.
	aMenu addLine.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble.
				aMenu add: 'Remove preamble' 	action: #removePreamble]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 	action: #addPreamble].
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
	(aMenu add: 'trim history' action: #trimHistory) 								isEnabled: isForBaseSystem.
	aMenu balloonTextForLastItem: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes. ',
				'NOTE: can cause confusion if later filed in over an earlier version of these changes'.

	aMenu add: 'view affected class categories' action: #viewAffectedClassCategories.
	aMenu balloonTextForLastItem: ' Show class categories affected by any contained change'.

	^ aMenu! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:43:21'!
add: aString action: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^self add: aString
		target: defaultTarget
		selector: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:44:00'!
add: aString selector: aSymbol argument: arg

	^self add: aString
		target: defaultTarget
		selector: aSymbol
		argumentList: (Array with: arg)
! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:44:36'!
add: aString subMenu: aMenuMorph
	"Append the given submenu with the given label."

	| item |
	item _ MenuItemMorph new.
	item
		contents: aString;
		subMenu: aMenuMorph.
	self addMorphBack: item.
	^item! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:44:10'!
add: aString target: aTarget action: aSymbol
	^self add: aString
		target: aTarget
		selector: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:44:15'!
add: aString target: anObject selector: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	^self add: aString
		target: anObject
		selector: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:48:29'!
add: aString target: target selector: aSymbol argument: arg
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	^self add: aString
		target: target
		selector: aSymbol
		argumentList: { arg }! !


!CustomMenu methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:47:21'!
addList: listOfTuplesAndDashes
	"Add a menu item to the receiver for each tuple in the given list of the form (<what to show> <selector>). Add a line for each dash (-) in the list."

	listOfTuplesAndDashes do: [:aTuple |
		aTuple == #-
			ifTrue: [self addLine]
			ifFalse: [self add: aTuple first action: aTuple second]]

	"
	CustomMenu new addList: #(
		('apples' buyApples)
		('oranges' buyOranges)
		-
		('milk' buyMilk)); startUpMenu
	"

! !

!methodRemoval: CustomMenu #add:target:selector:!
CustomMenu removeSelector: #add:target:selector:!
!methodRemoval: CustomMenu #add:target:selector:argument:!
CustomMenu removeSelector: #add:target:selector:argument:!
!methodRemoval: CustomMenu #add:target:selector:argumentList:!
CustomMenu removeSelector: #add:target:selector:argumentList:!
!methodRemoval: CustomMenu #addStayUpIcons!
CustomMenu removeSelector: #addStayUpIcons!
!methodRemoval: MenuMorph #add:action:enabled:!
MenuMorph removeSelector: #add:action:enabled:!
!methodRemoval: MenuMorph #add:target:selector:arguments:!
MenuMorph removeSelector: #add:target:selector:arguments:!
