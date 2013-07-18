'From Cuis 4.1 of 12 December 2012 [latest update: #1563] on 19 January 2013 at 3:18:17 pm'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 15:17'!
enableDragNDrop: aBoolean
	"Set both properties at once"
	self enableDrag: aBoolean.
	self enableDrop: aBoolean.! !

!Morph methodsFor: 'menus' stamp: 'jmv 1/19/2013 15:14'!
addToggleItemsToHaloMenu: aMenu
	"Add standard true/false-checkbox items to the memu"

	#(
	(stickinessString toggleStickiness 'whether I should be resistant to a drag done by mousing down on me')
	(lockedString lockUnlockMorph 'when "locked", I am inert to all user interactions')
	(hasClipSubmorphsString changeClipSubmorphs 'whether the parts of objects within me that are outside my bounds should be masked.')
	) do: [ :trip | 
			aMenu addUpdating: trip first action: trip second.
			aMenu balloonTextForLastItem: trip third ]! !


!PasteUpMorph methodsFor: 'menu & halo' stamp: 'jmv 1/19/2013 15:14'!
addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu, given that the receiver is a World"

	| unlockables |
	self addColorMenuItems: aMenu hand: aHandMorph.

"	aMenu addLine.
	self addWorldToggleItemsToHaloMenu: aMenu."
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.

	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables _ self submorphs select:
		[ :m | m isLocked].
	unlockables size = 1 ifTrue: [
		aMenu add: ('unlock "{1}"' format:{unlockables first printStringLimitedTo: 40})action: #unlockContents].
	unlockables size > 1 ifTrue: [
		aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.
! !

!methodRemoval: PasteUpMorph #addWorldToggleItemsToHaloMenu:!
PasteUpMorph removeSelector: #addWorldToggleItemsToHaloMenu:!
!methodRemoval: Morph #changeDragAndDrop!
Morph removeSelector: #changeDragAndDrop!
!methodRemoval: Morph #dragNDropEnabled!
Morph removeSelector: #dragNDropEnabled!
!methodRemoval: Morph #dropHighlightColor!
Morph removeSelector: #dropHighlightColor!
!methodRemoval: Morph #hasDragAndDropEnabledString!
Morph removeSelector: #hasDragAndDropEnabledString!
!methodRemoval: Morph #separateDragAndDrop!
Morph removeSelector: #separateDragAndDrop!
