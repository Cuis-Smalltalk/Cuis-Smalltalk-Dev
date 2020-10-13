'From Cuis 5.0 [latest update: #4402] on 12 October 2020 at 9:31:21 pm'!

!Morph methodsFor: 'menus' stamp: 'jmv 10/12/2020 21:29:49'!
addHaloActionsTo: aMenu 
	"Add items to aMenu representing actions requestable via halo"

	| subMenu |
	subMenu := MenuMorph new defaultTarget: self.
	subMenu addTitle: (self printStringLimitedTo: 40).
	subMenu addStayUpIcons.
	subMenu addLine.
	(subMenu add: 'delete' action: #dismissViaHalo)
		setBalloonText: 'Delete this object -- warning -- can be destructive!!' .
	self maybeAddCollapseItemTo: subMenu.
	(subMenu add: 'grab' action: #openInHand)
		setBalloonText: 'Pick this object up -- warning, since this removes it from its container, it can have adverse effects.' .
	subMenu addLine.
	(subMenu add: 'duplicate' action: #maybeDuplicateMorph)
		setBalloonText: 'Hand me a copy of this object'.
	(subMenu
		add: 'set color'
		target: self
		action: #changeColor)
			setBalloonText: 'Change the color of this object'.
	(subMenu
		add: 'inspect'
		target: self
		action: #inspect)
			setBalloonText: 'Open an Inspector on this object'.
	aMenu add: 'halo actions...' subMenu: subMenu! !

!methodRemoval: Morph #resizeFromMenu stamp: 'jmv 10/12/2020 21:30:04'!
Morph removeSelector: #resizeFromMenu!
!methodRemoval: Morph #resizeMorph stamp: 'jmv 10/12/2020 21:30:26'!
Morph removeSelector: #resizeMorph!
!classRemoval: #HandleMorph stamp: 'jmv 10/12/2020 21:30:51'!
Smalltalk removeClassNamed: #HandleMorph!

