'From Cuis 4.0 of 21 April 2012 [latest update: #1490] on 30 November 2012 at 9:02:14 pm'!

!MenuMorph methodsFor: 'private' stamp: 'jmv 11/30/2012 20:45'!
positionAt: aPoint forHand: hand
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| delta |
	self adjustSubmorphsLayout.
	self morphPosition: aPoint - (2 @ 8).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self moveRight: aPoint x + 1 ].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight:
			((owner world morphExtentInWorld y - 18) max: (hand morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 11/30/2012 20:13'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [ self delete ].
	popUpOwner ifNotNil: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt ].
	evt ifNotNil: [ evt hand releaseMouseFocus: self ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 11/30/2012 20:57'!
popUpAdjacentTo: rightOrLeftPointInWorld forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 11/30/2012 20:57'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m isKindOf: UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self.
	self positionAt: aPoint forHand: hand.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 11/30/2012 20:57'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m isKindOf: UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self.
	self positionAt: aPoint forHand: hand.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 11/30/2012 20:56'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: (
			(each respondsTo: #minItemWidth)
				ifTrue: [each minItemWidth]
				ifFalse: [each morphWidth])].

	w _ w + 1.
	p _ 5 @ 5.
	submorphs do: [ :m |
		m morphWidth: w.
		m morphPosition: p.
		p _ p + (0@(m morphHeight + 1)) ].

	self morphExtent: w+4 @ p y + 5! !

!methodRemoval: MenuMorph #positionAt:forHand:relativeTo:!
MenuMorph removeSelector: #positionAt:forHand:relativeTo:!
