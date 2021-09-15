'From Cuis 5.0 [latest update: #4636] on 19 June 2021 at 11:08:05 pm'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
WidgetMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner prevMouseFocus prevKbdFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuMorph methodsFor: 'control' stamp: 'hlsf 6/19/2021 22:50:33'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	prevKbdFocus _ hand keyboardFocus.
	prevMouseFocus _ hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'hlsf 6/19/2021 22:50:09'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	prevKbdFocus _ hand keyboardFocus.
	prevMouseFocus _ hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'events' stamp: 'hlsf 6/19/2021 23:05:11'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsGlobalPoint: aMouseButtonEvent eventPosition ]) 
		ifFalse: [
			self deleteIfPopUp: aMouseButtonEvent.
			self activeHand 
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
			^ self ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self! !

!MenuMorph methodsFor: 'events' stamp: 'hlsf 6/19/2021 23:05:34'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsGlobalPoint: aMouseButtonEvent eventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
		self deleteIfPopUp: aMouseButtonEvent.
		self activeHand 
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
		^ self].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]! !

!MenuMorph methodsFor: 'initialization' stamp: 'hlsf 6/19/2021 23:01:58'!
delete
	activeSubMenu ifNotNil: [activeSubMenu delete].
	self itemsDo: [ :i | i deselect ].
	super delete.! !

!classDefinition: #MenuMorph category: #'Morphic-Menus'!
WidgetMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner prevKbdFocus prevMouseFocus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
