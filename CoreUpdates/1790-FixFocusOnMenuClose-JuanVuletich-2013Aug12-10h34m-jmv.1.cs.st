'From Cuis 4.2 of 25 July 2013 [latest update: #1789] on 12 August 2013 at 10:34:52.365899 am'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp popUpOwner activeSubMenu titleMorph oldKeyboardFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 8/12/2013 09:12'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [ self delete ].
	popUpOwner ifNotNil: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt ].
	evt ifNotNil: [ evt hand ifNotNil: [ :h | h releaseMouseFocus: self ]].

	oldKeyboardFocus ifNotNil: [.
		oldKeyboardFocus world ifNotNil: [ :w |
			w activeHand newKeyboardFocus: oldKeyboardFocus ] ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 8/12/2013 09:11'!
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
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 8/12/2013 10:34'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m isKindOf: UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self.
	self positionAt: aPoint forHand: hand.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp popUpOwner activeSubMenu titleMorph oldKeyboardFocus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
