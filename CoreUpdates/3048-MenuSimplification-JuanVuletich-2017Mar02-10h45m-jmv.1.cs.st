'From Cuis 5.0 of 7 November 2016 [latest update: #3047] on 2 March 2017 at 10:50:58 am'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp popUpOwner activeSubMenu titleMorph oldKeyboardFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuMorph commentStamp: '<historical>' prior: 0!
Instance variables:
	defaultTarget 		<Object>					The default target for creating menu items
	selectedItem			<MenuItemMorph> 		The currently selected item in the receiver
	stayUp 				<Boolean>				True if the receiver should stay up after clicks!


!MenuMorph methodsFor: 'control' stamp: 'jmv 3/2/2017 10:47:30'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true."

	stayUp ifFalse: [ self delete ].
	evt ifNotNil: [ evt hand ifNotNil: [ :h | h releaseMouseFocus: self ]]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 3/2/2017 10:48:01'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
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

!MenuMorph methodsFor: 'control' stamp: 'jmv 3/2/2017 10:48:31'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 3/2/2017 10:48:26'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 3/2/2017 10:47:45'!
keyStroke: aKeyboardEvent 
	| matchString char asc selectable |
	char := aKeyboardEvent keyCharacter.
	asc := char numericValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
					selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: [
							"self delete."
							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			^self delete].
	(asc = 28 or: [asc = 29]) 
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := char = Character backspace 
				ifTrue: [
					matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent! !

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 3/2/2017 10:47:57'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	| eventPositionInOurCoordinates |
	eventPositionInOurCoordinates _ self internalizeFromWorld: aMorphicEvent eventPosition.

	self dispatchEvent: aMorphicEvent localPosition: eventPositionInOurCoordinates.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]]! !

!MenuMorph methodsFor: 'initialization' stamp: 'jmv 3/2/2017 10:47:34'!
initialize
	super initialize.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 3/2/2017 10:45:33'!
mouseEnter: evt
	"The mouse entered the receiver"
	owner ifNil: [ ^self ].
	owner selectItem: self! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 3/2/2017 10:36:12'!
deselect

	self isSelected: false.
	subMenu ifNotNil: [
		self removeAlarm: #deselectTimeOut ]! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 3/2/2017 10:36:17'!
select
	self isSelected: true.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphBoundsInWorld topRight + (10@0)
									with: self morphBoundsInWorld topLeft)
			from: self.
		subMenu selectItem: nil ]! !

!methodRemoval: MenuItemMorph #activateOwnerMenu:!
MenuItemMorph removeSelector: #activateOwnerMenu:!
!methodRemoval: MenuMorph #activeSubmenu:!
MenuMorph removeSelector: #activeSubmenu:!
!methodRemoval: MenuMorph #delete!
MenuMorph removeSelector: #delete!
!methodRemoval: MenuMorph #popUpOwner!
MenuMorph removeSelector: #popUpOwner!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
