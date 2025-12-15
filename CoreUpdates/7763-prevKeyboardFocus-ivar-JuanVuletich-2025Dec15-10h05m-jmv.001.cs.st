'From Cuis7.5 [latest update: #7758] on 15 December 2025 at 10:05:54 am'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedBoxMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner prevKbdFocus prevMouseFocus prevKeyboardFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 15/Dec/2025 10:05:51'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu.
	Answer wether some action was actually taken."

	| h |
	h := evt ifNotNil: [ evt hand ].
	h ifNil: [
		self world ifNotNil: [ :w | h := h activeHand ]].
	h ifNil: [ h := self activeHand ].
	h ifNotNil: [
		h keyboardFocus == self ifTrue: [
			h newKeyboardFocus: prevKeyboardFocus ].
		h mouseFocus == self ifTrue: [
			"h newMouseFocus: prevMouseFocus."
			h releaseMouseFocus ]].
	stayUp ifFalse: [
		self delete.
		popUpOwner ifNotNil: [
			popUpOwner isSelected: false.
			popUpOwner deleteIfPopUp: evt ].
		^true ].
	^false! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 15/Dec/2025 10:05:51'!
popUpAt: aPoint allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt hand world |
	self items isEmpty ifTrue: [^self].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	world := self runningWorld.
	world ifNil: [ ^ self ].
	hand := world activeHand.
	world addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInOwner.
	"Acquire focus for valid pop up behavior"
	prevKeyboardFocus := hand keyboardFocus.
	prevMouseFocus := hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt := hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 15/Dec/2025 10:05:51'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInOwner.
	"Acquire focus for valid pop up behavior"
	prevKeyboardFocus := hand keyboardFocus.
	prevMouseFocus := hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 15/Dec/2025 10:05:51'!
keyStroke: aKeyboardEvent
	| matchString selectable |
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
				selectedItem hasSubMenu 
					ifTrue: [
						aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
						aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
					ifFalse: [
						selectedItem invokeWithEvent: aKeyboardEvent].
				aKeyboardEvent wasHandled: true.
				^self ].
			(selectable := self items) size = 1 
				ifTrue: [
					selectable first invokeWithEvent: aKeyboardEvent ].
			"Consume the event in any case"
			aKeyboardEvent wasHandled: true.
			^self].
	aKeyboardEvent isEsc
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
							self displayFiltered: aKeyboardEvent.
							aKeyboardEvent wasHandled: true.
							^self]].
			"If a stand-alone menu, just delete it"
			self delete.
			aKeyboardEvent hand
				newKeyboardFocus: prevKeyboardFocus;
				newMouseFocus: prevMouseFocus.
			aKeyboardEvent wasHandled: true.
			^self].
	aKeyboardEvent isArrowLeft | aKeyboardEvent isArrowRight
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu ].
			"Consume the event in any case"
			aKeyboardEvent wasHandled: true.
			^self].
	aKeyboardEvent isArrowUp ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	aKeyboardEvent isArrowDown ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	aKeyboardEvent isPageUp ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	aKeyboardEvent isPageDown ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := aKeyboardEvent isBackspace
				ifTrue: [matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent.
	aKeyboardEvent wasHandled: true.! !

!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedBoxMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner prevKeyboardFocus prevMouseFocus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuMorph reorganize!
('accessing' activeSubMenu addBlankIconsIfNecessary defaultLayerNumber items itemsDo: label lastItem stayUp stayUp:)
('construction' add:action: add:action:argument: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemFromDictionary: addItemFromDictionary:targeting: addItemFromDictionaryOrNil: addItemsFromDictionaries: addLabel: addLine addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: delete deleteIfPopUp: popUpAdjacentTo:from: popUpAt:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing')
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesMouseDown:)
('events-processing' handleMouseFocusEvent:)
('geometry' fontPreferenceChanged)
('initialization' defaultBorderWidth defaultColor initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: keyboardUnfocused moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('testing' is: isIncludedInTaskbar)
('private' adjustSubmorphsLayout selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
('halos and balloon help' addHalo:)
!

