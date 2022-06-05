'From Cuis 6.0 [latest update: #5215] on 2 June 2022 at 8:17:57 pm'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/2/2022 19:26:33'!
popUpAt: aPoint allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt hand world |
	self items isEmpty ifTrue: [^self].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	world _ self runningWorld.
	world ifNil: [ ^ self ].
	hand _ world activeHand.
	world addMorphFront: self position: aPoint - `2 @ 8`.
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


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 6/2/2022 17:30:58'!
invokeNonModalAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point."

	self
		popUpAt: aPoint
		allowKeyboard: aBoolean.! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 6/2/2022 17:29:50'!
startUpNonModalWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpNonModalWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (PreferenceNG at: #menuKeyboardControl)! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 6/2/2022 17:30:15'!
startUpNonModalWithCaption: captionOrNil at: location allowKeyboard: aBoolean 
	"Display the menu, with caption if supplied. Wait for the mouse button to go down, then track the selection as long as the button is pressed. When the button is released,
	Answer the index of the current selection, or zero if the mouse is not released over  any menu item. Location specifies the desired topLeft of the menu body rectangle. The final argument indicates whether the menu should seize the keyboard focus in order to allow the user to navigate it via the keyboard."

	| maxHeight |
	maxHeight := Display height * 3 // 4.
	self frameHeight > maxHeight 
		ifTrue: [
			^self 
				startUpSegmented: maxHeight
				withCaption: captionOrNil
				at: location
				allowKeyboard: aBoolean].
	^(MVCMenuMorph from: self title: captionOrNil) 
		invokeNonModalAt: location
		allowKeyboard: aBoolean! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 6/2/2022 20:15:44'!
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
			self delete.
			aKeyboardEvent hand
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
			^self].
	(asc = 28 or: [asc = 29]) 
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
				ifFalse: [^ self]].
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

!MenuMorph methodsFor: 'events' stamp: 'jmv 6/2/2022 16:56:31'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self includesPixel: aMouseButtonEvent eventPosition ]) 
		ifFalse: [
			self deleteIfPopUp: aMouseButtonEvent.
			aMouseButtonEvent hand
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
			^ self ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self.! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 6/2/2022 20:17:15'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self includesPixel: aMouseButtonEvent eventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand releaseMouseFocus: self.
		self deleteIfPopUp: aMouseButtonEvent.
		aMouseButtonEvent hand
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
		^ self].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ].! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 6/2/2022 19:27:37'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand delay |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self popUpAt: actHand morphPosition allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	delay _ Delay forMilliseconds: 10.
	[ self isInWorld and: [self isModalInvokationDone not] ] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 6/2/2022 20:14:17'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|w delay |
	w _ self runningWorld.
	w doOneMinimalCycleNow.
	self popUpAt: aPoint allowKeyboard: aBoolean.
	done _ false.
	delay _ Delay forMilliseconds: 20.
	[ done not and: [self isInWorld] ] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	^ mvcSelection ! !


!PopUpMenu class methodsFor: 'dialogs' stamp: 'jmv 6/2/2022 17:29:26'!
inform: aString
	"PopUpMenu inform: 'I like Cuis'"

	UISupervisor whenUIinSafeState: [
		(self labels: ' OK ' icons: #(emblemImportantIcon)) startUpNonModalWithCaption: aString ]! !

!methodRemoval: MenuMorph #popUpAt:forHand:allowKeyboard: stamp: 'jmv 6/2/2022 20:10:54'!
MenuMorph removeSelector: #popUpAt:forHand:allowKeyboard:!
