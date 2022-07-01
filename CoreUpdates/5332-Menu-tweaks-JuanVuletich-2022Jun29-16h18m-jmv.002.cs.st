'From Cuis 6.0 [latest update: #5331] on 29 June 2022 at 4:36:26 pm'!

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 6/29/2022 16:18:04'!
invokeWithEvent: evt
	"Perform the action associated with the given menu item."

	| selArgCount |
	self isEnabled ifFalse: [^ self].
	owner ifNotNil: [
		owner deleteIfPopUp: evt ].
	selector ifNil: [ ^self ].
	(selArgCount _ selector numArgs) = 0
		ifTrue: [
			target perform: selector]
		ifFalse: [
			selArgCount = arguments size
				ifTrue: [target perform: selector withArguments: arguments]
				ifFalse: [target perform: selector withArguments: (arguments copyWith: evt)]]! !


!FillInTheBlankMorph methodsFor: 'invoking' stamp: 'jmv 6/29/2022 16:27:46'!
getUserResponse
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."

	| w delay |
	w _ self world.
	w isNil ifTrue: [^ response].
	done _ false.
	textPane focusText.
	delay _ Delay forMilliseconds: 10.
	[done] whileFalse: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	^ response! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 6/29/2022 16:34:17'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	| h |
	h _ evt ifNotNil: [ evt hand ].
	h ifNil: [
		self world ifNotNil: [ :w | h _ h activeHand ]].
	h ifNil: [ h _ self activeHand ].
	stayUp ifFalse: [ self delete ].
	h ifNotNil: [
		h newKeyboardFocus: prevKbdFocus.
		"h newMouseFocus: prevMouseFocus."
		h newMouseFocus: nil.
		].
	popUpOwner ifNotNil: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt ].! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 6/29/2022 16:29:24'!
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

!MenuMorph methodsFor: 'events' stamp: 'jmv 6/29/2022 16:23:13'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self includesPixel: aMouseButtonEvent eventPosition ]) 
		ifFalse: [
			self deleteIfPopUp: aMouseButtonEvent.
			^ self ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self.! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 6/29/2022 16:23:02'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self includesPixel: aMouseButtonEvent eventPosition) ifFalse: [
		"Mouse up outside. Release eventual focus and delete if pop up."
		self deleteIfPopUp: aMouseButtonEvent.
		^ self].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ].! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 6/29/2022 16:10:28'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w actHand delay |
	w _ self runningWorld.
	actHand _ w activeHand.
	w doOneMinimalCycleNow.
	self popUpAt: actHand morphPosition allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	delay _ Delay forMilliseconds: 10.
	[ self isInWorld and: [self isModalInvokationDone not] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	self delete.
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 6/29/2022 16:08:37'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	| w delay |
	w _ self runningWorld.
	w doOneMinimalCycleNow.
	self popUpAt: aPoint allowKeyboard: aBoolean.
	done _ false.
	delay _ Delay forMilliseconds: 20.
	[ done not and: [self isInWorld] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	^ mvcSelection ! !

