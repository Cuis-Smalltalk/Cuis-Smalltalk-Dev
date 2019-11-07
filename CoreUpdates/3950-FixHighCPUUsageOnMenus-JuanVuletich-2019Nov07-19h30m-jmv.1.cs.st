'From Cuis 5.0 [latest update: #3949] on 7 November 2019 at 7:30:54 pm'!

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 11/7/2019 19:16:35'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand delay |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self	
		popUpAt: actHand morphPosition
		forHand: actHand 
		allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	delay _ Delay forSeconds: 0.1.
	[ self isInWorld and: [self isModalInvokationDone not] ] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 11/7/2019 19:14:21'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus delay |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	delay _ Delay forSeconds: 0.1.
	[ done not and: [self isInWorld] ] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!FillInTheBlankMorph methodsFor: 'invoking' stamp: 'jmv 11/7/2019 19:15:41'!
getUserResponse
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."

	| w delay |
	w _ self world.
	w isNil ifTrue: [^ response].
	done _ false.
	textPane focusText.
	delay _ Delay forSeconds: 0.1.
	[done] whileFalse: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	^ response
! !

