'From Cuis 5.0 of 7 November 2016 [latest update: #3049] on 3 March 2017 at 9:27:56 am'!

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 3/2/2017 20:08:11'!
doOneMinimalCycleNow
	"see the comment in WorldState >> doOneMinimalCycleNow"

	worldState doOneMinimalCycleNow! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/2/2017 20:06:48'!
doOneMinimalCycleNow
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorldSafely.

	"Process user input events. Run all event triggered code."
	^activeHand processEventQueue! !


!MenuMorph methodsFor: 'modal control' stamp: 'jmv 3/2/2017 20:08:34'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self	
		popUpAt: actHand morphPosition
		forHand: actHand 
		allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	[ self isInWorld & self isModalInvokationDone not ] whileTrue: [ w doOneMinimalCycleNow ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 3/2/2017 20:08:29'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus |
	self flag: #bob.		"is <aPoint> global or local?"
	self flag: #arNote.	"<aPoint> is local to aWorld"
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	[ self isInWorld & done not ] whileTrue: [ w doOneMinimalCycleNow ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!FillInTheBlankMorph methodsFor: 'invoking' stamp: 'jmv 3/2/2017 20:08:24'!
getUserResponse
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."

	| w |
	w _ self world.
	w ifNil: [^ response].
	done _ false.
	textPane focusText.
	[done] whileFalse: [w doOneMinimalCycleNow].
	self delete.
	w doOneMinimalCycleNow.
	^ response
! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 3/3/2017 09:26:42'!
request: queryString initialAnswer: defaultAnswer centerAt: aPoint onCancelReturn: returnOnCancel acceptOnCR: acceptBoolean
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts.   If the user cancels, answer returnOnCancel."
	"
	FillInTheBlankMorph
		request: 'Type something, then type [Return].'
		initialAnswer: 'yo ho ho!!'
	"

	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		setQuery: queryString
		initialAnswer: defaultAnswer
		acceptOnCR: acceptBoolean.
	aFillInTheBlankMorph responseUponCancel: returnOnCancel.
	self runningWorld addMorph: aFillInTheBlankMorph centeredNear: aPoint.
	^ aFillInTheBlankMorph getUserResponse! !

