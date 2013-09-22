'From Cuis 4.2 of 25 July 2013 [latest update: #1827] on 22 September 2013 at 11:25:58.168406 am'!

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 9/21/2013 20:06'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneCycle.
	self	
		popUpAt: actHand morphPosition
		forHand: actHand 
		allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	[ self isInWorld & self isModalInvokationDone not ] whileTrue: [ w doOneCycle ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 9/21/2013 20:05'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus |
	self flag: #bob.		"is <aPoint> global or local?"
	self flag: #arNote.	"<aPoint> is local to aWorld"
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneCycle.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	[ self isInWorld & done not ] whileTrue: [ w doOneCycle ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 9/21/2013 20:00'!
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	"See #eventTickler"
	Cursor currentCursor = Cursor wait ifTrue: [
		Cursor normal show ].

	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely: aWorld.

	"Run steps, alarms and deferred UI messages"
	aWorld runStepMethods.

	"Process user input events. Run all event triggered code."
	hadAnyEvent _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ hadAnyEvent | h processEventQueue.
		activeHand _ nil ].

	"The default is the primary hand"
	activeHand _ self hands first.


	^hadAnyEvent! !

!methodRemoval: WorldState #doOneSubCycleFor:!
WorldState removeSelector: #doOneSubCycleFor:!
!methodRemoval: WorldState #setDefaultActiveHand!
WorldState removeSelector: #setDefaultActiveHand!
!methodRemoval: PasteUpMorph #doOneCycleNow!
PasteUpMorph removeSelector: #doOneCycleNow!
!methodRemoval: PasteUpMorph #doOneSubCycle!
PasteUpMorph removeSelector: #doOneSubCycle!
