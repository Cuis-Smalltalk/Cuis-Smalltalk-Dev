'From Cuis7.7 [latest update: #7875] on 13 April 2026 at 11:56:27 am'!

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 13/Apr/2026 11:47:13'!
doHaltOnAction
	self setProperty: #haltOnAction toValue: true
! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 13/Apr/2026 11:48:43'!
doesModalInvokation
	^(self valueOfProperty: #isModalInvokationDone ifAbsent:[^false]) notNil! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 13/Apr/2026 11:46:37'!
mustHaltOnAction
	^self valueOfProperty: #haltOnAction ifAbsent:[false]! !


!MVCMenuMorph methodsFor: 'as yet unclassified' stamp: 'jmv 13/Apr/2026 11:48:45'!
doesModalInvokation
	^true! !


!MenuMorph methodsFor: 'modal control' stamp: 'jmv 13/Apr/2026 11:51:36'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w actHand delay |
	w := self runningWorld.
	w ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer := self invokeModal: allowKeyboardControl ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	actHand := w activeHand.
	w doOneMinimalCycleNow.
	self popUpAt: actHand morphPosition allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	delay := Delay forMilliseconds: 10.
	[ self isInWorld and: [self isModalInvokationDone not] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	self delete.
	self mustHaltOnAction ifTrue: [ self halt: 'About to answer selection' ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 13/Apr/2026 11:55:46'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	| w delay |
	w := self runningWorld.
	w ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer := self invokeAt: aPoint allowKeyboard: aBoolean ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	w doOneMinimalCycleNow.
	self popUpAt: aPoint allowKeyboard: aBoolean.
	done := false.
	delay := Delay forMilliseconds: 20.
	[ done not and: [self isInWorld] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	self delete.
	self mustHaltOnAction ifTrue: [ self halt: 'About to answer selection' ].
	^ mvcSelection ! !


!MenuItemMorph methodsFor: 'debug and other' stamp: 'jmv 13/Apr/2026 11:51:08'!
debugAction
	
	(self isEnabled or: [ PopUpMenu confirm: 'The menu item is disabled, do you want to continue?' ])
		ifFalse: [ ^ self ].

	owner doesModalInvokation 
		ifTrue: [
			"Flag owner to halt when action actually taken."
			owner doHaltOnAction ]
		ifFalse: [
			"Action will happen right now. Do halt."
			self halt: 'About to execute action'].
	self invokeWithEvent: nil.
	^self! !

