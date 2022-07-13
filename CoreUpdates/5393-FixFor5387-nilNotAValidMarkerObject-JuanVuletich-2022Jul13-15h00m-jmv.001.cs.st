'From Cuis 6.0 [latest update: #5393] on 13 July 2022 at 3:01:25 pm'!

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 7/13/2022 15:01:00'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock

	| morph world |
	morph _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer.
	world _ self runningWorld.
	world ifNil: [
		| answer |
		answer _ self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer _ self request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	^ morph getUserResponseOrCancel: cancelBlock! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 7/13/2022 15:01:04'!
request: queryString initialAnswer: defaultAnswer onCancel: aBlock
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Evaluate aBlock if the user cancels."
	"
	FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'
		onCancel: [ 'plase play the game!!' ]
		"
	| w |
	w _ self runningWorld.
	w ifNil: [
		| answer |
		answer _ self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer _ self request: queryString initialAnswer: defaultAnswer onCancel: aBlock ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	^ (self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: w activeHand morphPosition
		onCancelReturn: nil
		acceptOnCR: true)
			ifNil: aBlock! !


!MenuMorph methodsFor: 'modal control' stamp: 'jmv 7/13/2022 15:00:56'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w actHand delay |
	w _ self runningWorld.
	w ifNil: [
		| answer |
		answer _ self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer _ self invokeModal: allowKeyboardControl ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
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


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 7/13/2022 15:00:52'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	| w delay |
	w _ self runningWorld.
	w ifNil: [
		| answer |
		answer _ self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer _ self invokeAt: aPoint allowKeyboard: aBoolean ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	w doOneMinimalCycleNow.
	self popUpAt: aPoint allowKeyboard: aBoolean.
	done _ false.
	delay _ Delay forMilliseconds: 20.
	[ done not and: [self isInWorld] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	^ mvcSelection ! !

