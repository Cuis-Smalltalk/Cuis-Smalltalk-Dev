'From Cuis 6.0 [latest update: #5386] on 11 July 2022 at 6:53:16 pm'!

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 7/11/2022 18:43:02'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock

	| morph world |
	morph _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer.
	world _ self runningWorld.
	world ifNil: [
		| answer |
		answer _ nil.
		UISupervisor whenUIinSafeState: [ answer _ self request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock ].
		 [ answer isNil ] whileTrue: [ Processor yield ].
		^answer ].
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	^ morph getUserResponseOrCancel: cancelBlock! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 7/11/2022 18:47:24'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"
	FillInTheBlankMorph request: 'What is your favorite color?'
	"

	^ self request: queryString initialAnswer: '' onCancel: nil! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 7/11/2022 18:46:55'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"
	FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'
		"

	^ self request: queryString initialAnswer: defaultAnswer onCancel: nil! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 7/11/2022 18:48:56'!
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
		answer _ nil.
		UISupervisor whenUIinSafeState: [ answer _ self request: queryString initialAnswer: defaultAnswer onCancel: aBlock ].
		 [ answer isNil ] whileTrue: [ Processor yield ].
		^answer ].
	^ (self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: w activeHand morphPosition
		onCancelReturn: nil
		acceptOnCR: true)
			ifNil: aBlock! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 7/11/2022 18:46:31'!
request: queryString onCancel: aBlock
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Evaluate aBlock if the user cancels."
	"
	FillInTheBlankMorph 
		request: 'What is your favorite color?'
		onCancel: [ 'plase play the game!!' ]
	"

	^ self request: queryString initialAnswer: '' onCancel: aBlock! !


!MenuMorph methodsFor: 'modal control' stamp: 'jmv 7/11/2022 18:51:27'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w actHand delay |
	w _ self runningWorld.
	w ifNil: [
		| answer |
		answer _ nil.
		UISupervisor whenUIinSafeState: [ answer _ self invokeModal: allowKeyboardControl ].
		 [ answer isNil ] whileTrue: [ Processor yield ].
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


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 7/11/2022 18:38:29'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	| w delay |
	w _ self runningWorld.
	w ifNil: [
		| answer |
		answer _ nil.
		UISupervisor whenUIinSafeState: [ answer _ self invokeAt: aPoint allowKeyboard: aBoolean ].
		 [ answer isNil ] whileTrue: [ Processor yield ].
		^answer ].
	w doOneMinimalCycleNow.
	self popUpAt: aPoint allowKeyboard: aBoolean.
	done _ false.
	delay _ Delay forMilliseconds: 20.
	[ done not and: [self isInWorld] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	^ mvcSelection ! !

