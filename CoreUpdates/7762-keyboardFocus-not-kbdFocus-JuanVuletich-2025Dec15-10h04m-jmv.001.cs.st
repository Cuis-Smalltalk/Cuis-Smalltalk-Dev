'From Cuis7.5 [latest update: #7758] on 15 December 2025 at 10:05:36 am'!

!ModalDialog class methodsFor: 'instance creation' stamp: 'jmv 15/Dec/2025 10:05:08'!
openModal: aModalDialog centeredNear: aWorldPosition
	"Open a Modal Dialog. Answer user's response (if OK) or nil (if Cancel)
	This is an abstract class. Better play with SampleModalDialog. Inspect the result of:
		SampleModalDialog openModal
	Note: a ModalDialog can also be opened in non-modal mode calling #open.
		No answer will be returned, so #ok and #cancel need to do any appropriate actions.
	"
	| world answer prevKeyboardFocus |
	world := self runningWorld ifNil: [UISupervisor ui].
	prevKeyboardFocus := world activeHand keyboardFocus.
	world addMorph: aModalDialog centeredNear: aWorldPosition.
	answer := aModalDialog waitForUserResponse.
	prevKeyboardFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKeyboardFocus ].
	^answer! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 15/Dec/2025 10:05:15'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"Modal. This method returns after an answer is provided."

	| morph world prevKeyboardFocus response |
	morph := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer.
	world := self runningWorld.
	world ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer := self request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	prevKeyboardFocus := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	prevKeyboardFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKeyboardFocus ].
	^response! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 15/Dec/2025 10:05:22'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
	"Modal. This method returns when an answer is provided."

	| morph world prevKeyboardFocus response |
	morph := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock.
	world := self runningWorld.
	world ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [
			answer := self request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	prevKeyboardFocus := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	prevKeyboardFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKeyboardFocus ].
	^response! !

