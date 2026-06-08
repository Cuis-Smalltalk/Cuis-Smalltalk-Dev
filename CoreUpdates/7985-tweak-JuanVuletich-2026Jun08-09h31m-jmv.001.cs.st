'From Cuis7.9 [latest update: #7984] on 8 June 2026 at 9:32:15 am'!

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 8/Jun/2026 09:31:48'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"Modal. This method returns after an answer is provided."

	| morph world pkf response |
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
	pkf := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	pkf ifNotNil: [ world activeHand newKeyboardFocus: pkf ].
	^response! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 8/Jun/2026 09:31:56'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
	"Modal. This method returns when an answer is provided."

	| morph world pkf response |
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
	pkf := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	pkf ifNotNil: [ world activeHand newKeyboardFocus: pkf ].
	^response! !

