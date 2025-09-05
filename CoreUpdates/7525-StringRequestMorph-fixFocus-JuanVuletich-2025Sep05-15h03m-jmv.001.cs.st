'From Cuis7.5 [latest update: #7523] on 5 September 2025 at 3:04:16 pm'!

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 9/5/2025 14:16:11'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	| morph |
	morph := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: morph centeredNear: aPoint - self deltaToTextPane.
	"(Preferences at: #focusFollowsMouse) ifFalse: [answer textBox focusText]."
	morph textBox focusText.
	^ morph! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 9/5/2025 14:15:59'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"Modal. This method returns after an answer is provided."

	| morph world |
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
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	^ morph getUserResponseOrCancel: cancelBlock! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 9/5/2025 14:16:24'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
	"Modal. This method returns when an answer is provided."

	| morph world |
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
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	^ morph getUserResponseOrCancel: cancelBlock! !

