'From Cuis7.1 [latest update: #6869] on 28 November 2024 at 4:29:21 pm'!

!Object methodsFor: 'user interface' stamp: 'jmv 11/28/2024 11:43:36'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock
	"Modal. This method returns when an answer is provided."

	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer verifying: validationBlock! !

!Object methodsFor: 'user interface' stamp: 'jmv 11/28/2024 11:39:58'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
	"Modal. This method returns when an answer is provided."

	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 11/28/2024 11:42:50'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock
	"Modal. This method returns when an answer is provided."

	^self request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: []! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 11/28/2024 11:38:48'!
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
	^ morph getUserResponseOrCancel: cancelBlock! !


!WorldMorph methodsFor: 'ui services' stamp: 'jmv 11/28/2024 11:46:28'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock
	"Modal. This method returns when an answer is provided."

	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock! !

!WorldMorph methodsFor: 'ui services' stamp: 'jmv 11/28/2024 11:39:50'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
	"Modal. This method returns when an answer is provided."

	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock! !

