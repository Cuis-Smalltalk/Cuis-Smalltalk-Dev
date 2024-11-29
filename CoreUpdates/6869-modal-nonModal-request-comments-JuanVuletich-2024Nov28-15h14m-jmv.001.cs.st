'From Cuis7.1 [latest update: #6868] on 28 November 2024 at 3:25:06 pm'!

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 04:00:44'!
request: queryString do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: '' verifying: [:aString| true] do: acceptBlock orCancel: []! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 04:11:48'!
request: queryString initialAnswer: defaultAnswer do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: [:aString| true] do: acceptBlock orCancel: []! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 03:59:45'!
request: queryString initialAnswer: defaultAnswer do: acceptBlock orCancel: cancelBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: [:aString| true] do: acceptBlock orCancel: cancelBlock! !

!Object methodsFor: 'user interface' stamp: 'len 5/20/2020 07:37:39'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: []! !

!Object methodsFor: 'user interface' stamp: 'jmv 5/23/2020 21:00:11'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	^ UISupervisor ui request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'hlsf 6/24/2022 09:32:19'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	| answer |
	answer := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: answer centeredNear: aPoint - self deltaToTextPane.
	(Preferences at: #focusFollowsMouse) ifFalse: [answer textBox focusText].
	^ answer! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'HAW 9/23/2020 18:47:29'!
request: queryString initialAnswer: defaultAnswer do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	
	^ self 
		request: queryString 
		centeredAt: self runningWorld activeHand morphPosition 
		initialAnswer: defaultAnswer 
		validationBlock: [:aString| true] 
		acceptBlock: acceptBlock 
		cancelBlock: []! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 11/28/2024 15:14:52'!
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
	^ morph getUserResponseOrCancel: cancelBlock! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'HAW 9/23/2020 18:48:44'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	
	^ self 
		request: queryString 
		centeredAt: self runningWorld activeHand morphPosition 
		initialAnswer: defaultAnswer 
		validationBlock: validationBlock 
		acceptBlock: acceptBlock
		cancelBlock: cancelBlock! !

