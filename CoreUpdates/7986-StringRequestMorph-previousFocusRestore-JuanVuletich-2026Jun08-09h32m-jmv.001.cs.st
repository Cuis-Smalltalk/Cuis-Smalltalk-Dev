'From Cuis7.9 [latest update: #7984] on 8 June 2026 at 9:40:40 am'!
!classDefinition: #StringRequestMorph category: #'Morphic-Composite Widgets'!
LinearLayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock textMorph titleLabelMorph prevKeyboardFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 8/Jun/2026 09:33:18'!
prevKeyboardFocus: aMorph
	prevKeyboardFocus := aMorph! !


!StringRequestMorph methodsFor: 'private' stamp: 'jmv 8/Jun/2026 09:37:22'!
cancel
	response := ''.
	self delete.
	cancelBlock ifNotNil: [cancelBlock value].
	prevKeyboardFocus ifNotNil: [
		prevKeyboardFocus world ifNotNil: [ :w |
			w activeHand newKeyboardFocus: prevKeyboardFocus ]].! !

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 8/Jun/2026 09:37:14'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| world delay done canceled answer |
	world := self world.
	world isNil ifTrue: [^ response asString asByteStringIfAscii ].
	done := false.
	canceled := false.
	(Preferences at: #focusFollowsMouse) ifFalse: [self textBox focusText].
	acceptBlock := [:aString| done := true].
	cancelBlock := [done := true. canceled := true].
	delay := Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ world doOneMinimalCycleNow. delay wait ].
	self delete.
	world doOneMinimalCycleNow.
	answer := canceled
		ifTrue: [aBlock value]
		ifFalse: [response asString asByteStringIfAscii].
	prevKeyboardFocus ifNotNil: [
		prevKeyboardFocus world ifNotNil: [ :w |
			w activeHand newKeyboardFocus: prevKeyboardFocus ]].
	^answer! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 8/Jun/2026 09:38:23'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	| morph world |
	morph := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	world :=self runningWorld.
	morph prevKeyboardFocus: world activeHand keyboardFocus.
	world addMorph: morph centeredNear: aPoint - self deltaToTextPane.
	"(Preferences at: #focusFollowsMouse) ifFalse: [answer textBox focusText]."
	morph textBox focusText.
	^ morph! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 8/Jun/2026 09:38:57'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"Modal. This method returns after an answer is provided."

	| morph world response |
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
	morph prevKeyboardFocus: world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	^response! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 8/Jun/2026 09:39:29'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
	"Modal. This method returns when an answer is provided."

	| morph world response |
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
	morph prevKeyboardFocus: world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	^response! !

!classDefinition: #StringRequestMorph category: #'Morphic-Composite Widgets'!
LinearLayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock textMorph titleLabelMorph prevKeyboardFocus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
