'From Cuis7.5 [latest update: #7560] on 11 September 2025 at 4:27:57 pm'!

!ModalDialog methodsFor: 'modal UI' stamp: 'jmv 9/11/2025 16:10:40'!
waitForUserResponse
	"Wait for the user to accept or cancel, and answer the response. Answers nil if the user cancels."
	"Details:
	This method suspends the current process until we have an appropriate 'response' and close.
	To keep the UI alive, it schedules a new Morphic Process that is terminated as ours is resumed.
	(If called from a non-UI process, a new Morphic process is not needed, but the calling process is suspended nevertheless)."

	| w |
	w := self world.
	w isNil ifTrue: [^ response ].
	callingProcess := nil.
	done := false.

	"If called from the UI process, suspend it (to later resume it with the answer)
	and run a new UI process for UI interaction with the receiver.."
	callingProcessIsUIProcess := UISupervisor isUIProcessRunning.
	callingProcessIsUIProcess ifTrue: [
		UISupervisor spawnNewMorphicProcessFor: w createNewCanvas: false ].

	"Suspend the calling process, regardless of it being the UI process or not.
	Usually variable 'done' will be false at this point.
	In the extremely rare case where user has already done [ok] or [cancel] in the just spawned new UI process,
	don't suspend us, as no one would resume us again."
	done ifFalse: [
		callingProcess := Processor activeProcess.
		callingProcess suspend ].

	"At this point we are resumed by ModalDialog >> #delete, after user doing [ok] or [cancel]"
	^ response.! !


!ModalDialog class methodsFor: 'instance creation' stamp: 'jmv 9/11/2025 16:19:08'!
openModal
	"Open a Modal Dialog. Answer user's response (if OK) or nil (if Cancel)
	This is an abstract class. Better play with SampleModalDialog. Inspect the result of:
		SampleModalDialog openModal
	Note: a ModalDialog can also be opened in non-modal mode calling #open.
		No answer will be returned, so #ok and #cancel need to do any approriate actions.
	"
	| world dialog answer prevKbdFocus |
	world := self runningWorld ifNil: [UISupervisor ui].
	prevKbdFocus := world activeHand keyboardFocus.
	dialog := self new beColumn buildWidgets.
	world addMorph: dialog centeredNear: world activeHand morphPosition.
	answer := dialog waitForUserResponse.
	prevKbdFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKbdFocus ].
	^answer! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 9/11/2025 16:17:20'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"Modal. This method returns after an answer is provided."

	| morph world prevKbdFocus response |
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
	prevKbdFocus := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	prevKbdFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKbdFocus ].
	^response! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 9/11/2025 16:18:16'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
	"Modal. This method returns when an answer is provided."

	| morph world prevKbdFocus response |
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
	prevKbdFocus := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	prevKbdFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKbdFocus ].
	^response! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/11/2025 16:05:47'!
releaseKeyboardFocusIfOrOwnedBy: aMorph
	"If the given morph had the keyboard focus before, release it"
	keyboardFocus ifNotNil: [
		keyboardFocus withAllOwnersDo: [ :outerOwner |
			outerOwner == aMorph ifTrue: [
				self releaseKeyboardFocus.
				"Try to find a good keyboard focus"
				self dispatchMouseOverEvent ]]]! !

