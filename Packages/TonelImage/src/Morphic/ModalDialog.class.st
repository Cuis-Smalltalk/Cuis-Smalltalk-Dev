"
This class includes the fundamentals for any Modal Dialog.
For a more complete, working example, check SampleModalDialog.

Subclasses may offer both modal and non-modal functionality:
	SampleModalDialog openModal. "Print-it"
	SampleModalDialog open. "Nothing answered"

"
Class {
	#name : 'ModalDialog',
	#superclass : 'Dialog',
	#instVars : [
		'callingProcess',
		'callingProcessIsUIProcess',
		'done',
		'response'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'instance creation' }
ModalDialog class >> openModal [
	
	| world |
	world := self runningWorld ifNil: [UISupervisor ui].
	^ self 
		openModal: self new beColumn buildWidgets
		centeredNear: world activeHand morphPosition
]

{ #category : 'instance creation' }
ModalDialog class >> openModal: aModalDialog centeredNear: aWorldPosition [
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
	^answer
]

{ #category : 'events' }
ModalDialog >> cancel [
	"May be redefined in subclasses. This default implementation may be enough, though."

	response := nil.
	self delete.
]

{ #category : 'modal UI' }
ModalDialog >> delete [
	"Resume the calling process, so #waitForUserResponse can finish and answer 'response'."

	super delete.
	done ifFalse: [
		done := true.
		callingProcess ifNotNil: [ "Only if (as usual) our calling process is suspended"
			callingProcessIsUIProcess
				ifTrue: [
					UISupervisor uiProcess: callingProcess.
					callingProcess := nil.
					Processor terminateActive ]
				ifFalse: [
					callingProcess resume.
					callingProcess := nil ]]].
]

{ #category : 'initialization' }
ModalDialog >> initialize [
	super initialize.
	done := false.
]

{ #category : 'events' }
ModalDialog >> ok [
	"This is a sample implementation. Subclasses should prepare a meaningful response"

	response := 'Build an appropriate response for OK'.
	self delete.
]

{ #category : 'modal UI' }
ModalDialog >> waitForUserResponse [
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
	^ response.
]
