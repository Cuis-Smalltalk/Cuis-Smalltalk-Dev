'From Cuis7.1 [latest update: #6807] on 1 November 2024 at 10:36:31 am'!
!classDefinition: #ModalMorph category: #'Morphic-ModalWidgets'!
LayoutMorph subclass: #ModalMorph
	instanceVariableNames: 'callingProcess callingProcessIsUIProcess done response'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-ModalWidgets'!

!ModalMorph commentStamp: '<historical>' prior: 0!
This class includes the fundamentals for any Modal Morph.
For a more complete, working example, check ModalDialog.!

!classDefinition: #ModalDialog category: #'Morphic-ModalWidgets'!
ModalMorph subclass: #ModalDialog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-ModalWidgets'!

!ModalDialog commentStamp: 'jmv 10/31/2024 14:45:17' prior: 0!
A ModalDialog waits for user to [Accept] or [Cancel] before returning the user's response and continuing execution.
Inspect the answer of (click either on [Accept] or [Cancel]:

	ModalDialog openModal

#buildWidgets and #ok are sample implementations. Subclasses should reimplement them.!


!ModalMorph methodsFor: 'modal UI' stamp: 'jmv 10/31/2024 13:02:49'!
delete
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
					callingProcess := nil ]]].! !

!ModalMorph methodsFor: 'modal UI' stamp: 'jmv 10/31/2024 13:02:41'!
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

	"At this point we are resumed by #deleteAndResumeCallingProcess, after user doing [ok] or [cancel]"
	^ response.! !


!ModalMorph class methodsFor: 'instance creation' stamp: 'jmv 10/31/2024 14:46:54'!
openModal
	"Open a Modal Dialog. Answer user's response (if OK) or nil (if Cancel)
	This is an abstract class. Better play with ModalDialog. Inspect the result of:
		ModalDialog openModal
	"
	| world dialog |
	world := self runningWorld ifNil: [UISupervisor ui].
	dialog := self newColumn buildWidgets.
	world addMorph: dialog centeredNear: world activeHand morphPosition.
	^ dialog waitForUserResponse.! !


!ModalDialog methodsFor: 'initialization' stamp: 'jmv 10/31/2024 14:44:50'!
buildWidgets
	"
	This is a sample implementation. Actual subclasses should reimplement #buildWidgets (without calling super!!).
	self openModal
	"

	| typeRow buttonRow |
	typeRow := LayoutMorph newRow.
	typeRow
		addMorph: (LabelMorph contents: 'Type');
		addMorph: (LabelMorph contents: 'So what');
		addMorph: (LabelMorph contents: 'And then').
	buttonRow := LayoutMorph newRow.
	buttonRow
		padding: 10;
		gap: 20;
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	self
		addMorph: 
			((LabelMorph contents: 'This is a title for this Dialog' fontSize: 25)
				borderWidth: 2; borderColor: color lighter;
				padding: 20);
		addMorph: Sample01Star new;
		addMorph: typeRow;
		addMorph: buttonRow.! !

!ModalDialog methodsFor: 'defaults' stamp: 'jmv 10/28/2024 16:56:37'!
defaultColor
	^ Theme current defaultWindowColor! !

!ModalDialog methodsFor: 'defaults' stamp: 'jmv 10/28/2024 16:56:43'!
defaultExtent
	^`400@300`! !

!ModalDialog methodsFor: 'defaults' stamp: 'jmv 10/31/2024 14:44:43'!
defaultPadding
	^ `30@10`! !

!ModalDialog methodsFor: 'events' stamp: 'jmv 10/31/2024 14:44:32'!
cancel
	"May be redefined in subclasses. This default implementation may be enough, though."

	response := nil.
	self delete.! !

!ModalDialog methodsFor: 'events' stamp: 'jmv 10/31/2024 14:43:46'!
ok
	"This is a sample implementation. Subclasses should prepare a meaningful response"

	response := 'Build an appropriate response for OK'.
	self delete.! !

