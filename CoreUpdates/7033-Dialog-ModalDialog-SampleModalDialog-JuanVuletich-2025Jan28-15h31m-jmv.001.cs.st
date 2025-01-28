'From Cuis7.3 [latest update: #7029] on 28 January 2025 at 3:32:33 pm'!
!classDefinition: #Dialog category: #'Morphic-Composite Widgets'!
BorderedBoxMorph subclass: #Dialog
	instanceVariableNames: 'layoutMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!Dialog commentStamp: '<historical>' prior: 0!
Hierachy of UI Dialogs, both Modal and Non-Modal.!

!classDefinition: #ModalDialog category: #'Morphic-Composite Widgets'!
Dialog subclass: #ModalDialog
	instanceVariableNames: 'callingProcess callingProcessIsUIProcess done response'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!ModalDialog commentStamp: '<historical>' prior: 0!
This class includes the fundamentals for any Modal Dialog.
For a more complete, working example, check SampleModalDialog.

Subclasses may offer both modal and non-modal functionality:
	SampleModalDialog openModal. "Print-it"
	SampleModalDialog open. "Nothing answered"
!

!classDefinition: #SampleModalDialog category: #'Morphic-Composite Widgets'!
ModalDialog subclass: #SampleModalDialog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!SampleModalDialog commentStamp: '<historical>' prior: 0!
SampleModalDialog openModal. "Print-it"
SampleModalDialog open. "Nothing answered"
!


!Dialog methodsFor: 'defaults' stamp: 'jmv 1/27/2025 14:58:41'!
defaultColor
	^ Theme current defaultWindowColor! !

!Dialog methodsFor: 'defaults' stamp: 'jmv 1/27/2025 14:58:45'!
defaultExtent
	^`400@300`! !

!Dialog methodsFor: 'defaults' stamp: 'jmv 1/27/2025 14:58:48'!
defaultPadding
	^ `30@10`! !

!Dialog methodsFor: 'layout' stamp: 'jmv 1/27/2025 15:01:32'!
beColumn
	layoutMorph
		ifNotNil: [ layoutMorph beColumn ]
		ifNil: [
			layoutMorph := LayoutMorph newColumn.
			self addMorphFront: layoutMorph ]! !

!Dialog methodsFor: 'geometry' stamp: 'jmv 1/27/2025 15:24:58'!
extentChanged: oldExtent
	"Our extent changed.
	- Resize our layoutMorph"

	layoutMorph ifNotNil: [
		layoutMorph
			position: self interiorOrigin
			extent: self interiorExtent ].! !

!Dialog methodsFor: 'geometry' stamp: 'jmv 1/27/2025 15:31:11'!
minimumExtent
	^layoutMorph minimumExtent + (borderWidth + padding * 2)! !


!Dialog class methodsFor: 'instance creation' stamp: 'jmv 1/27/2025 15:44:05'!
open
	"Open a Non-Modal Dialog. Answer user's response (if OK) or nil (if Cancel)
	This is an abstract class. Better play with SampleModalDialog. Inspect the result of:
		SampleModalDialog open
	"
	| world dialog |
	world := self runningWorld ifNil: [UISupervisor ui].
	dialog := self new beColumn buildWidgets.
	world addMorph: dialog centeredNear: world activeHand morphPosition.
	^ dialog.! !


!ModalDialog methodsFor: 'modal UI' stamp: 'jmv 1/27/2025 14:57:55'!
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

!ModalDialog methodsFor: 'modal UI' stamp: 'jmv 1/27/2025 14:57:59'!
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

!ModalDialog methodsFor: 'events' stamp: 'jmv 1/27/2025 14:59:20'!
cancel
	"May be redefined in subclasses. This default implementation may be enough, though."

	response := nil.
	self delete.! !

!ModalDialog methodsFor: 'events' stamp: 'jmv 1/27/2025 14:59:15'!
ok
	"This is a sample implementation. Subclasses should prepare a meaningful response"

	response := 'Build an appropriate response for OK'.
	self delete.! !

!ModalDialog methodsFor: 'initialization' stamp: 'jmv 1/27/2025 15:47:50'!
initialize
	super initialize.
	done := false.! !


!ModalDialog class methodsFor: 'instance creation' stamp: 'jmv 1/27/2025 15:50:02'!
openModal
	"Open a Modal Dialog. Answer user's response (if OK) or nil (if Cancel)
	This is an abstract class. Better play with SampleModalDialog. Inspect the result of:
		SampleModalDialog openModal
	Note: a ModalDialog can also be opened in non-modal mode calling #open.
		No answer will be returned, so #ok and #cancel need to do any approriate actions.
	"
	| world dialog |
	world := self runningWorld ifNil: [UISupervisor ui].
	dialog := self new beColumn buildWidgets.
	world addMorph: dialog centeredNear: world activeHand morphPosition.
	^ dialog waitForUserResponse.! !


!SampleModalDialog methodsFor: 'initialization' stamp: 'jmv 1/27/2025 15:33:03'!
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
	layoutMorph
		addMorph: 
			((LabelMorph contents: 'This is a title for this Dialog' fontSize: 25)
				borderWidth: 2; borderColor: color lighter;
				padding: 20);
		addMorph: Sample01Star new;
		addMorph: typeRow;
		addMorph: buttonRow.
	layoutMorph
		position: self interiorOrigin
		extent: self interiorExtent! !

!SampleModalDialog methodsFor: 'layout' stamp: 'jmv 1/27/2025 15:32:15'!
defaultExtent
	^`600@400`! !

