'From Cuis 4.2 of 25 July 2013 [latest update: #2241] on 15 April 2015 at 10:13:16.881929 pm'!

!Cursor methodsFor: 'displaying' stamp: 'jmv 4/15/2015 22:11'!
activateCursor
	"Make the hardware's mouse cursor look like the receiver"

	Cursor currentCursor: self! !

!Cursor methodsFor: 'primitives' stamp: 'jmv 4/15/2015 22:09'!
installCursor
	"Primitive. Tell the interpreter to use the receiver as the current cursor 
	image. Fail if the receiver does not match the size expected by the 
	hardware. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed! !

!Cursor methodsFor: 'primitives' stamp: 'jmv 4/15/2015 22:10'!
installCursorWithMask: maskForm
	"Primitive. Tell the interpreter to use the receiver as the current cursor image with the given mask Form. Both the receiver and the mask should have extent 16@16 and a depth of one. The mask and cursor bits are combined as follow:
			mask	cursor	effect
			 0		  0		transparent (underlying pixel shows through)
			 1		  1		opaque black
			 1		  0		opaque white
			 0		  1		invert the underlying pixel"
"Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed
! !


!CursorWithAlpha methodsFor: 'primitives' stamp: 'jmv 4/15/2015 22:10'!
installCursor
	<primitive: 101>
	self fallback installCursor! !


!CursorWithMask methodsFor: 'primitives' stamp: 'jmv 4/15/2015 22:10'!
installCursor

	^ self installCursorWithMask: maskForm! !


!Debugger methodsFor: 'private' stamp: 'jmv 4/15/2015 22:12'!
resumeProcess
	| mustTerminateActive |
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		Processor activeProcess animatedUI = interruptedProcessUI ifTrue: [
			interruptedProcess animatedUI: interruptedProcessUI.
			mustTerminateActive _ true ].
		interruptedProcess resume ].
	"if old process was terminated, just terminate current one"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	self runningWorld ifNotNil: [ :w | w displayWorld ].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, then it will terminate and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 4/15/2015 22:11'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor normal activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices, (msgString ifNil: ['']) ]
		ifFalse: [ msgString ].

	interruptedProcessUI _ ProjectX newProcessIfUIX: interruptedProcess.
	WorldState addDeferredUIMessage: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/15/2015 22:12'!
snapshot: save andQuit: quit embedded: embeddedFlag
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	| resuming msg |
	ActiveModel flushEventSystem.
	(SourceFiles at: 2) ifNotNil: [
		msg _ String streamContents: [ :s |
			s
				nextPutAll: '----';
				nextPutAll:
				(save
					ifTrue: [
						quit
							ifTrue: [ 'QUIT' ]
							ifFalse: [ 'SNAPSHOT' ]]
					ifFalse: [
						quit
							ifTrue: [ 'QUIT/NOSAVE' ]
							ifFalse: [ 'NOP' ]]);
				nextPutAll: '----';
				print: Date dateAndTimeNow;
				space;
				nextPutAll: (FileDirectory default localNameFor: self imageName);
				nextPutAll: ' priorSource: ';
				print: LastQuitLogPosition ].
		self assureStartupStampLogged.
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ].
		self logChange: msg.
		Transcript
			 newLine;
			 show: msg;
			 newLine ].
	self processShutDownList: quit.
	Cursor write activateCursor.
	save
		ifTrue: [
			resuming _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ resuming _ false ].
	quit & (resuming == false) ifTrue: [ self quitPrimitive ].

	"Here, startup begins!! (resuming might be nil)"
	self startup: resuming == true.

	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].
	^ resuming! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 4/15/2015 22:12'!
startup: resuming
	"
	resuming
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"

	"Here, startup begins!!"
	Cursor normal activateCursor.
	self setGCParameters.
	resuming ifTrue: [
		self clearExternalObjects ].
	self readCommandLineArguments.
	self processStartUpList: resuming.
	resuming ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp.
		self processCommandLineArguments ].! !


!Cursor methodsFor: 'displaying' stamp: 'jmv 4/15/2015 22:12'!
showWhile: aBlock 
	"While evaluating the argument, aBlock, make the receiver be the cursor 
	shape."

	| oldcursor |
	oldcursor _ Cursor currentCursor.
	self activateCursor.
	^aBlock ensure: [oldcursor activateCursor]
! !


!Cursor class methodsFor: 'current cursor' stamp: 'jmv 4/15/2015 22:10'!
currentCursor: aCursor 
	"Make the instance of cursor, aCursor, be the current cursor. Display it. 
	Create an error if the argument is not a Cursor."

	CurrentCursor _ aCursor.
	Preferences biggerCursors
		ifTrue: [
			[ ^aCursor asBigCursor installCursor] 
				on: Error do: nil  "fall through "].
	aCursor installCursor! !


!EventSensor methodsFor: 'private' stamp: 'jmv 4/15/2015 22:12'!
eventTickler
	"If the UI process hasn't processed events in a while, do it here.
	This is needed to detect the interrupt key."
	| delay delta |
	delay _ Delay forMilliseconds: self class eventPollPeriod.
	self lastEventPoll.	"ensure not nil."
	[
		[ 
			delay wait.
			delta _ Time localMillisecondClock - lastEventPoll.
			delta > self class eventPollPeriod
				ifTrue: [
					"See #doOneCycleNowFor:"
					Cursor currentCursor = Cursor normal ifTrue: [
						Cursor wait activateCursor ].
					"Discard any mouse events. This code is run when the UI is slow, essentially to have a working
					interrupt key. Processing mouse events is pointless: the UI will not handle them anyway.
					In addition, at least on Windows 7, when the machine is suspended and resumed with Cuis
					running, a lot of meaningless mouseMove events with the same coordinates are sent, maing
					Cuis extremely slow and CPU hungry for a few minutes without reason. Discarding mouse
					events makes the 'processing' of those very quick."
					self fetchMoreEventsDiscardingMouseEvents: true ]
		] on: Error do: [ :ex | ].
	] repeat! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 4/15/2015 22:12'!
restoreMorphicDisplay
	DisplayScreen startUp.
	self
		morphExtent: Display extent;
		viewBox: Display boundingBox;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded.
	WorldState addDeferredUIMessage: [ Cursor normal activateCursor ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2015 22:12'!
needsToBeDrawn
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor from the display."
	
	"Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"

	(savedPatch notNil
		or: [ submorphs anySatisfy: [ :ea | ea visible ] ] )
		ifTrue: [
			"using the software cursor; hide the hardware one"
			Cursor currentCursor == Cursor blank ifFalse: [Cursor blank activateCursor].
			^ true].

	^ false! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2015 22:12'!
restoreSavedPatchOn: aCanvas 
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas image: savedPatch at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^self ].

		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self invalidateDisplayRect: (self morphPosition extent: extent) from: nil.
		Cursor currentCursor == Cursor normal ifFalse: [ Cursor normal activateCursor ].	"show hardware cursor"
		savedPatch _ nil ]! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 4/15/2015 22:12'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self cursor activateCursor.
	hand _ aMouseButtonEvent hand.
	self startStepping! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 4/15/2015 22:12'!
mouseEnter: anEvent
	super mouseEnter: anEvent.
	self cursor activateCursor.
	hand _ anEvent hand! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 4/15/2015 22:12'!
mouseLeave: anEvent

	super mouseLeave: anEvent.
	hand ifNotNil: [
		hand _ nil.
		Cursor normal activateCursor ]! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 4/15/2015 22:12'!
stepAt: millisecondClockValue
	"got the #mouseLeave: message"

	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal activateCursor ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			Preferences cheapWindowReframe ifTrue: [
				owner morphBoundsInWorld newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphBoundsInWorld ]].
			]
		ifFalse: [
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal activateCursor ]]! !


!Point class methodsFor: 'instance creation' stamp: 'jmv 4/15/2015 22:12'!
fromUser
	Sensor waitNoButton.
	Cursor crossHair activateCursor.
	Sensor waitButton.
	Cursor normal activateCursor.
	^ Sensor mousePoint

"Point fromUser"! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 4/15/2015 22:12'!
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	"See #eventTickler"
	Cursor currentCursor = Cursor wait ifTrue: [
		Cursor normal activateCursor ].

	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely: aWorld.

	"Run steps, alarms and deferred UI messages"
	aWorld runStepMethods.

	"Process user input events. Run all event triggered code."
	hadAnyEvent _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ hadAnyEvent | h processEventQueue.
		activeHand _ nil ].

	"The default is the primary hand"
	activeHand _ self hands first.


	^hadAnyEvent! !

!methodRemoval: CursorWithMask #beCursor!
CursorWithMask removeSelector: #beCursor!
!methodRemoval: CursorWithAlpha #beCursor!
CursorWithAlpha removeSelector: #beCursor!
!methodRemoval: Cursor #beCursor!
Cursor removeSelector: #beCursor!
!methodRemoval: Cursor #beCursorWithMask:!
Cursor removeSelector: #beCursorWithMask:!
!methodRemoval: Cursor #changed:!
Cursor removeSelector: #changed:!
!methodRemoval: Cursor #showCursor!
Cursor removeSelector: #showCursor!
