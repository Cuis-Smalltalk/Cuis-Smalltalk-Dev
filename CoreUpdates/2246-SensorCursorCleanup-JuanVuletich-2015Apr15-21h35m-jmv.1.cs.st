'From Cuis 4.2 of 25 July 2013 [latest update: #2241] on 15 April 2015 at 9:43:40.680929 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 4/15/2015 20:31'!
resumeProcess
	| mustTerminateActive |
	savedCursor
		ifNotNil: [Cursor currentCursor: savedCursor].
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

!Debugger methodsFor: 'initialization' stamp: 'jmv 4/15/2015 20:33'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor currentCursor: Cursor normal.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices, (msgString ifNil: ['']) ]
		ifFalse: [ msgString ].

	interruptedProcessUI _ ProjectX newProcessIfUIX: interruptedProcess.
	WorldState addDeferredUIMessage: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !


!Cursor methodsFor: 'displaying' stamp: 'jmv 4/15/2015 20:36'!
show
	"Make the hardware's mouse cursor look like the receiver"

	Cursor currentCursor: self! !

!Cursor methodsFor: 'displaying' stamp: 'jmv 4/15/2015 20:33'!
showWhile: aBlock 
	"While evaluating the argument, aBlock, make the receiver be the cursor 
	shape."

	| oldcursor |
	oldcursor _ Cursor currentCursor.
	self show.
	^aBlock ensure: [oldcursor show]
! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2015 20:34'!
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
			Cursor currentCursor == Cursor blank ifFalse: [Cursor blank show].
			^ true].

	^ false! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2015 20:34'!
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
		Cursor currentCursor == Cursor normal ifFalse: [ Cursor normal show ].	"show hardware cursor"
		savedPatch _ nil ]! !

!methodRemoval: InputSensor #currentCursor!
InputSensor removeSelector: #currentCursor!
!methodRemoval: InputSensor #currentCursor:!
InputSensor removeSelector: #currentCursor:!
