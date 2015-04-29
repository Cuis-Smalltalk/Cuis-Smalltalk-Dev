'From Cuis 4.2 of 25 July 2013 [latest update: #2241] on 15 April 2015 at 9:58:11.167929 pm'!

!Cursor methodsFor: 'displaying' stamp: 'jmv 4/15/2015 21:45'!
showCursor
	"Make the hardware's mouse cursor look like the receiver"

	Cursor currentCursor: self! !


!Taskbar class methodsFor: 'commands' stamp: 'jmv 4/15/2015 21:52'!
hideTaskbar

	self singleton visible: false! !

!Taskbar class methodsFor: 'commands' stamp: 'jmv 4/15/2015 21:45'!
showTaskbar

	| taskbar | taskbar _ self singleton.
	
	taskbar visible: true.
	taskbar isInWorld ifFalse: [ taskbar openInWorld ]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/15/2015 21:50'!
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
	Cursor write showCursor.
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

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 4/15/2015 21:50'!
startup: resuming
	"
	resuming
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"

	"Here, startup begins!!"
	Cursor normal showCursor.
	self setGCParameters.
	resuming ifTrue: [
		self clearExternalObjects ].
	self readCommandLineArguments.
	self processStartUpList: resuming.
	resuming ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp.
		self processCommandLineArguments ].! !


!Cursor methodsFor: 'displaying' stamp: 'jmv 4/15/2015 21:46'!
showWhile: aBlock 
	"While evaluating the argument, aBlock, make the receiver be the cursor 
	shape."

	| oldcursor |
	oldcursor _ Cursor currentCursor.
	self showCursor.
	^aBlock ensure: [oldcursor showCursor]
! !


!EventSensor methodsFor: 'private' stamp: 'jmv 4/15/2015 21:46'!
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
						Cursor wait showCursor ].
					"Discard any mouse events. This code is run when the UI is slow, essentially to have a working
					interrupt key. Processing mouse events is pointless: the UI will not handle them anyway.
					In addition, at least on Windows 7, when the machine is suspended and resumed with Cuis
					running, a lot of meaningless mouseMove events with the same coordinates are sent, maing
					Cuis extremely slow and CPU hungry for a few minutes without reason. Discarding mouse
					events makes the 'processing' of those very quick."
					self fetchMoreEventsDiscardingMouseEvents: true ]
		] on: Error do: [ :ex | ].
	] repeat! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 4/15/2015 21:49'!
restoreMorphicDisplay
	DisplayScreen startUp.
	self
		morphExtent: Display extent;
		viewBox: Display boundingBox;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded.
	WorldState addDeferredUIMessage: [ Cursor normal showCursor ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2015 21:48'!
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
			Cursor currentCursor == Cursor blank ifFalse: [Cursor blank showCursor].
			^ true].

	^ false! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2015 21:48'!
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
		Cursor currentCursor == Cursor normal ifFalse: [ Cursor normal showCursor ].	"show hardware cursor"
		savedPatch _ nil ]! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 4/15/2015 21:48'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self cursor showCursor.
	hand _ aMouseButtonEvent hand.
	self startStepping! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 4/15/2015 21:48'!
mouseEnter: anEvent
	super mouseEnter: anEvent.
	self cursor showCursor.
	hand _ anEvent hand! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 4/15/2015 21:48'!
mouseLeave: anEvent

	super mouseLeave: anEvent.
	hand ifNotNil: [
		hand _ nil.
		Cursor normal showCursor ]! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 4/15/2015 21:49'!
stepAt: millisecondClockValue
	"got the #mouseLeave: message"

	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal showCursor ].
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
				Cursor normal showCursor ]]! !


!Point class methodsFor: 'instance creation' stamp: 'jmv 4/15/2015 21:49'!
fromUser
	Sensor waitNoButton.
	Cursor crossHair showCursor.
	Sensor waitButton.
	Cursor normal showCursor.
	^ Sensor mousePoint

"Point fromUser"! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 4/15/2015 21:49'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe false)
		(syntaxHighlightingAsYouType true)
		(tapAndHoldEmulatesButton2 true)
		(clickGrabsMorphs false)
		
		(allowUnderscoreAssignments true)
		(allowUnderscoreSelectors true)
		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	).
	self useAllIcons.
	Theme beCurrent.
	Taskbar showTaskbar! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 4/15/2015 21:52'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoIcons.
	self runningWorld backgroundImageData: nil.
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 4/15/2015 21:52'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak and Cuis in recent years. Caution: turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

	Preferences smalltalk80
	"

	self setPreferencesFrom:

	#(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating false)
		(subPixelRenderFonts true)
		(thoroughSenders false)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoIcons.
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 4/15/2015 21:53'!
preferencesMenu
	"Build the preferences menu for the world."

	^self fillIn: (self menu: 'Preferences...') from: {
		{'Focus follows mouse'. {Preferences. #enableFocusFollowsMouse}. 'At all times, make the active window and widget the one on which the mouse is located.'}.
		{'Click to focus'. {Preferences. #disableFocusFollowsMouse}. 'At all times, make the active window and widget the one where the mouse was clicked.'}.
		{'Font Sizes...' . {Theme . #changeFontSizes} . 'use larger or smaller text and widgets'}.
		{'Icons...' . {Theme . #changeIcons} . 'show more or less icons.'}.
		{'Themes...' . {Theme . #changeTheme} . 'switch to another theme.'}.
		nil.
		{'Show taskbar' . {Taskbar . #showTaskbar} . 'show the taskbar'}.
		{'Hide taskbar' . {Taskbar . #hideTaskbar} . 'hide the taskbar'}.
		nil.
		{'Full screen on' . { self . #fullScreenOn} . 'puts you in full-screen mode, if not already there.'}.
		{'Full screen off' . { self . #fullScreenOff} . 'if in full-screen mode, takes you out of it.'}.
		nil.
		{'Set display depth...' . {self. #setDisplayDepth} . 'choose how many bits per pixel.'}.
		{'Set desktop color...' . {self. #changeBackgroundColor} . 'choose a uniform color to use as desktop background.'}.
		nil.
		{'Set Code Author...'. {Utilities. #setAuthor}. 'supply initials to be used to identify the author of code and other content.'}.
		{'All preferences...'. {Preferences. #openPreferencesInspector}. 'view and change various options.'}.
	}! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 4/15/2015 21:47'!
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	"See #eventTickler"
	Cursor currentCursor = Cursor wait ifTrue: [
		Cursor normal showCursor ].

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

!methodRemoval: Taskbar class #hide!
Taskbar class removeSelector: #hide!
!methodRemoval: Taskbar class #show!
Taskbar class removeSelector: #show!
!methodRemoval: Cursor #show!
Cursor removeSelector: #show!
!methodRemoval: Cursor #showCorsor!
Cursor removeSelector: #showCorsor!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Sensor installEventTickler!

