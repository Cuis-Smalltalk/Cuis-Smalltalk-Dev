'From Cuis 6.0 [latest update: #5600] on 19 December 2022 at 6:59:24 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 7/14/2022 13:27:09'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay _ self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		(UISupervisor isUIProcessRunning and: [ UISupervisor ui == interruptedProcessUI]) ifTrue: [
			UISupervisor uiProcess: interruptedProcess.
			mustTerminateActive _ true ].
		interruptedProcess resume ].
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorld ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 12/19/2022 18:55:47'!
mainLoop
	| d |

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.

	canvas isNil ifTrue: [
		self setMainCanvas ].
	d _ Delay forMilliseconds: 50.
	[
		self displayWorld.
		[
			self doOneCycle: d.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex | | erroredMorph |
		"Handle a drawing error"
		erroredMorph _ (ex signalerContext findContextSuchThat: [ :context | (context receiver is: #Morph) ])
			ifNotNil: [ :context | context receiver ]
			ifNil: [ canvas currentMorph ].
		erroredMorph drawingFails.
		self clearCanvas.
		"Install the old error handler, so we can re-raise the error.
		This means, open a Debugger.
		This will also create a new Morphic process. if the user closes the debugger."
		ex receiver error: ex description.
		"Create a new UI process, if the user proceeds in the debugger."
		UISupervisor newUIProcess.
	]! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/19/2022 18:47:38'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	lastCycleTime _ Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	canvas ensureCurrentMorphIsWorld.
	self displayWorld.
	"Run steps, alarms and deferred UI messages"
	self runDeferredUIMessages.
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.
	"Process user input events. Run all event triggered code."
	hadAny _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAny _ hadAny | h processEventQueue ].
	"The default is the primary hand"
	activeHand _ self hands first.
	"Run any deferred UI messages that were enqueued during this cycle before the inter cycle pause."
	self runDeferredUIMessages.
	lastCycleHadAnyEvent _ hadAny.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/19/2022 18:49:19'!
doOneMinimalCycleNow
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorld.

	"Process user input events. Run all event triggered code."
	^activeHand processEventQueue! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/19/2022 18:48:32'!
collapseAll
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse].
	self collapseNonWindows.
	self displayWorld! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 12/19/2022 18:46:21'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner hasMouseFocus ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: true.
		self world ifNotNil: [ :w | w displayWorld ].  "show myself"
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent! !


!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 12/19/2022 18:47:03'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		w displayWorld.  "show myself"
		].
	aBlock value.
	self delete! !

!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 5/5/2018 19:37:19'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorld	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorld ]! !


!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:11:00'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self fullUpdateCurrentProtrudingBounds.
		self outOfMorph ].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:10:54'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self fullUpdateCurrentProtrudingBounds.
		self outOfMorph ].! !

!methodRemoval: WorldMorph #displayWorldSafely stamp: 'jmv 12/19/2022 18:50:04'!
WorldMorph removeSelector: #displayWorldSafely!
!methodRemoval: Morph #refreshWorld stamp: 'jmv 12/19/2022 18:47:45'!
Morph removeSelector: #refreshWorld!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
		UISupervisor newUIProcess.!

