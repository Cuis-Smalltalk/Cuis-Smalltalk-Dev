"
A WorldMorph is a kind of PasteUpMorph that can act as the root morph of a Morphic system. Usually uses the whole Display.

activeHand 				-- a HandMorph
hands 					-- Array of HandMorphs (typically only one) representing the Cursor and its event queue.
canvas 					-- BitBlitCanvas on the DisplayScreen. (Display getCanvas)
damageRecorder 		-- the DamageRecorder for the Display
stepList 					-- a Heap of StepMessage.  Each morph which wants stepping has a StepMessage here.
								See comment in #Morph>>stepAt:
lastStepTime 			-- 'now' as last sampled at step. (Time localMillisecondClock)
lastStepMessage 		-- nil or a selector
lastCycleTime 			-- 'now' as last sampled (Time localMillisecondClock)
alarms 					-- a Heap of MorphicAlarm.  See comment in #Morph>>addAlarm:after:
								An _alarm_ is an action to be done once, vs a _step_ which is cycled.
lastAlarm 				-- 'now' as sampled at last alarm (Time localMillisecondClock).
drawingFailureMorphs 	-- a WeakIdentitySet of Morphs with drawing failures
waitDelay 				-- a Delay
pause 					-- A 'phase-locked loop' style value to help regularise the step/alarm/event service rate.
lastCycleHadAnyEvent 	-- a boolean

"
Class {
	#name : 'WorldMorph',
	#superclass : 'PasteUpMorph',
	#instVars : [
		'activeHand',
		'canvas',
		'damageRecorder',
		'stepList',
		'lastCycleTime',
		'alarms',
		'lastAlarmTime',
		'deferredUIMessages',
		'drawingFailingMorphs',
		'waitDelay',
		'pause',
		'lastCycleHadAnyEvent',
		'taskbar'
	],
	#classVars : [
		'ScaleFactor'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'new-morph participation' }
WorldMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'class initialization' }
WorldMorph class >> initClassCachedState [
	ScaleFactor := DisplayScreen hostOSScaleFactor
]

{ #category : 'instance creation' }
WorldMorph class >> newWorld [
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w := self new.
	w color: (Color r: 0.167 g: 0.344 b: 0.629).
	w position: `0@0` extent: Display extent.
	w activeHand privateOwner: w.
	^w
]

{ #category : 'hands' }
WorldMorph >> activeHand [
	^activeHand
]

{ #category : 'alarms' }
WorldMorph >> addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime [
	"Add a new alarm with the given set of parameters"

	alarms add: 
		(MorphicAlarm 
			receiver: aTarget
			selector: aSelector
			arguments: argArray
			at: scheduledTime)
]

{ #category : 'errors on draw' }
WorldMorph >> addKnownFailing: aMorph [
	drawingFailingMorphs add: aMorph
]

{ #category : 'submorphs-add/remove' }
WorldMorph >> addMorph: aMorph centeredNear: aPoint [
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	self addMorph: aMorph.
	self center: aMorph near: aPoint.
]

{ #category : 'menu & halo' }
WorldMorph >> addWorldHaloMenuItemsTo: aMenu hand: aHandMorph [
	"Add standard halo items to the menu, given that the receiver is a World"

	| unlockables |
	self addColorMenuItems: aMenu hand: aHandMorph.

"	aMenu addLine.
	self addWorldToggleItemsToHaloMenu: aMenu."
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.

	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables := self submorphs select:
		[ :m | m isLocked].
	unlockables size = 1 ifTrue: [
		aMenu add: ('unlock "{1}"' format:{unlockables first shortPrintString})action: #unlockContents].
	unlockables size > 1 ifTrue: [
		aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.

]

{ #category : 'change reporting' }
WorldMorph >> addedMorph: aMorph [
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]
]

{ #category : 'alarms' }
WorldMorph >> adjustAlarmTimes: nowTime [
	"Adjust the alarm times after some clock weirdness (such as image-startup etc)"
	| deltaTime |
	deltaTime := nowTime - lastAlarmTime.
	alarms do: [ :alarm |
		alarm scheduledTime: alarm scheduledTime + deltaTime ]
]

{ #category : 'alarms' }
WorldMorph >> alarmSortBlock [

	^[ :alarm1 :alarm2 | alarm1 scheduledTime < alarm2 scheduledTime ]
]

{ #category : 'submorphs-accessing' }
WorldMorph >> allMorphsDo: aBlock [
	"Enumerate all morphs in the world, including those held in hands."

	super allMorphsDo: aBlock.
	activeHand allMorphsDo: aBlock.
]

{ #category : 'world state' }
WorldMorph >> allNonWindowRelatedSubmorphs [
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #TaskbarMorph ] ]
]

{ #category : 'dropping/grabbing' }
WorldMorph >> allowsFilesDrop [

	^ true
]

{ #category : 'geometry' }
WorldMorph >> basicDisplayBounds [
	^ 0@0 extent: extent
]

{ #category : 'windows' }
WorldMorph >> bringWindowsFullOnscreen [
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ]) do: [ :each |
		each makeMeFullyVisible ]
]

{ #category : 'submorphs-add/remove' }
WorldMorph >> center: aMorph near: aPoint [
	"Position the given morph in this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	aMorph morphExtent
		ifNil: [ aMorph positionInWorld: aPoint ]
		ifNotNil:
			[ :e |
				| trialRect delta |
				trialRect := Rectangle center: aPoint extent: e.
				delta := trialRect amountToTranslateWithin: self displayBounds.
				aMorph positionInWorld: (trialRect origin + delta) rounded ].
]

{ #category : 'update cycle' }
WorldMorph >> checkForNewScreenSize [
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p needsNewUIProcessAndCanvas |

	"Linux and MacOS VMs continuously update #actualScreenSize while the user is resizing the OS window.
	Try to avoid reallocation of Display until the user is done with that.
	For that, wait until a real event happens inside Cuis. (This requires the user to move the mouse back into Cuis).
	Or use a timer, and do it no more often than every quarter a second, and only if the resize finishes/pauses for that time."
	DisplayScreen isDisplayExtentOk ifFalse: [
		Smalltalk platformName = 'Win32' ifFalse: [
			self valueOfProperty: #startForNewDisplayTimeout ifAbsentPut: [Time localMillisecondClock].
			self valueOfProperty: #newDisplayOldExtent ifAbsentPut: [DisplayScreen actualScreenSize].
			lastCycleHadAnyEvent ifFalse: [
				(self valueOfProperty: #newDisplayOldExtent) = DisplayScreen actualScreenSize ifFalse: [
					"If user still resizing, restart timer: wait until the user finishes resizing."
					self setProperty: #startForNewDisplayTimeout toValue: Time localMillisecondClock.
					self setProperty: #newDisplayOldExtent toValue: DisplayScreen actualScreenSize].
				Time localMillisecondClock - (self valueOfProperty: #startForNewDisplayTimeout) < 250 ifTrue: [
					^self ]].
			self removeProperty: #startForNewDisplayTimeout.
			self removeProperty: #newDisplayOldExtent ]].

	needsNewUIProcessAndCanvas := false.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"''print. {DateAndTime now. 'Smalltalk Display:'. Display extent. 'os display:'. DisplayScreen actualScreenSize} print."
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := DisplayScreen hostOSWindowExtent ifNotNil: [ :ee |
				(ee * ScaleFactor - DisplayScreen actualScreenSize + Display extent) truncated ].
			p := DisplayScreen hostOSWindowPosition.
			e ifNotNil: [
				"{'New ScaleFactor'. ScaleFactor. 'Set os display to:'. e} print."
				DisplayScreen hostOSWindowExtent: e].
			p ifNotNil: [DisplayScreen hostOSWindowPosition: p].
		].
		"When moving the Cuis window, for instance, between a laptop screen with ScaleFactor=2.0 and an external screen with
		ScaleFactor=1.0, usually the #hostOSWindowExtent: already left us with isDisplayExtentOk, so asking again is in order."
		DisplayScreen isDisplayExtentOk ifFalse: [
			"This allocates new memory for Display and is critical."
			needsNewUIProcessAndCanvas := true.
			"This may help with memory fragmentation issues possibly making new display allocation fail."
			canvas unpinAndFreeMemory.
			DisplayScreen setupDisplay ]].
	(needsNewUIProcessAndCanvas or: [canvas isConsistentWithTarget not])
		ifTrue: [
			self whenUIinSafeState: [ Cursor defaultCursor activateCursor ].
			"The current canvas is not consistent with Display, and therefore can no longer be used. A new one is required.
			Freeeing the memory it uses may help with memory fragmentation issues possibly making new canvas allocation fail.
			(If there is not enough free memory for VectorCanvas and engine, use a BitBltCanvas, see #getMainCanvas)"
			canvas unpinAndFreeMemory.
			"Create a new UI process, including a new canvas consistent with updated Display."
			UISupervisor spawnNewMorphicProcessFor: self createNewCanvas: true.
			"Terminate the current process (the old UI process). This is needed because the old canvas can no longer be used:
			A canvas + vector engine inconsistent with Display will cause invalid memory accesses, crashing the system."
			Processor terminateActive.
			].
]

{ #category : 'update cycle' }
WorldMorph >> checkIfUpdateNeeded [

	self isSubmorphRedrawNeeded ifTrue: [ ^true ].
	damageRecorder updateIsNeeded ifTrue: [^true].
	(activeHand isRedrawNeeded | activeHand isSubmorphRedrawNeeded and: [
		activeHand isDrawnBySoftware]) ifTrue: [^true].
	^false  "display is already up-to-date"
]

{ #category : 'windows' }
WorldMorph >> cleanUpWorldExceptFor: aMorph  [
	
	(SelectionMenu confirm:
'This will remove all windows except the selected one,
and those containing unsaved text.
It will also remove all non-window morphs on the desktop.
Are you sure you want to do this?') ifFalse: [ ^ self ].
	self allNonWindowRelatedSubmorphs do: [ :m |
		m delete ] without: aMorph.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ] without: aMorph.
]

{ #category : 'stepping' }
WorldMorph >> cleanseStepList [
	"Remove morphs from the step list that are not in this World."

	| deletions |
	deletions := OrderedCollection new.
	stepList do: [ :entry |
		entry receiver world == self ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		stepList remove: entry ].

	deletions := OrderedCollection new.
	alarms do: [ :entry |
		((entry receiver is: #Morph) and: [ entry receiver world == self ]) ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		alarms remove: entry ]
]

{ #category : 'events' }
WorldMorph >> click: aMouseButtonEvent localPosition: localEventPosition [

	^self mouseButton2Activity.
]

{ #category : 'windows' }
WorldMorph >> closeUnchangedWindows [
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	(SelectionMenu confirm: 'Do you really want to close all windows
except those with unaccepted edits?') ifFalse: [ ^ self ].
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]
]

{ #category : 'windows' }
WorldMorph >> collapseAll [
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse ].
	self collapseNonWindows.
	self redrawNeeded.
]

{ #category : 'world menu' }
WorldMorph >> collapseNonWindows [
	self allNonWindowRelatedSubmorphs do: [ :m |
		m collapse]
]

{ #category : 'world state' }
WorldMorph >> deleteAllHalos [
	| c |
	c := OrderedCollection new.
	self haloMorphsDo: [ :halo | c add: halo ].
	self removeAllMorphsIn: c
]

{ #category : 'halos and balloon help' }
WorldMorph >> deleteBalloon [
	"Delete any existing balloon help."

	activeHand deleteBalloon
]

{ #category : 'halos and balloon help' }
WorldMorph >> deleteBalloonTarget: aMorph [
	"Delete the balloon help targeting the given morph"
	activeHand deleteBalloonTarget: aMorph.
]

{ #category : 'world menu' }
WorldMorph >> deleteNonWindows [
	(SelectionMenu confirm:
'Do you really want to discard all objects
that are not in windows?')
		ifFalse: [^ self].

	self allNonWindowRelatedSubmorphs do: [:m |
		m delete ]
]

{ #category : 'drawing' }
WorldMorph >> displayWorldOn: aMorphicCanvas [
	"Update this world's display on the argument."

	| allDamageRect |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"Restore world drawing under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		activeHand restoreSavedPatchOn: aMorphicCanvas ].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	aMorphicCanvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamageRect := aMorphicCanvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	self submorphNeedsRedraw: false.
	self needsRedraw: false.
	aMorphicCanvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self ifHandToBeDrawnForDamage: allDamageRect do: [
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamageRect := (activeHand savePatchFrom: aMorphicCanvas) quickMerge: allDamageRect ]
			ifFalse: [ allDamageRect := (activeHand displayFullBoundsForPatchIn: aMorphicCanvas) quickMerge: allDamageRect ].
		aMorphicCanvas fullDrawHand: activeHand.
		activeHand needsRedraw: false.
		activeHand submorphNeedsRedraw: false ].

	"Copy altered rects to Display"
	allDamageRect ifNotNil: [
		DisplayScreen forceDeferredToScreen: allDamageRect ].
]

{ #category : 'update cycle' }
WorldMorph >> doOneCycleNow [
	"Plase don't remove this method. It is used, for instance, for custom image setup shell scripts."
	self doOneCycleNowOn: canvas
]

{ #category : 'update cycle' }
WorldMorph >> doOneCycleNowOn: aMorphicCanvas [
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| thisCycleTime hadAny |
	thisCycleTime := Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldOn: aMorphicCanvas.
	"Run steps, alarms and deferred UI messages."
	self runDeferredUIMessages.
	self triggerAlarmsBefore: thisCycleTime.
	self runLocalStepMethods: thisCycleTime.
	"Ensure that any affected morph is redrawn ASAP."
	self displayWorldOn: aMorphicCanvas.
	"Process user input events. Run all event triggered code."
	hadAny := activeHand processEventQueue.
	"Run any deferred UI messages that were enqueued during this cycle, perhaps
	as consequence of event handling or step methods, or sent from other processes,
	before the inter cycle pause."
	self runDeferredUIMessages.
	"Ensure that any affected morph is redrawn ASAP, before the inter cycle pause."
	self displayWorldOn: aMorphicCanvas.
	lastCycleTime := thisCycleTime.
	lastCycleHadAnyEvent := hadAny.
]

{ #category : 'update cycle' }
WorldMorph >> doOneCycleOn: aMorphicCanvas delay: delay [
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta := Time localMillisecondClock - lastCycleTime.
	r := 15@1515 extent: 60@30.
	Delta := Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	self doOneCycleNowOn: aMorphicCanvas.

	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause := 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait := 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			activeHand waitingForMoreClicks
				ifTrue: [ pause := 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause := pause * 21//20 ] ].
			waitUntil := lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil := waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil := waitUntil min: alarms first scheduledTime ]].
			wait := waitUntil - Time localMillisecondClock max: 0 ].
	(Preferences at: #serverMode)
		ifTrue: [ wait := wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			delay setDelay: wait; wait ].
]

{ #category : 'update cycle' }
WorldMorph >> doOneMinimalCycleNow [
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorldOn: self mainCanvas.

	"Process user input events. Run all event triggered code."
	^self activeHand processEventQueue
]

{ #category : 'drawing' }
WorldMorph >> drawOn: aCanvas [

	"draw background image."
	backgroundImage
		ifNotNil: [
			super drawOn: aCanvas ]
		ifNil: [
			"draw background fill"
			(aCanvas form == Display and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: `0@0` in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]
]

{ #category : 'dropping/grabbing' }
WorldMorph >> dropFiles: aDropFilesEvent [

	(DropFilesAction for: aDropFilesEvent) value
]

{ #category : 'geometry' }
WorldMorph >> extentChanged: oldExtent [
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	taskbar ifNotNil: [ taskbar screenSizeChanged ].
]

{ #category : 'geometry' }
WorldMorph >> externalizeBoundsToWorld: aRectangle [
	"aRectangle is in own coordinates. Answer is in world coordinates."

	^ aRectangle
]

{ #category : 'geometry' }
WorldMorph >> externalizeDistanceToWorld: aPoint [
	"aPoint is a delta in own coordinates. Answer is in world coordinates."

	^ aPoint
]

{ #category : 'geometry' }
WorldMorph >> externalizeToWorld: aPoint [
	"aPoint is in own coordinates. Answer is in world coordinates."

	^ aPoint
]

{ #category : 'misc' }
WorldMorph >> fillRects: rectangleList [
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt := (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: `0@0`;
		clipRect: self viewBox;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect := r translatedBy: self viewBox origin.
		blt fillColor: Color random.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 50) wait
]

{ #category : 'world menu' }
WorldMorph >> findAChangeSorter: evt [
	"Locate a change sorter, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model isMemberOf: ChangeSorter]
		orMakeOneUsing: [ ChangeSorterWindow open: ChangeSorter new label: nil ]
]

{ #category : 'world menu' }
WorldMorph >> findAFileList: evt [
	"Locate a file list, open it, and bring it to the front.
	Create one if necessary, respecting the Preference."
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == FileList ]
		orMakeOneUsing: [
			FileList open ]
]

{ #category : 'world menu' }
WorldMorph >> findAMessageNamesWindow: evt [
	"Locate a MessageNames tool, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == MessageNames ]
		orMakeOneUsing: [
			MessageNamesWindow open: MessageNames new label: 'Message Names' ]
]

{ #category : 'windows' }
WorldMorph >> findATranscript [
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self
		findAWindowSatisfying: [ :aWindow | aWindow model == Transcript]
		orMakeOneUsing: [ Transcript open ]
]

{ #category : 'world menu' }
WorldMorph >> findATranscript: evt [
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self findATranscript
]

{ #category : 'windows' }
WorldMorph >> findAWindowSatisfying: qualifyingBlock orMakeOneUsing: makeBlock [
	"Locate a window satisfying a block, open it, and bring it to the front.  Create one if necessary, by using the makeBlock"
	| aWindow |
	submorphs do: [ :aMorph |
		(((aWindow := aMorph) is: #SystemWindow) and: [ qualifyingBlock value: aWindow ]) ifTrue: [
			aWindow isCollapsed ifTrue: [ aWindow expand ].
			aWindow activateAndForceLabelToShow.
			^ aWindow ]].
	"None found, so create one"
	^ makeBlock value.
]

{ #category : 'world menu' }
WorldMorph >> findDirtyBrowsers: evt [
	"Present a menu of window titles for browsers with changes,
	and activate the one that gets chosen."
	| menu |
	menu := MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [
				(w model is: #CodeProvider) and: [ w canDiscardEdits not ]]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]
]

{ #category : 'world menu' }
WorldMorph >> findDirtyWindows: evt [
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	| menu |
	menu := MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits not ]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]
]

{ #category : 'world menu' }
WorldMorph >> findWindow: evt [
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu := MenuMorph new.
	expanded := SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ].
	collapsed := SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ].
	nakedMorphs := self submorphsSatisfying: [ :m |
		(m is: #SystemWindow) not ].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) ifTrue: [ ^ Smalltalk beep ].
	(expanded asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activateAndForceLabelToShow.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) ifFalse: [ menu addLine ].
	(collapsed asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #expand.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	nakedMorphs isEmpty ifFalse: [ menu addLine ].
	(nakedMorphs asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #comeToFrontAndAddHalo ].
	menu addTitle: 'find window'.
	menu popUpInWorld: self
]

{ #category : 'world state' }
WorldMorph >> fullRepaintNeeded [
	self redrawNeeded.
	SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ifTrue: [ w makeMeVisible ].
			false ]
]

{ #category : 'world menu' }
WorldMorph >> getMenu [
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed (by answering nil)"

	^(Preferences at: #worldMenu) getMenuIn: self at: self activeHand.
]

{ #category : 'world state' }
WorldMorph >> haloMorphsDo: aBlock [
	activeHand halo ifNotNil: [ :halo | aBlock value: halo ].
]

{ #category : 'taskbar' }
WorldMorph >> hideTaskbar [
	taskbar ifNotNil: [
		taskbar delete.
		taskbar := nil ]
]

{ #category : 'hands' }
WorldMorph >> ifHandToBeDrawnForDamage: aRectangle do: aBlock [
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	activeHand isDrawnBySoftware ifTrue: [
		activeHand isRedrawNeeded
			ifTrue: aBlock
			ifFalse: [
				aRectangle ifNotNil: [
					activeHand displayFullBounds ifNotNil: [ :handBounds |
						(aRectangle intersects: handBounds) ifTrue: aBlock ]]]].
]

{ #category : 'initialization' }
WorldMorph >> initSortBlocks [

	stepList := Heap sortBlock: self stepListSortBlock.
	alarms := Heap sortBlock: self alarmSortBlock.
]

{ #category : 'initialization' }
WorldMorph >> initialize [

	super initialize.
	activeHand := HandMorph new.
	damageRecorder := DamageRecorder new.
	stepList := Heap sortBlock: self stepListSortBlock.
	alarms := Heap sortBlock: self alarmSortBlock.
	lastAlarmTime := 0.
	deferredUIMessages := SharedQueue new.
	drawingFailingMorphs := WeakIdentitySet new.
	pause := 20.
	lastCycleTime := Time localMillisecondClock.
	lastCycleHadAnyEvent := false
]

{ #category : 'geometry' }
WorldMorph >> internalizeDistanceFromWorld: aPoint [
	"aPoint is a delta in World coordinates. Answer is in own coordinates."

	^ aPoint
]

{ #category : 'geometry' }
WorldMorph >> internalizeFromWorld: aPoint [
	"aPoint is in World coordinates. Answer is in own coordinates."

	^ aPoint
]

{ #category : 'change reporting' }
WorldMorph >> invalidateDisplayRect: damageRect for: aMorph [
        "Clip damage reports to my bounds, since drawing is _always_ clipped to my bounds."

	self recordDamagedRect: (damageRect intersect: self localBounds ) for: aMorph
]

{ #category : 'errors on draw' }
WorldMorph >> isKnownFailing: aMorph [
	^drawingFailingMorphs includes: aMorph
]

{ #category : 'testing' }
WorldMorph >> isReallyUnlocked [
	"Answer true only if none in the owner chain is locked (i.e. if we are really unlocked!)"
	^self isLocked not
]

{ #category : 'testing' }
WorldMorph >> isReallyVisible [
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!)"
	^self visible
]

{ #category : 'classification' }
WorldMorph >> isWorldMorph [

	^ true
]

{ #category : 'canvas' }
WorldMorph >> mainCanvas [
	"Answer the main canvas that was last set"

	^ canvas
]

{ #category : 'canvas' }
WorldMorph >> mainCanvas: aMorphicCanvas [
	"Set the main canvas"

	canvas == aMorphicCanvas ifFalse: [
		(Preferences at: #saveReleaseOptionsInWorldMenu) ifFalse: [
			aMorphicCanvas pinArrays ].
		canvas := aMorphicCanvas.
	].
	damageRecorder := DamageRecorder new.
	damageRecorder recordInvalidRect: self localBounds for: nil.
	waitDelay := Delay forMilliseconds: 50.
]

{ #category : 'interaction loop' }
WorldMorph >> mainLoop [

	[
		self displayWorldOn: canvas.
		[
			"Here canvas and waitDelay are passed as arguments.
			This is because when debugging Morphic, a new UI process,
			these ivars are updated, but in the debugger process we still want the original ones."
			self doOneCycleOn: canvas delay: waitDelay.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				#(drawOn: postDrawOn: fullDraw: step stepAt:) statePointsTo: context selector ])
			ifNotNil: [ :context | | failingMorph selector |
				failingMorph := context selector = #fullDraw:
					ifTrue: [ (context at: 1) ]
					ifFalse: [ context receiver ].
				selector := context selector.
				(failingMorph is: #Morph) ifTrue: [
					(#(drawOn: postDrawOn: fullDraw:) statePointsTo: selector)
						ifTrue: [ failingMorph drawingFails ].
					(#(step stepAt:) statePointsTo: selector)
						ifTrue: [ failingMorph stopStepping ]].
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger.
				This is to use a new, clean Canvas."
				UISupervisor newUIProcess ]
			ifNil: [
				"Debugging aid: Uncomment. Set Transcript to log to StdOut and File.
				Be ready to need to kill Cuis from the OS."
				"'-----' print.
				{Time now. Processor activeProcess. ex} print.
				thisContext printStack: 50.
				[UISupervisor newUIProcess] fork."
				"ex return"
				ex pass ]
	].
]

{ #category : 'submorphs-menus' }
WorldMorph >> menuNshortcutsSpecsDo: aBlock [
	"Iterate over our own menu and shortcut specs.
	Specific implementation in Worlds."

	self worldMenuSpec do: aBlock.
]

{ #category : 'interaction loop' }
WorldMorph >> millisecondsSinceLastCycle [
	"
	self runningWorld millisecondsSinceLastCycle
	"
	^Time localMillisecondClock - lastCycleTime
]

{ #category : 'object serialization' }
WorldMorph >> objectForDataStream: refStrm [
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp := DiskProxy global: #Smalltalk selector: #runningWorld args: #().
	refStrm replace: self with: dp.
	^ dp
]

{ #category : 'geometry' }
WorldMorph >> positionInWorld [
	"Answer is in world coordinates."

	^ `0@0`
]

{ #category : 'printing' }
WorldMorph >> printOn: aStream [
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	aStream nextPutAll: ' [world]'
]

{ #category : 'canvas' }
WorldMorph >> recordDamagedRect: damageRect for: aMorph [

	damageRect hasPositiveExtent ifFalse: [ ^self ].

	damageRecorder ifNotNil: [
		damageRecorder recordInvalidRect: damageRect for: aMorph ].
]

{ #category : 'defaul desktop' }
WorldMorph >> recreateDefaultDesktop [
	"
	self runningWorld recreateDefaultDesktop
	"
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		Theme current useTaskbar ifTrue: [self showTaskbar].
		Transcript clear.
		Transcript open
			resizeBottomLeft
	].
]

{ #category : 'caching' }
WorldMorph >> releaseCachedState [
	super releaseCachedState.
	self cleanseStepList.
	canvas ifNotNil: [
		canvas unpinAndFreeMemory ].
	canvas := nil.
	damageRecorder := nil.
	activeHand releaseCachedState.
]

{ #category : 'alarms' }
WorldMorph >> removeAlarm: aSelector for: aTarget  [
	"Remove the alarm with the given selector"

	| alarm |
	alarm := alarms 
				detect: [ :any | any receiver == aTarget and: [any selector == aSelector]]
				ifNone: nil.
	alarm ifNotNil: [ alarms remove: alarm ]
]

{ #category : 'errors on draw' }
WorldMorph >> removeAllKnownFailing [
	drawingFailingMorphs do: [ :m | m redrawNeeded ].
	drawingFailingMorphs := WeakIdentitySet new.

]

{ #category : 'errors on draw' }
WorldMorph >> removeKnownFailing: aMorph [
	drawingFailingMorphs remove: aMorph
]

{ #category : 'change reporting' }
WorldMorph >> removedMorph: aMorph [
	"Notify the receiver that aMorph was just removed from its children"
	super removedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasDeleted: aMorph ]
]

{ #category : 'ui services' }
WorldMorph >> request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock [
	"Modal. This method returns when an answer is provided."

	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
]

{ #category : 'ui services' }
WorldMorph >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock [
	"Modal. This method returns when an answer is provided."

	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock
]

{ #category : 'ui services' }
WorldMorph >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock [
	"Non-Modal. This method returns immediately.
	Either acceptBlock or cancelBlock will be executed later, when the user provides an answer."

	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
]

{ #category : 'ui services' }
WorldMorph >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock [
	"Modal. This method returns when an answer is provided."

	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock
]

{ #category : 'world menu' }
WorldMorph >> restoreAll [
	"Expand all windows to their previous state"
	taskbar
		ifNotNil: [ :tb | tb restoreAll ]
]

{ #category : 'misc' }
WorldMorph >> restoreDisplay [
	self morphExtent: Display extent.
	activeHand visible: true.
	self fullRepaintNeeded.
]

{ #category : 'stepping' }
WorldMorph >> runDeferredUIMessages [
	"Processing the queue until empty is wrong if a block in it calls #addDeferredUIMessage: itself, because this loop will never end.
	Instead, process no more than entries already in queue befor we start iterating!"
	| readyToProcess |
	readyToProcess := deferredUIMessages size.
	readyToProcess timesRepeat: [
		UISupervisor isUIProcessRunning ifFalse: [ ^self ].
		deferredUIMessages nextOrNil ifNotNil: [ :block |
			block value ]].
]

{ #category : 'stepping' }
WorldMorph >> runLocalStepMethods: nowTime [
	"Run morph 'step' methods whose time has come. Purge any morphs that are no longer in this world."

	| timerBefore timerAfter stepMessage |
	timerAfter := nowTime.
	[ stepList notEmpty and: [ (stepMessage := stepList first) scheduledTime <= nowTime ]] 
		whileTrue: [
			(stepMessage receiver shouldGetStepsFrom: self)
				ifFalse: [
					stepList remove: stepMessage ifAbsent: nil]
				ifTrue: [
					timerBefore := timerAfter.
					stepMessage valueAtTime: nowTime.
					timerAfter := Time localMillisecondClock.
					"No step message should take more than 50% CPU. 
					Reschedule it only after its required time is elapsed again."
					stepMessage rescheduleAfter: timerAfter + (timerAfter - timerBefore).
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort ]].
]

{ #category : 'interaction loop' }
WorldMorph >> runProcess: createNewCanvas [
	
	^[
		createNewCanvas ifTrue: [
			self position: `0@0` extent: Display extent.
			"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."
			self setupCanvas.
			self restoreDisplay ].
		"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
		Might happen, especially on RasPi or other systems without an RTC"
		lastCycleTime := Time localMillisecondClock.
		lastAlarmTime := 0.
		self mainLoop
	] newProcess.
]

{ #category : 'canvas' }
WorldMorph >> setupCanvas [
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."

	| newCanvas |
	newCanvas := [ MorphicCanvas onDisplayToReplace: canvas ]
				on: OutOfMemory
				do: [
					"In case of lack of memory, use a much cheaper (but limited) BitBltCanvas."
					'Not enough memory to run VectorEngine. Using BitBltCanvas instead.' print.
					MorphicCanvas activeSubclass: BitBltCanvas.
					MorphicCanvas onDisplay ].
	self mainCanvas: newCanvas.
	canvas world: self.
]

{ #category : 'canvas' }
WorldMorph >> setupNewCanvasAndEngine [
	canvas := nil.
	self setupCanvas.
	self whenUIinSafeState: [self whenUIinSafeState: [self restoreDisplay]].
]

{ #category : 'taskbar' }
WorldMorph >> showTaskbar [

	taskbar ifNil: [
		taskbar := TaskbarMorph newRow.
		self addMorph: taskbar ]
]

{ #category : 'submorphs-menus' }
WorldMorph >> smalltalk [
	"For the benefit of worldMenuSpec"

	^Smalltalk
]

{ #category : 'stepping' }
WorldMorph >> startStepping: aMorph at: scheduledTime selector: aSelector stepTime: timeIntervalMillisecondsOrNil [
	"Add the given morph to the step list"

	self stopStepping: aMorph selector: aSelector.
	stepList add: (
		StepMessage 
			receiver: aMorph
			selector: aSelector
			at: scheduledTime
			stepTime: timeIntervalMillisecondsOrNil)
]

{ #category : 'initialization' }
WorldMorph >> stepListSortBlock [

	^ [ :stepMsg1 :stepMsg2 |
		stepMsg1 scheduledTime <= stepMsg2 scheduledTime ]
]

{ #category : 'stepping' }
WorldMorph >> stopStepping: aMorph selector: aSelector [
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and: [ stepMsg selector == aSelector ]])
]

{ #category : 'stepping' }
WorldMorph >> stopSteppingMorph: aMorph [
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select: [ :stepMsg | stepMsg receiver == aMorph])
]

{ #category : 'taskbar' }
WorldMorph >> taskbar [
	^taskbar
]

{ #category : 'taskbar' }
WorldMorph >> taskbarDeleted [
	taskbar := nil
]

{ #category : 'defaul desktop' }
WorldMorph >> tearDownDesktop [
	self hideTaskbar.
	submorphs 
		do: [ :ea | (ea is: #SystemWindow) ifTrue: [ ea delete ]].
]

{ #category : 'submorphs-menus' }
WorldMorph >> theme [
	"For the benefit of worldMenuSpec"

	^Theme
]

{ #category : 'structure' }
WorldMorph >> topmostWorld [
	"Answer the last morph in the owner chain (i.e. the morph without owner) if it is a WorldMorph, or nil."
	owner
		ifNotNil: [ ^owner topmostWorld ].
	^self
]

{ #category : 'alarms' }
WorldMorph >> triggerAlarmsBefore: nowTime [
	"Trigger all pending alarms that are to be executed before nowTime."

	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	[ alarms notEmpty and: [ alarms first scheduledTime < nowTime ]]
		whileTrue: [ alarms removeFirst valueAtTime: nowTime ].
	lastAlarmTime := nowTime
]

{ #category : 'geometry' }
WorldMorph >> viewBox [

	^ self localBounds
]

{ #category : 'halos and balloon help' }
WorldMorph >> wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph [
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	^#(addDebugHandle: addMenuHandle: addHelpHandle:)
		statePointsTo: aSelector
]

{ #category : 'event handling' }
WorldMorph >> wantsWindowEvent: anEvent [
	^true
]

{ #category : 'update cycle' }
WorldMorph >> whenUIinSafeState: evaluableObject [
	"Please call
	UISupervisor whenUIinSafeState: evaluableObject
	"
	deferredUIMessages nextPut: evaluableObject
]

{ #category : 'events' }
WorldMorph >> windowEvent: aWindowEvent [

	aWindowEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]

]

{ #category : 'structure' }
WorldMorph >> world [

	^ self
]

{ #category : 'submorphs-menus' }
WorldMorph >> worldMenuSpec [
	"Spec for the menu and shortcut keys for the World.
	See coment at Morph >> #getMenu
	"
	^
	#(
		"Keyboard shortcut only. No menu entry."
		#(991 	nil 	$b 	smalltalk 	browse)
		#(992 	nil 	$f 	smalltalk 	findAndBrowseClass)
		#(993 	nil 	$F 	smalltalk 	findInSourceCode)
		#(994 	nil 	$+ 	theme   	largerDefaultFont)
		#(995 	nil 	$= 	theme   	largerDefaultFont) "See comment at #isCmdAltGreaterThan. This hack is needed for the shortcut to work on Mac."
		#(996 	nil 	$- 	theme   	smallerDefaultFont)
		#(997 	nil 	$/ 	theme 	setExpandAllWindows)
		#(998 	nil 	$* 	theme 	setDefaultSizeAllWindows)
	)
]
