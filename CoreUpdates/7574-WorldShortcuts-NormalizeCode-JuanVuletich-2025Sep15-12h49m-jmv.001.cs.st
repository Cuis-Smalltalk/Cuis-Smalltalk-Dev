'From Cuis7.5 [latest update: #7570] on 15 September 2025 at 1:11:56 pm'!

!SystemDictionary methodsFor: 'browsing - main' stamp: 'jmv 9/15/2025 13:09:48'!
openClassFinder
	"
	Smalltalk openClassFinder.
	"
	(Preferences at: #classFinder) value.! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 9/15/2025 13:10:43'!
keyStroke: aKeyboardEvent

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	"Handle a keystroke event."
	(aKeyboardEvent shiftPressed and: [ aKeyboardEvent isReturnKey ])
		ifTrue: [
			Smalltalk openClassFinder.
			aKeyboardEvent wasHandled: true ].! !

!WorldMorph methodsFor: 'submorphs-menus' stamp: 'jmv 9/15/2025 12:59:25'!
menuNshortcutsSpecsDo: aBlock
	"Iterate over our own menu and shortcut specs.
	Specific implementation in Worlds."

	self worldMenuSpec do: aBlock.! !

!WorldMorph methodsFor: 'submorphs-menus' stamp: 'jmv 9/15/2025 13:03:04'!
smalltalk
	"For the benefit of worldMenuSpec"

	^Smalltalk! !

!WorldMorph methodsFor: 'submorphs-menus' stamp: 'jmv 9/15/2025 13:02:28'!
worldMenuSpec
	"Spec for the menu and shortcut keys for the World.
	See coment at Morph >> #getMenu
	"
	^
	#(
		"Keyboard shortcut only. No menu entry."
		#(991 	nil 	$b 	smalltalk 	browse)
		#(992 	nil 	$f 	smalltalk 	findAndBrowseClass)
		#(993 	nil 	$F 	smalltalk 	findInSourceCode)
	)! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/15/2025 13:07:49'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	keyEventType == #keystroke ifTrue: [
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].! !

!methodRemoval: KeyboardEvent #isFindClassShortcut stamp: 'jmv 9/15/2025 13:10:39'!
KeyboardEvent removeSelector: #isFindClassShortcut!

!WorldMorph reorganize!
('accessing')
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('caching' releaseCachedState)
('canvas' mainCanvas mainCanvas: recordDamagedRect:for: setupCanvas setupNewCanvasAndEngine)
('change reporting' addedMorph: invalidateDisplayRect:for: removedMorph:)
('classification' isWorldMorph)
('defaul desktop' recreateDefaultDesktop tearDownDesktop)
('drawing' displayWorldOn: drawOn:)
('dropping/grabbing' allowsFilesDrop dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('event handling' wantsWindowEvent:)
('events' click:localPosition: keyStroke: windowEvent:)
('geometry' basicDisplayBounds extentChanged: externalizeBoundsToWorld: externalizeDistanceToWorld: externalizeToWorld: internalizeDistanceFromWorld: internalizeFromWorld: positionInWorld viewBox)
('halos and balloon help' deleteBalloonTarget: wantsHaloHandleWithSelector:inHalo:)
('hands' activeHand ifHandToBeDrawnForDamage:do:)
('initialization' initSortBlocks initialize stepListSortBlock)
('interaction loop' mainLoop millisecondsSinceLastCycle runProcess:)
('menu & halo' addWorldHaloMenuItemsTo:hand:)
('misc' fillRects: restoreDisplay)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' cleanseStepList runDeferredUIMessages runLocalStepMethods: startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('structure' topmostWorld world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addMorph:centeredNear: center:near:)
('submorphs-menus' menuNshortcutsSpecsDo: smalltalk worldMenuSpec)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('testing' isReallyUnlocked isReallyVisible)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('update cycle' checkForNewScreenSize checkIfUpdateNeeded doOneCycleNow doOneCycleNowOn: doOneCycleOn:delay: doOneMinimalCycleNow whenUIinSafeState:)
('windows' bringWindowsFullOnscreen cleanUpWorldExceptFor: closeUnchangedWindows collapseAll findATranscript findAWindowSatisfying:orMakeOneUsing:)
('world menu' collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findDirtyBrowsers: findDirtyWindows: findWindow: getMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos fullRepaintNeeded haloMorphsDo:)
!

