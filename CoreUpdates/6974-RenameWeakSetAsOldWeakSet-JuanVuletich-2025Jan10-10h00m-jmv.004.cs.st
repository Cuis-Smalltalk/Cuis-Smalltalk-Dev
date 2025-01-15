'From Cuis7.3 [latest update: #6973] on 10 January 2025 at 10:02:05 am'!
Smalltalk renameClassNamed: #WeakSet as: #OldWeakSet!
Smalltalk renameClassNamed: #WeakIdentitySet as: #OldWeakIdentitySet!

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 1/10/2025 10:01:18'!
removeAllKnownFailing
	drawingFailingMorphs do: [ :m | m redrawNeeded ].
	drawingFailingMorphs := OldWeakIdentitySet new.
! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 1/10/2025 10:01:18'!
initialize

	super initialize.
	activeHand := HandMorph new.
	damageRecorder := DamageRecorder new.
	stepList := Heap sortBlock: self stepListSortBlock.
	alarms := Heap sortBlock: self alarmSortBlock.
	lastAlarmTime := 0.
	deferredUIMessages := SharedQueue new.
	drawingFailingMorphs := OldWeakIdentitySet new.
	pause := 20.
	lastCycleTime := Time localMillisecondClock.
	lastCycleHadAnyEvent := false! !


!WorldMorph reorganize!
('accessing' handlesKeyboard)
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('caching' releaseCachedState)
('canvas' mainCanvas mainCanvas: recordDamagedRect:for: setupCanvas)
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
('initialization' initialize stepListSortBlock)
('interaction loop' mainLoop millisecondsSinceLastCycle runProcess:)
('menu & halo' addWorldHaloMenuItemsTo:hand:)
('misc' fillRects: restoreDisplay)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' cleanseStepList runDeferredUIMessages runLocalStepMethods: startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('structure' topmostWorld world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addMorph:centeredNear:)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('testing' isReallyVisible)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('update cycle' checkForNewScreenSize checkIfUpdateNeeded doOneCycleNow doOneCycleNowOn: doOneCycleOn:delay: doOneMinimalCycleNow whenUIinSafeState:)
('windows' bringWindowsFullOnscreen cleanUpWorldExceptFor: closeUnchangedWindows collapseAll findATranscript findAWindowSatisfying:orMakeOneUsing:)
('world menu' collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findDirtyBrowsers: findDirtyWindows: findWindow: getMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos fullRepaintNeeded haloMorphsDo:)
!

