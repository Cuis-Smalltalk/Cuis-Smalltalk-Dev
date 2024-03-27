'From Cuis6.3 [latest update: #6302] on 27 March 2024 at 10:42:25 am'!

!WorldMorph methodsFor: 'windows' stamp: 'PB 3/27/2024 10:39:30'!
cleanUpWorldExceptFor: aMorph 
	
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
		w delete ] without: aMorph.! !


!SystemWindow methodsFor: 'open/close' stamp: 'PB 3/27/2024 10:39:32'!
closeOtherWindows
	self world cleanUpWorldExceptFor: self.! !


!SystemWindow methodsFor: 'menu' stamp: 'PB 3/27/2024 09:21:30'!
buildWindowMenu

	| aMenu |

	aMenu := MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' 			action: #relabel 						icon: #saveAsIcon;
		add: 'window color...' 			action: #setWindowColor 			icon: #graphicsIcon;
		add: 'close others...' 			action: #closeOtherWindows 			icon: #closeIcon.
	
	self hasSaveAs
		ifTrue: [ aMenu add: 'Save as ...' action: #saveContents icon: #saveAsIcon ].
		
	aMenu
		addLine.
		
	self addWindowControlTo: aMenu.
	self addTileResizerMenuTo: aMenu.

	^ aMenu! !


!WorldMorph reorganize!
('accessing' handlesKeyboard)
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('caching' releaseCachedState)
('canvas' mainCanvas recordDamagedRect:for:)
('change reporting' addedMorph: invalidateDisplayRect:for: removedMorph:)
('classification' isWorldMorph)
('defaul desktop' recreateDefaultDesktop tearDownDesktop)
('drawing' displayWorldOn: drawOn:)
('dropping/grabbing' allowsFilesDrop dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('event handling' mouseButton2Activity wantsWindowEvent:)
('events' click:localPosition: keyStroke: windowEvent:)
('geometry' basicDisplayBounds extentChanged: externalizeBoundsToWorld: externalizeDistanceToWorld: externalizeToWorld: internalizeDistanceFromWorld: internalizeFromWorld: morphPositionInWorld viewBox)
('halos and balloon help' deleteBalloonTarget: wantsHaloHandleWithSelector:inHalo:)
('hands' activeHand hands handsDo: handsReverseDo: handsToDrawForDamage:do:)
('initialization' initialize stepListSortBlock)
('interaction loop' mainLoop millisecondsSinceLastCycle runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand:)
('misc' fillRects: restoreDisplay)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' cleanseStepList runDeferredUIMessages runLocalStepMethods: startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('structure' topmostWorld world)
('submorphs-accessing' allMorphsDo: firstHand)
('submorphs-add/remove' addMorph:centeredNear:)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('testing' isReallyVisible)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying:do:orCancel:)
('update cycle' checkForNewScreenSize checkIfUpdateNeeded doOneCycleNow doOneCycleNowOn: doOneCycleOn:delay: doOneMinimalCycleNow whenUIinSafeState:)
('windows' bringWindowsFullOnscreen cleanUpWorldExceptFor: closeUnchangedWindows collapseAll findATranscript findAWindowSatisfying:orMakeOneUsing:)
('world menu' collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos fullRepaintNeeded haloMorphsDo:)
!

