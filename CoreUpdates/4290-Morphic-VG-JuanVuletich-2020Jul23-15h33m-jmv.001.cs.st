'From Cuis 5.0 [latest update: #4286] on 23 July 2020 at 3:34:20 pm'!

!PasteUpMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/23/2020 14:47:46'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphExtent.
	delta _ trialRect amountToTranslateWithin: self displayBounds.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: trialRect origin + delta.! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 7/23/2020 15:25:53'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| trialRect e |
	popUpOwner _ sourceItem.
	sourceItem world addMorphFront: self position: rightOrLeftPointInWorld first.
	e _ self morphExtent.
	trialRect _ rightOrLeftPointInWorld first extent: e.
	trialRect right > sourceItem world morphWidth ifTrue: [
		self morphPosition: rightOrLeftPointInWorld second - (e x@0)].
	self fitInWorld.! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 7/23/2020 15:01:17'!
fitInWorld

	| delta trialRect |
	trialRect _ Rectangle origin: self morphPosition extent: self morphExtent.
	delta _ trialRect amountToTranslateWithin: owner displayBounds.
	self morphPosition: trialRect origin + delta.! !


!PasteUpMorph reorganize!
('accessing' activeHand color: handlesKeyboard)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:from: redrawNeeded removedMorph:)
('classification' isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: keyStroke: mouseButton1Down:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' mouseButton2Activity wantsWindowEvent: windowEventHandler)
('geometry' displayBounds externalizeDisplayBounds: externalizeToWorld: fontPreferenceChanged internalizeFromWorld: morphPositionInWorld privateExtent:)
('initialization' clearCanvas clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor)
('interaction loop' doOneCycleNow mainLoop runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: setCanvas: viewBox)
('stepping' startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('stepping and presenter' wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addMorph:centeredNear:)
('testing' is: isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos displayWorld displayWorldSafely doOneCycle doOneMinimalCycleNow fillRects: fullRepaintNeeded haloMorphs privateOuterDisplayWorld restoreDisplay whenUIinSafeState: worldState:)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
('object serialization' objectForDataStream:)
('windows' findATranscript)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('defaul desktop' recreateDefaultDesktop tearDownDesktop)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying:do:orCancel:)
!

