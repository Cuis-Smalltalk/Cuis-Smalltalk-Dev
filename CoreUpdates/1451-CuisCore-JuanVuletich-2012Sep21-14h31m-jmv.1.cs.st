'From Cuis 4.0 of 21 April 2012 [latest update: #1450] on 21 September 2012 at 2:46:24 pm'!

!PasteUpMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/21/2012 14:37'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	self isWorldMorph ifFalse: [
		^super wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph ].

	^#(addDebugHandle: addMenuHandle: addHelpHandle:)
		statePointsTo: aSelector! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/21/2012 14:40'!
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	aHaloMorph haloBox: box.
	Preferences haloSpecifications do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/21/2012 14:39'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	Preferences selectiveHalos ifFalse: [
		^true ].

	(#(#addDismissHandle: ) includes: aSelector)
		ifTrue: [ ^ self resistsRemoval not ].
	(#(#addDragHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToBrownDragEasily ].
	(#(#addGrowHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToResizeEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 9/21/2012 14:44'!
addHandles
	| box |
	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target worldBoundsForHalo.  "update my size"
	box _ self basicBox.

	target addHandlesTo: self box: box.

	self addNameBeneath: self basicBox string: (target printStringLimitedTo: 40).
	growingOrRotating _ false.
	self redrawNeeded! !

!methodRemoval: Preferences class #haloSpecificationsForWorld!
Preferences class removeSelector: #haloSpecificationsForWorld!

!PasteUpMorph reorganize!
('WiW support' shouldGetStepsFrom:)
('accessing' color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' invalidateRect: redrawNeeded)
('classification' isPlayfieldLike isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: dropEnabled morphToDropFrom: repelsMorph:event: wantsDroppedMorph:event:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: mouseDown:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' morphToGrab: mouseButton2Activity wantsWindowEvent: windowEventHandler)
('events-processing' dispatchEvent:localPosition:)
('geometry' externalizeToWorld: internalizeFromWorld: morphExtent: morphPositionInWorld)
('initialization' clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor initialize)
('interaction loop' doOneCycleNow)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: addWorldToggleItemsToHaloMenu: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: listOfSteppingMorphs stepListSize steppingMorphsNotInWorld viewBox viewBox:)
('stepping' cleanseStepList runLocalStepMethods runStepMethods startStepping: startStepping:at:selector:arguments:stepTime: stopStepping: stopStepping:selector:)
('stepping and presenter' step wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addAllMorphs: addMorphFront:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows expandAll findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu openRecentSubmissionsBrowser:)
('world state' addMorph:centeredNear: allNonFlapRelatedSubmorphs assuredNonDisplayCanvas deleteAllHalos displayWorld displayWorldSafely doOneCycle doOneSubCycle flashRects: fullRepaintNeeded haloMorphs install privateOuterDisplayWorld restoreMorphicDisplay startSteppingSubmorphsOf:)
('private')
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
!

!methodRemoval: HaloMorph #addHandlesForWorldHalos!
HaloMorph removeSelector: #addHandlesForWorldHalos!
!methodRemoval: HaloMorph #addName!
HaloMorph removeSelector: #addName!
!methodRemoval: Morph #addWorldHandlesTo:box:!
Morph removeSelector: #addWorldHandlesTo:box:!
