'From Cuis 4.0 of 21 April 2012 [latest update: #1486] on 25 November 2012 at 12:09:06 am'!

!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 11/25/2012 00:08'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| globalPt aMorphHandlesIt handledByInner lastHandler answer eventPositionInChild |

	"Only for MouseDown"
	self isMouseDown
		ifFalse: [ ^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	globalPt _ self eventPosition.
	(aMorph morphFullBoundsInWorld containsPoint: globalPt) ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	lastHandler _ eventHandler.
	aMorphHandlesIt _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self)
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue:[
			"aMorph is in the top-most unlocked, visible morph in the chain."
			aMorphHandlesIt ifTrue: [ self sentTo: aMorph localPosition: positionInAMorph ].
			answer _ self ]
		ifFalse: [
			"Mouse was not on aMorph nor any of its children"
			answer _ #rejected ].

	eventHandler _ lastHandler.
	^answer! !

!methodRemoval: PasteUpMorph #isPlayfieldLike!
PasteUpMorph removeSelector: #isPlayfieldLike!

!PasteUpMorph reorganize!
('accessing' activeHand color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' invalidateRect: redrawNeeded)
('classification' isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: dropEnabled morphToDropFrom: repelsMorph:event: wantsDroppedMorph:event:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: mouseDown:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' morphToGrab: mouseButton2Activity wantsWindowEvent: windowEventHandler)
('geometry' externalizeToWorld: internalizeFromWorld: morphExtent: morphPositionInWorld)
('initialization' clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor initialize)
('interaction loop' doOneCycleNow)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: addWorldToggleItemsToHaloMenu: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: viewBox viewBox:)
('stepping' cleanseStepList runStepMethods startStepping: startStepping:at:selector:arguments:stepTime: stopStepping: stopStepping:selector:)
('stepping and presenter' wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addAllMorphs: addMorphFront:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows expandAll findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu openRecentSubmissionsBrowser:)
('world state' addMorph:centeredNear: allNonFlapRelatedSubmorphs assuredNonDisplayCanvas deleteAllHalos displayWorld displayWorldSafely doOneCycle doOneSubCycle flashRects: fullRepaintNeeded haloMorphs privateOuterDisplayWorld restoreMorphicDisplay startSteppingSubmorphsOf: worldState:)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
!

!methodRemoval: Morph #isPlayfieldLike!
Morph removeSelector: #isPlayfieldLike!
