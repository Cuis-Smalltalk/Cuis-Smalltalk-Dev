'From Cuis 4.2 of 25 July 2013 [latest update: #1866] on 10 November 2013 at 11:31:05.853967 pm'!
!classDefinition: #Taskbar category: #'Taskbar-Core'!
LayoutMorph subclass: #Taskbar
	instanceVariableNames: 'viewBox'
	classVariableNames: 'SoleInstance '
	poolDictionaries: ''
	category: 'Taskbar-Core'!

!Taskbar commentStamp: '<historical>' prior: 0!
A simple task bar written for Cuis.

dashBoard contains views/controls
viewBox contains graphic buttons of "iconized" windows/morphs.!


!Morph methodsFor: 'naming' stamp: 'jmv 11/10/2013 19:31'!
label
	"Answer the name to show in a list of windows-and-morphs to represent the receiver"

	^ self class name! !


!Taskbar methodsFor: 'initialization' stamp: 'jmv 11/10/2013 23:18'!
defaultHeight
	^Preferences windowTitleFont height * 2! !

!Taskbar methodsFor: 'initialization' stamp: 'jmv 11/10/2013 23:06'!
initialize
	super initialize.
	viewBox _ LayoutMorph newRow.
	self
		addMorph: UpdatingStringMorph initializedInstance 
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 1.0).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			minorDirectionPadding: #right).
	viewBox separation: 5
! !


!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'KenD 11/7/2013 14:04'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (StrikeFont familyName: 'DejaVu' pointSize: 22) 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !


!TaskbarTask methodsFor: 'commands' stamp: 'jmv 11/10/2013 23:29'!
minimize: aMorph

	| button imageForm |

	morph _ aMorph.

	self turnOn.

	button _ PluggableButtonMorph
			model: self
			stateGetter: nil
			action: #switch.
	imageForm _ aMorph imageForm: 32.
	button
		color: Taskbar singleton color;
		icon: imageForm;
		setBalloonText: morph label.
	morph visible: false.
			
	^button! !

!TaskbarTask methodsFor: 'commands' stamp: 'jmv 11/10/2013 23:16'!
restore

	morph visible: true.
	(Taskbar singleton buttonFor: morph) ifNotNil: [ :button |
		button delete ]! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 11/10/2013 19:32'!
findWindow: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu _ MenuMorph new.
	expanded _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ].
	collapsed _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ].
	nakedMorphs _ self submorphsSatisfying: [ :m |
		(m is: #SystemWindow) not ].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) ifTrue: [ ^ Smalltalk beep ].
	(expanded asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activateAndForceLabelToShow.
		w canDiscardEdits ifFalse: [ menu lastItem color: Color red ]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) ifFalse: [ menu addLine ].
	(collapsed asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #expand.
		w canDiscardEdits ifFalse: [ menu lastItem color: Color red ]].
	nakedMorphs isEmpty ifFalse: [ menu addLine ].
	(nakedMorphs asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #comeToFrontAndAddHalo ].
	menu addTitle: 'find window'.
	menu popUpInWorld: self! !


!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 11/10/2013 19:17'!
magnifiedIcon
	| factor magnifiedExtent |
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		self isRoundButton
			ifFalse: [ ^ magnifiedIcon ].

		factor _ extent x / icon width min: extent y / icon height.
		(factor < 1 or: [ factor > 1.7 ]) ifTrue: [
			magnifiedExtent _ (icon extent * factor) rounded.
			magnifiedIcon _ icon magnifyTo: magnifiedExtent ]].
	^magnifiedIcon! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 11/10/2013 22:56'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| xcess newPos |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	aHand world addMorphFront: self.
	newPos _ aHand morphPosition + (-6@20).
	xcess _ newPos + self morphExtent - aHand world morphExtent.
	xcess _ (xcess x max: 0) @ (xcess y max: 0).
	xcess r > 0 ifTrue: [
		newPos _ newPos - xcess ].
	self morphPosition: newPos.
	aHand balloonHelp: self! !


!Taskbar methodsFor: 'commands' stamp: 'jmv 11/10/2013 23:20'!
minimize: aMorph
	viewBox
		addMorph: (TaskbarTask new minimize: aMorph)
		fixedWidth: self defaultHeight! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 11/10/2013 23:15'!
restoreAll
	
	viewBox ifNotNil: [ viewBox submorphs do: [ :button |  button model restore ]]! !

!Taskbar methodsFor: 'private' stamp: 'jmv 11/10/2013 23:14'!
buttonFor: aMorph
	
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model morph == aMorph
				ifTrue: [ ^button ]]
	].
	^nil! !

!Taskbar methodsFor: 'stepping' stamp: 'jmv 11/10/2013 23:07'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	self morphExtent: (self internalize: self world morphExtent) x @ self defaultHeight.
	self morphPosition: 0@ (self world morphExtent y - (self externalizeDistance: extent ) y)! !


!Taskbar class methodsFor: 'class initialization' stamp: 'KenD 11/6/2013 20:58'!
startUp
	"Sign up to get screenSizeChanged events."

	Display
		when: #screenSizeChanged
		send: #screenSizeChanged
		to: Taskbar! !

!Taskbar class methodsFor: 'events' stamp: 'KenD 11/6/2013 20:57'!
screenSizeChanged
	"My uniqueInstance handles screen size changes"

	"Wait until Display has had a chance to complete its update"
	WorldState addDeferredUIMessage: [  self singleton screenSizeChanged ].! !

!methodRemoval: Taskbar #buttonForTask:!
Taskbar removeSelector: #buttonForTask:!
!classDefinition: #Taskbar category: #'Taskbar-Core'!
LayoutMorph subclass: #Taskbar
	instanceVariableNames: 'dashBoard viewBox'
	classVariableNames: 'SoleInstance'
	poolDictionaries: ''
	category: 'Taskbar-Core'!

!Taskbar reorganize!
('change reporting' delete)
('commands' minimize: restore: restoreAll)
('initialization' defaultColor defaultHeight initialize)
('testing' is: isCollapsed: isSticky)
('private' buttonFor:)
('events')
('stepping' screenSizeChanged)
!

!methodRemoval: Morph #nameForFindWindowFeature!
Morph removeSelector: #nameForFindWindowFeature!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock resistsRemoval toggleStickiness unlock unlockContents)
('accessing - properties' hasProperty: isLocked isSticky lock: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits dragEvent:localPosition: rotationDegrees:)
('caching' fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: clipsSubmorphs drawErrorOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hide imageForm: imageForm:forRectangle: isKnownFailing ownShadowForm refreshWorld show visible visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseLeave: mouseMove:localPosition: mouseStillDown: windowEvent:)
('event handling testing' allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' closeWindowFor: containsPoint:event: dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseStillDown:localPosition: processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus keyboardFocusChange:)
('geometry' clippingRect externalize: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: internalize: internalizeDistance: internalizeFromWorld: layoutBounds minimumExtent morphBoundsInWorld morphBoundsInWorld: morphExtent morphExtent: morphExtentInWorld morphFullBoundsInWorld morphHeight morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphWidth worldBoundsForHalo)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' fullContainsPoint: isOrthoRectangularMorph morphContainsPoint:)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: layoutSubmorphs layoutSubmorphsIfNeeded someSubmorphPositionOrExtentChanged submorphBounds)
('layout-properties' layoutSpec layoutSpec: layoutSpecOrNil)
('macpal' flash)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor collapse expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: changeColorTarget:selector:originalColor:hand: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets resizeFromMenu resizeMorph)
('naming' label)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('rotate scale and flex' rotationDegrees)
('stepping' wantsSteps)
('stepping and presenter' arrangeToStartStepping arrangeToStartSteppingIn: shouldGetStepsFrom: startStepping startStepping:at:arguments:stepTime: startSteppingSelector: step stepAt: stopStepping stopSteppingSelector:)
('structure' allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root veryLastLeave withAllOwnersDo: world)
('submorphs-accessing' allMorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphsAt: morphsAt:behind:unlocked: morphsAt:unlocked: morphsAt:unlocked:do: noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorphBack: addMorphFront: addMorphFrontFromWorldPosition: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOpaqueMorph isReallyVisible stepTime)
('updating' redrawNeeded update:)
('user interface' activateWindow activateWindowAndSendTopToBack:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateOwner: privateRemove:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Taskbar reset!