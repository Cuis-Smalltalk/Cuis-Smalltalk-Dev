'From Cuis 4.2 of 25 July 2013 [latest update: #1857] on 28 October 2013 at 8:06:27.522867 pm'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs extension location layoutNeeded layoutSpec properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'accessing' stamp: 'jmv 10/28/2013 19:03'!
beSticky
	"make the receiver sticky"
	self sticky: true! !

!Morph methodsFor: 'accessing' stamp: 'jmv 10/28/2013 19:04'!
toggleStickiness
	"togle the receiver's Stickiness"
	self sticky: self isSticky not! !

!Morph methodsFor: 'accessing - properties' stamp: 'jmv 10/28/2013 19:07'!
hasProperty: aSymbol 
	"Answer whether the receiver has the property named aSymbol"
	properties ifNil: [ ^false ].
	^properties includesKey: aSymbol! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/17/2013 17:09'!
isLocked
	"Answer whether the receiver is Locked.
	The idea is that a locked morph behaves as if it wasn't a separate object,
	but just part of the #drawOn: method in the owner. Locked morphs receive no events."

	^ self valueOfProperty: #locked ifAbsent: [ false ]! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/17/2013 17:09'!
isSticky
	"Answer whether the receiver is Sticky.
	A morph that is made sticky can not be easily grabbed with the hand."

	^ self valueOfProperty: #sticky ifAbsent: [ false ]! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/17/2013 17:09'!
lock: aBoolean
	"change the receiver's lock property"

	self setProperty: #locked toValue: aBoolean! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/17/2013 17:12'!
removeProperty: aSymbol
	"removes the property named aSymbol if it exists"

	properties ifNil: [ ^ self ].
	properties
		removeKey: aSymbol
		ifAbsent: nil.
	properties isEmpty ifTrue: [ properties := nil ]! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/28/2013 20:03'!
setProperty: aSymbol toValue: anObject
	"change the receiver's property named aSymbol to anObject"

	"the properties dictionary never has nil as value.
	Asking for a nil value is the same as removing the property."

	anObject ifNil: [^ self removeProperty: aSymbol].
	properties ifNil: [ properties := IdentityDictionary new ].
	properties at: aSymbol put: anObject! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/17/2013 17:09'!
sticky: aBoolean
	"change the receiver's sticky property"

	self setProperty: #sticky toValue: aBoolean! !

!Morph methodsFor: 'accessing - properties' stamp: 'jmv 10/28/2013 20:05'!
valueOfProperty: aSymbol
	"Answer the value of the receiver's property named aSymbol.
	If property is not present, answer nil."

	^ properties
		  ifNotNil: [ properties at: aSymbol ifAbsent: nil ]! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/17/2013 17:09'!
valueOfProperty: aSymbol ifAbsent: aBlock
	"if the receiver possesses a property of the given name, answer
	its value. If not then evaluate aBlock and answer the result of
	this block evaluation"
	^ properties
		ifNil: [ aBlock value ]
		ifNotNil: [ properties at: aSymbol ifAbsent: aBlock ]! !

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 10/17/2013 17:09'!
valueOfProperty: aSymbol ifPresentDo: aBlock
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property"
	properties ifNil: [^ self ].
	^ aBlock
		value: (properties valueOfProperty: aSymbol ifAbsent: [ ^ self ])! !

!Morph methodsFor: 'drawing' stamp: 'KenD 10/17/2013 17:09'!
visible
	"Answer if I am visible -- default is true"

	^ self valueOfProperty: #visible ifAbsent: [ true ]! !

!Morph methodsFor: 'drawing' stamp: 'KenD 10/17/2013 17:09'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean"

	self visible == aBoolean
		ifTrue: [ ^ self ].
	self setProperty: #visible toValue: aBoolean.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded! !

!Morph methodsFor: 'halos and balloon help' stamp: 'KenD 10/17/2013 17:09'!
balloonText
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer"

	^ self valueOfProperty: #balloonText ifAbsent: [ nil ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'KenD 10/17/2013 17:09'!
setBalloonText: stringOrText
	"Set receiver's balloon help text. Pass nil to remove the help."

	 stringOrText
		ifNil: [ self removeProperty: #balloonText ]
		ifNotNil: [ self setProperty: #balloonText
			    	 toValue: stringOrText asString ]! !

!Morph methodsFor: 'layout-properties' stamp: 'jmv 2/24/2011 20:10'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ]..
	layoutSpec := LayoutSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec ! !

!Morph methodsFor: 'layout-properties' stamp: 'jmv 8/25/2012 14:25'!
layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	self layoutSpec == aLayoutSpec ifTrue:[^self].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].! !

!Morph methodsFor: 'layout-properties' stamp: 'jmv 8/20/2013 10:55'!
layoutSpecOrNil
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout.
	Answer nil if none!!"

	^ layoutSpec ! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 10/28/2013 19:59'!
keyStroke: aKeyboardEvent 
	| matchString char asc selectable help |
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self world activeHand.
	char := aKeyboardEvent keyCharacter.
	asc := char asciiValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
					selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: [
							"self delete."
							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil event: aKeyboardEvent.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			self selectItem: nil event: aKeyboardEvent.
			aKeyboardEvent hand newMouseFocus: popUpOwner owner.
			^aKeyboardEvent hand newKeyboardFocus: popUpOwner owner].
	(asc = 28 or: [asc = 29]) 
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := char = Character backspace 
				ifTrue: [
					matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent.
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self world activeHand! !

!methodRemoval: Morph #assureExtension!
Morph removeSelector: #assureExtension!
!methodRemoval: Morph #extension!
Morph removeSelector: #extension!
!methodRemoval: Morph #hasExtension!
Morph removeSelector: #hasExtension!
!methodRemoval: Morph #privateExtension:!
Morph removeSelector: #privateExtension:!
!methodRemoval: Morph #properties!
Morph removeSelector: #properties!
!methodRemoval: Morph #resetExtension!
Morph removeSelector: #resetExtension!
!methodRemoval: Morph #setBalloonText:maxLineLength:!
Morph removeSelector: #setBalloonText:maxLineLength:!
!methodRemoval: Morph #valueOfProperty:ifAbsentPut:!
Morph removeSelector: #valueOfProperty:ifAbsentPut:!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock resistsRemoval toggleStickiness unlock unlockContents)
('accessing - extension')
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
('naming' nameForFindWindowFeature)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('property extension')
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
Morph allSubInstancesDo: [ :m | | e | e _ m instVarNamed: 'extension'. e ifNotNil: [ m instVarNamed: 'layoutSpec' put: e layoutSpec. m instVarNamed: 'properties' put: (e instVarNamed: 'otherProperties')]]!
