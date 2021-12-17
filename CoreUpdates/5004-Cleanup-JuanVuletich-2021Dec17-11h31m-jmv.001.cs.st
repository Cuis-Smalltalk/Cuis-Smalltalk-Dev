'From Cuis 5.0 [latest update: #5003] on 17 December 2021 at 11:34:35 am'!

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:08:31'!
externalizeDistanceToWorld: aPoint
	"aPoint is a delta in own coordinates. Answer is in world coordinates."

	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:10:53'!
internalizeDistanceFromWorld: aPoint
	"aPoint is a delta in World coordinates. Answer is in own coordinates."

	^ aPoint! !


!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:07:10'!
externalizeBoundsToWorld: aRectangle
	"aRectangle is in own coordinates. Answer is in world coordinates."

	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: aRectangle ]
		ifNil: [ aRectangle ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:08:23'!
externalizeDistanceToWorld: aPoint
	"aPoint is a delta in own coordinates. Answer is in world coordinates."

	| inOwners |
	inOwners _ self externalizeDistance: aPoint.
	^owner
		ifNotNil: [ owner externalizeDistanceToWorld: inOwners ]
		ifNil: [ inOwners ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:05:24'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."

	| inOwners |
	inOwners _ self externalize: aPoint.
	^owner
		ifNotNil: [ owner externalizeToWorld: inOwners ]
		ifNil: [ inOwners ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:10:15'!
internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."

	^ aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:09:34'!
internalizeDistance: aPoint
	"aPoint is a delta in owner's coordinates. Answer is in own coordinates."

	^ aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:09:04'!
internalizeDistanceFromWorld: aPoint
	"aPoint is a delta in World coordinates. Answer is in own coordinates."

	| inOwners |
	inOwners _ owner
		ifNotNil: [ owner internalizeDistanceFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalizeDistance: inOwners! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:10:05'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."

	| inOwners |
	inOwners _ owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalize: inOwners! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:13:29'!
morphPositionInWorld
	"Answer is in world coordinates."

	^ self externalizeToWorld: `0@0`! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:07:15'!
externalizeBoundsToWorld: aRectangle
	"aRectangle is in own coordinates. Answer is in world coordinates."

	| inOwners |
	inOwners _ location externalBoundingRectOf: aRectangle.
	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: inOwners ]
		ifNil: [ inOwners ]! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:10:20'!
internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."

	^ location internalizePosition: aPoint! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:09:42'!
internalizeDistance: aPoint
	"aPoint is a delta in owner's coordinates. Answer is in own coordinates."

	^ location internalizeDelta: aPoint! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:15:10'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph.
	Argument is in world coordinates."

	| newPositionInOwner |
	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	location _ location withTranslation: newPositionInOwner.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !


!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 12/17/2021 11:24:34'!
initClassCachedState

	self allSubInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each redrawNeeded ].
	Cursor defaultCursor activateCursor.! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:07:19'!
externalizeBoundsToWorld: aRectangle
	"aRectangle is in own coordinates. Answer is in world coordinates."

	^ aRectangle! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:05:47'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."

	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:10:25'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."

	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 11:13:35'!
morphPositionInWorld
	"Answer is in world coordinates."

	^ `0@0`! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 12/17/2021 11:26:30'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"
	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			Preferences cheapWindowReframe and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !

!methodRemoval: WorldMorph class #initClassCachedState stamp: 'jmv 12/17/2021 11:33:25'!
WorldMorph class removeSelector: #initClassCachedState!
!methodRemoval: Object #revisar stamp: 'jmv 12/17/2021 11:33:25'!
Object removeSelector: #revisar!

!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents rawBasicAt: rawBasicAt:put: size yourself)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed: removeDependent:)
('error handling' assert: assert:description: caseError doesNotUnderstand: error: halt halt: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: explore hasContentsInExplorer inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('debugging-haltOnce' clearHaltOnce haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('updating' changed update:)
!


!MovableMorph reorganize!
('accessing' location location:)
('geometry' adjustExtent allocHeightForFactor: allocWidthForFactor: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: internalize: internalizeDistance: minimumLayoutExtent morphAlign:with: morphPosition morphPosition: morphPositionInWorld: orbitBy: referencePosition referencePositionInOwner rotateBy: rotation rotation: rotation:scale: rotationCenter scale scale: scaleBy:)
('initialization' initialize openInWorld:)
('dropping/grabbing' aboutToBeGrabbedBy:)
('layout' minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth)
('geometry testing' hasOwnLocation isOrAnyOwnerIsRotated yAxisPointsUp)
('private' fixYAxisDirection privateOwner: privatePosition:)
('layout-properties' layoutSpec layoutSpec:)
('testing' isProportionalHeight isProportionalWidth)
!

