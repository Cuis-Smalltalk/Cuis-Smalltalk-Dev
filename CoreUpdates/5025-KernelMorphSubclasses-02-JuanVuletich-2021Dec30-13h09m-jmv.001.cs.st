'From Cuis 5.0 [latest update: #5024] on 30 December 2021 at 1:10:26 pm'!
Smalltalk renameClassNamed: #HandMorph as: #OldHandMorph!
Smalltalk renameClassNamed: #PasteUpMorph as: #OldPasteUpMorph!
Smalltalk renameClassNamed: #WorldMorph as: #OldWorldMorph!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 12/30/2021 13:10:02'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	OldWorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoMenuIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!Morph methodsFor: 'structure' stamp: 'jmv 12/30/2021 13:09:45'!
root
	"Return the root of the composite morph containing the receiver. The owner of the root is either nil, a WorldMorph, or a HandMorph. If the receiver's owner is nil, the root is the receiver itself. This method always returns a morph."

	(owner isNil or: [ owner isWorldMorph or: [ owner is: #OldHandMorph ] ]) ifTrue: [ ^self ].
	^owner root! !

!Morph methodsFor: 'testing' stamp: 'jmv 12/30/2021 13:09:45'!
isOwnedByHand
	^owner is: #OldHandMorph! !


!HaloMorph methodsFor: 'handles' stamp: 'jmv 12/30/2021 13:09:45'!
addCollapseHandle: handleSpec
	"Add the collapse handle, with all of its event handlers set up, unless the target's owner is not the world or the hand."

	target owner
		ifNil: [ ^self ]	"nil happens, amazingly"
		ifNotNil: [ :to |
			(to isWorldMorph or: [ to is: #OldHandMorph ])
				ifFalse: [ ^self ]].
		
	(self addHandle: handleSpec)
		mouseDownSelector: #mouseDownInCollapseHandle:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeCollapse:with:! !


!OldHandMorph methodsFor: 'classification' stamp: 'jmv 12/30/2021 13:09:45'!
is: aSymbol
	^ aSymbol == #OldHandMorph or: [ super is: aSymbol ]! !


!OldWorldMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:09:45'!
initialize

	super initialize.
	activeHand _ OldHandMorph new.
	hands _ { activeHand }.
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	alarms _ Heap sortBlock: self alarmSortBlock.
	lastAlarmTime _ 0.
	deferredUIMessages _ SharedQueue new.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 12/30/2021 13:09:45'!
damageReportedOther
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand.
	Answer might include nils. Skip them."
	| answer possiblyMoreRectsToMerge |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m visible not or: [m owner isNil or: [m owner is: #OldHandMorph]]) ifTrue: [
				answer add: r]].
		otherDamage do: [ :r | answer add: r ].
	].
	possiblyMoreRectsToMerge _ true.
	[possiblyMoreRectsToMerge] whileTrue: [
		possiblyMoreRectsToMerge _ false.
		answer withIndexDo: [ :r1 :i |
			r1 ifNotNil: [
				i+1 to: answer size do: [ :j | | r2 |
					r2 _ answer at: j.
					r2 ifNotNil: [
						(r1 intersects: r2) ifTrue: [
							r1 updateMerging: r2.
							answer at: j put: nil.
							possiblyMoreRectsToMerge _ true ]]]]]].
	^answer! !


!SystemDictionary reorganize!
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs:clearAllClassState: saveAsNewReleaseAndQuit saveAsNewReleaseAndStay saveImage send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMessageListUnsorted:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive specialSelectors)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit isSpur maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' doStartUp: processCommandLineArgument:storeStartUpScriptArgsOn: processCommandLineArguments readCommandLineArguments setStartupStamp startUpArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put:)
('special selectors' hasSpecialSelector:ifTrueSetByte:)
!


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky boundsFinderCanvas canvas color location location: lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo: valueOfProperty:ifPresentDo:ifAbsent:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' drawOn: drawingFails drawingFailsNot fullyCovers: hide icon imageForm: imageForm:depth: isKnownFailing postDrawOn: refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayBoundsSetFrom: displayBoundsUpdateFrom: displayFullBounds extentChanged: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtentInOwner morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale topLeftInOwner)
('geometry services' collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: displayBoundsIntersects: fullIncludesPixel: includesPixel: isCloserThan:toPixel: on:ifCovered:uncoveredPartsDo:else: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf wantsToBeOnTop: withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allSubmorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront canAdd: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' drawsKeyboardFocusIndicator hasModel is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isProportionalHeight isProportionalWidth isReallyVisible wantsToBeOnTop)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateArrangeWantsToBeOnTop privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!


!HaloMorph reorganize!
('accessing' target target:)
('drawing' drawCoordinateSystemOn: drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:handPosition:hand: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' rejectsEvent: rejectsEventFully:)
('geometry services')
('geometry testing' knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addExploreHandle: addGrabHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addResizeHandle: addRotateHandle: addScaleHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor delete)
('testing' is: isIncludedInTaskbar wantsHalo)
('private' addHandle: addHandles addHandles: addNameString: doDebug:with: doDrag:with: doDup:with: doExplore:with: doGrab:with: doMenu:with: doRecolor:with: doResize:with: doRot:with: doScale:with: endInteraction handlesBox maybeCollapse:with: maybeDismiss:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startResize:with: startRot:with: startScale:with:)
('forward to target' deleteBalloon mouseDownOnHelpHandle:)
('stepping' step stepTime wantsSteps)
('geometry' morphLocalBounds)
!


!OldHandMorph reorganize!
('accessing' lastMouseEvent lastMouseEventTime mouseOverHandler)
('balloon help' balloonHelp balloonHelp: deleteBalloonTarget: removePendingBalloonFor: spawnBalloonFor: triggerBalloonFor:after:)
('caching' releaseCachedState)
('classification' is:)
('double click support' dontWaitForMoreClicks waitForClicksOrDrag:event:clkSel:dblClkSel: waitForClicksOrDrag:event:dragSel:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel:dragSel: waitingForMoreClicks)
('drawing' drawOn: fullDrawHandOn: hideHardwareCursor isDrawnBySoftware restoreSavedPatchOn: savePatchFrom: showHardwareCursor)
('event handling' createEventFrom:ofType: flushEvents noticeMouseOver:event: processEventQueue)
('events-processing' startDropEventDispatch: startDropFilesEventDispatch: startEventDispatch: startKeyboardDispatch: startMouseDispatch: startWindowEventDispatch:)
('focus handling' activateNextWindow activatePreviousWindow keyboardFocus keyboardFocusNext keyboardFocusPrevious mouseFocus newKeyboardFocus: newMouseFocus: nextFocusMorph nextFocusWindow previousFocusMorph previousFocusWindow releaseAllFoci releaseKeyboardFocus releaseKeyboardFocus: releaseMouseFocus releaseMouseFocus:)
('geometry' basicDisplayBounds displayFullBoundsForPatch morphPosition:)
('geometry testing' submorphsMightProtrude)
('grabbing/dropping' attachMorph: attachMorphBeside: dropMorph:event: dropMorphs: grabMorph: grabMorph:delta: grabMorph:moveUnderHand:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('testing' isIncludedInTaskbar)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('updating')
('private' forgetGrabMorphDataFor: grabMorphDataFor: rememberGrabMorphDataFor:)
('private events' generateDropFilesEvent: generateKeyboardEvent: generateMouseEvent: generateMouseScrollEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
!


!OldWorldMorph reorganize!
('accessing' handlesKeyboard)
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('caching' releaseCachedState)
('canvas' canvas clearCanvas ensureNonDisplayCanvas recordDamagedRect:for: resetCanvas setCanvas: setMainCanvas)
('change reporting' addedMorph: invalidateDisplayRect:for: removedMorph:)
('classification' isWorldMorph)
('defaul desktop' recreateDefaultDesktop tearDownDesktop)
('drawing' displayWorld drawOn:)
('dropping/grabbing' allowsFilesDrop dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('event handling' mouseButton2Activity wantsWindowEvent:)
('events' click:localPosition: keyStroke: windowEvent:)
('geometry' basicDisplayBounds extentChanged: externalizeBoundsToWorld: externalizeDistanceToWorld: externalizeToWorld: internalizeDistanceFromWorld: internalizeFromWorld: morphPositionInWorld viewBox)
('halos and balloon help' deleteBalloonTarget: wantsHaloHandleWithSelector:inHalo:)
('hands' activeHand hands handsDo: handsReverseDo: handsToDrawForDamage:do:)
('initialization' clearWaitDelay initialize stepListSortBlock)
('interaction loop' mainLoop runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand:)
('misc' fillRects: restoreDisplay)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' cleanseStepList runLocalStepMethods: runStepMethods startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('structure' topmostWorld world)
('submorphs-accessing' allMorphsDo: canHandle: firstHand)
('submorphs-add/remove' addMorph:centeredNear:)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('testing' isReallyVisible)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying:do:orCancel:)
('update cycle' checkForNewScreenSize checkIfUpdateNeeded displayWorldSafely doOneCycle doOneCycleNow doOneMinimalCycleNow handleFatalDrawingError: tryDeferredUpdatingAndSetCanvas whenUIinSafeState:)
('windows' findATranscript)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos fullRepaintNeeded haloMorphsDo:)
!


!DamageRecorder reorganize!
('initialization' initialize)
('recording' damageReportedFor: damageReportedOther recordInvalidRect:for: reset)
('testing' updateIsNeeded)
('private' pvtAccessProtect pvtInnerRecordInvalidRect:for:)
!

