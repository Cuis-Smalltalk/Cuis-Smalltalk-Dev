'From Cuis 5.0 [latest update: #4414] on 21 October 2020 at 4:20:37 pm'!
Smalltalk renameClassNamed: #PasteUpMorph as: #OldPasteUpMorph!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 10/21/2020 16:20:05'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	OldPasteUpMorph allInstancesDo: [ :w |
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


!Morph methodsFor: 'testing' stamp: 'jmv 10/21/2020 16:20:05'!
isOwnedByWorld
	^owner is: #OldPasteUpMorph! !


!OldPasteUpMorph methodsFor: 'testing' stamp: 'jmv 10/21/2020 16:20:05'!
is: aSymbol
	^ aSymbol == #OldPasteUpMorph or: [ super is: aSymbol ]! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/21/2020 16:20:05'!
activeSubclass: aMorphicCanvasSubclass
	ActiveSubclass _ aMorphicCanvasSubclass.
	OldPasteUpMorph allInstancesDo: [ :w | w setCanvas ]! !


!SystemDictionary reorganize!
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSnapshot:andQuit: nopTag okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitNoSaveTag quitPrimitive quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit: snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects hasSpecialSelector:ifTrueSetByte: primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate')
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMessageList:ofSize:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel)
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
('vm parameters' doMixedArithmetic doMixedArithmetic: vmParameterAt: vmParameterAt:put:)
!


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits rotationDegrees: whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:fromSubmorph:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: isKnownFailing refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropFiles:localPosition: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' allocHeightForFactor: allocWidthForFactor: displayBounds displayBounds: displayBoundsForHalo displayBoundsOrBogus displayFullBounds extentBorder externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent minimumLayoutExtent morphAlign:with: morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth rotateBy: rotation: rotation:scale: scaleBy:)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' clipsLastSubmorph fullContainsGlobalPoint: fullContainsPoint: isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('layout-properties' layoutSpec layoutSpec: layoutSpecOrNil)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('rotate scale and flex' rotationDegrees)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsDo: clippedSubmorph findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsDrawingOutsideReverseDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying: unclippedSubmorphsReverseDo:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront canAdd: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOwnedByHand isOwnedByWorld isProportionalHeight isProportionalWidth isReallyVisible stepTime)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!


!OldPasteUpMorph reorganize!
('accessing' activeHand color: handlesKeyboard)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:fromSubmorph:for: removedMorph:)
('classification' isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: keyStroke: mouseButton1Down:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' mouseButton2Activity wantsWindowEvent: windowEventHandler)
('geometry' displayBounds externalizeDisplayBounds: externalizeToWorld: fontPreferenceChanged internalizeFromWorld: morphPositionInWorld privateExtent:)
('initialization' clearCanvas clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor setCanvas)
('interaction loop' doOneCycleNow mainLoop runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: setCanvas: viewBox)
('stepping' startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('stepping and presenter' wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addMorph:centeredNear: canHandle:)
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


!MorphicCanvas class reorganize!
('instance creation' activeSubclass activeSubclass: depth:over: on:over: onForm: subclassToUse withExtent:depth:)
!

