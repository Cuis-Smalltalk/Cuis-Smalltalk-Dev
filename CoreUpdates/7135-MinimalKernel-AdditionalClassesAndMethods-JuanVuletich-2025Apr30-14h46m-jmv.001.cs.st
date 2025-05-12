'From Cuis7.3 [latest update: #7123] on 30 April 2025 at 3:02:05 pm'!
!classDefinition: #WeakArray category: #'MinimalKernel-Collections'!
Array weakSubclass: #WeakArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Collections'!
!classDefinition: #WeakSet category: #'MinimalKernel-Collections'!
Collection subclass: #WeakSet
	instanceVariableNames: 'buckets estimatedSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Collections'!
!classDefinition: #Exception category: #'MinimalKernel-Exceptions'!
Object subclass: #Exception
	instanceVariableNames: 'messageText tag signalContext handlerContexts outerContext handlerBlockNotCurtailed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Exceptions'!
!classDefinition: #Error category: #'MinimalKernel-Exceptions'!
Exception subclass: #Error
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Exceptions'!
!classDefinition: #ArithmeticError category: #'MinimalKernel-Exceptions'!
Error subclass: #ArithmeticError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Exceptions'!
!classDefinition: #DomainError category: #'MinimalKernel-Exceptions'!
ArithmeticError subclass: #DomainError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Exceptions'!
!classDefinition: #ArithmeticMessageError category: #'MinimalKernel-Exceptions'!
Error subclass: #ArithmeticMessageError
	instanceVariableNames: 'receiver selector arguments'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Exceptions'!
!classDefinition: #ZeroDivide category: #'MinimalKernel-Exceptions'!
ArithmeticMessageError subclass: #ZeroDivide
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinimalKernel-Exceptions'!

!ProtoObject reorganize!
('minimalKernel - Bootstrap' == ~~ become: doesNotUnderstand: error: identityHash initialize isNil notNil warning:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isObject)
('comparing' scaledIdentityHash)
('system primitives' cannotInterpret: nextInstance nextObject statePointsTo:)
('objects from disk' rehash)
('debugging' flag: withArgs:executeMethod:)
('apply primitives' tryPrimitive:withArgs:)
('tracing' pointsTo:)
!


!Object reorganize!
('minimalKernel - Bootstrap' = ~= at: at:put: basicAt: basicSize class hash is: isBehavior isByteString isCharacter isInteger isInterval isMethodProperties isNumber isString isSymbol isUnicodeString perform: perform:with: printOn: value)
('accessing' addInstanceVarNamed:withValue: basicAt:put: rawBasicAt: rawBasicAt:put: size yourself)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' isKindOf: isMemberOf: respondsTo:)
('comparing' literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' valueWithArguments: valueWithPossibleArgument:)
('dependency mechanism' addDependent: breakDependents changed: removeDependent:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor finalizationRegistry finalize mourn retryWithGC:until: toFinalizeSend:to: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard customizeExplorerContents explore hasContentsInExplorer inspect inspectWithLabel: inspectorClass)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform:with:with: perform:with:with:with: perform:with:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
('stepping' stepAt: wantsSteps)
('testing' isArray isBlock isClosure isCollection isCompiledCode isComplex isContext isFloat isFloatOrFloatComplex isFraction isObject isPoint isPseudoContext isVariableBinding name renameTo: shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand flash runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('updating' changed update:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('literals' allLiteralsDo: allLiteralsDo:scanForSpecial: hasLiteral: hasLiteralSuchThat: hasLiteralSuchThat:scanForSpecial:)
!


!Symbol reorganize!
('minimalKernel - Bootstrap' = storeOn:)
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('printing' print)
('system primitives' flushCache)
('private' errorNoModification species string:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
!


!SystemDictionary reorganize!
('minimalKernel - Bootstrap' quitPrimitive: specialObjectsArray)
('minimalKernel - VM and image support' getSystemAttribute: listBuiltinModule: listLoadedModule: platformName primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: at:put: declare:)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imagePath primImageName: userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModules listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformSubtype unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allImplementorsOf:subTo: allImplementorsOf:superTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithSourceString:matchCase:scopeClass:includeSubclasses:includeSuperclasses: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSelect:localTo: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfDirectSendersOf: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers rootClasses unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit filenameForUpdatedImage: lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitTag readAndApplyUserPrefs removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs: saveAs:clearAllClassState: saveImage saveRollingReleaseAndQuit saveStableRelease send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: logToUserChanges:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: versionString)
('special objects' clearExternalObjects externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialSelectorAt: specialSelectorSize unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: confirmRemovalOf:on:andSubclasses: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing - main' browse browseFull: browseFull:selector: browseHierarchy: browseHierarchy:selector: browserWindowClass findAndBrowseClass findInSourceCode)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllClasses browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOf:subTo: browseAllImplementorsOf:superTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:localTo: browseAllSelect:name:autoHighlight: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqEqSentToCharacterConstants browseEqEqSentToNumericConstants browseEqEqSentToSmallIntegerConstants browseEqSentToCharacterConstants browseEqSentToNumericConstants browseEqSentToSmallIntegerConstants browseEqSmallConstant browseInstVarRefs: browseLikelyUnneededRedefinitions browseMessageList:name: browseMessageList:name:autoHighlight:allOccurrences: browseMessageListUnsorted:name:autoHighlight: browseMethodsWithSourceString: browseMethodsWithSourceString:in: browseMethodsWithSourceString:scopeClass:includeSubclasses:includeSuperclasses: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseSendersOf:notTo: browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive specialSelectors)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields: processSuspensionUnblocks)
('query' classesWithExtensionCategories hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' checkIfAlreadyRunningOrStoppedNoExit displayCommandLineUsageOn: displaySystemInformationStringOn: doStartUp: lastCommandLineArguments logStartupDebugAid: logStartupDebugAids logStartupError: logStartupNotice: processCommandLineArguments: processFinalCommandLineOption:optionArgument:raiseExceptions: processInitialCommandLineOption:optionArgument: readCommandLineArguments setStartupStamp startUpArguments startUpScriptAllArguments startUpScriptArguments)
('startup - recover user changes' warnAndOfferLastUserChanges)
('startup - restore lost changes' isQuitNoSaveRecord: isQuitRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put:)
('special selectors' hasSpecialSelector:ifTrueSetByte:)
!


!StdIOWriteStream reorganize!
('minimalKernel - Bootstrap' flush newLine nextPut: nextPutAll: openOnHandle:name: primFlush: primWrite:from:startingAt:count: printOn: space)
!

