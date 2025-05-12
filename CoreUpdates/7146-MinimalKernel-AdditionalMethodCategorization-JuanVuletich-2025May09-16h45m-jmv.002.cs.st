'From Cuis7.3 [latest update: #7135] on 9 May 2025 at 5:46:25 pm'!

!Object reorganize!
('[mk] accessing' at: at:put: basicAt: basicSize)
('[mk] class membership' class)
('[mk] comparing' = ~= hash)
('[mk] evaluating' value)
('[mk] message handling' perform: perform:with:)
('[mk] printing' printOn:)
('[mk] testing' is: isBehavior isByteString isCharacter isInteger isInterval isMethodProperties isNumber isString isSymbol isUnicodeString)
('[mk] system primitives' instVarAt:put:)
('[mkObjectCopying] copying' copy postCopy shallowCopy)
('[mkObjectSerialization] object serialization' comeFullyUpOnReload: readDataFrom:size:)
('accessing' addInstanceVarNamed:withValue: basicAt:put: rawBasicAt: rawBasicAt:put: size yourself)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' isKindOf: isMemberOf: respondsTo:)
('comparing' literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copyForClipboard copyFrom: copySameFrom: veryDeepCopy)
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
('object serialization' convertToCurrentVersion:refStream: objectForDataStream: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
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


!Class reorganize!
('[mk] accessing' name)
('[mk] accesing class hierarchy' subclassesDo:)
('[mk] printing' printNameOn:)
('[mk] class variables' classPool)
('[mk] pool variables' sharedPools)
('[mkObjectCopying] copying' copy)
('testing' hasMethods isObsolete)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classPool: classPoolFrom: classVarBindingOf: classVarNames classVarValueOf: ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool:)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' ephemeronSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('subclass creation-convenience' subclass:instanceVariableNames:classVariableNames:poolDictionaries:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutAlphabetically fileOutHierarchy fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: hasSubclasses removeSubclass: subclasses superclass:)
('private' setName: spaceUsed)
('initialization' declareClassVarString: declareClassVariables: declareSharedPoolString: declareSharedPools: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('SUnit tests' testCaseClass)
('enumerating' allSubclassesSortedDo: subclassesSortedDo:)
!


!Array reorganize!
('[mk] system prinitives' elementsExchangeIdentityWith: replaceFrom:to:with:startingAt:)
('enumerating' collect:thenSelect: select:thenCollect:)
('comparing' literalEqual:)
('converting' asArray elementsForwardIdentityTo: elementsForwardIdentityTo:copyHash: evalStrings)
('printing' isLiteral printAsBraceFormOn: printAsLiteralOn: printOn: storeAsBraceFormOn: storeOn:)
('private' hasLiteralSuchThat:)
('accessing' atWrap: atWrap:put:)
('testing' identityIncludes: is: isArray shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('literals' allLiteralsDo:)
!


!CompiledMethod reorganize!
('[mk] accessing' methodClass numLiterals selector withPropertiesDo:ifSelector:)
('[mk] literals' header literalAt: objectAt:)
('[mk] private' penultimateLiteral)
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass endPC flag frameSize initialPC methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector: setSignFlag signFlag trailer)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('cleaning' flushCache voidCogVMState)
('closures' containsBlockClosures embeddedBlockClosures)
('code analysis' scanForEqSmallConstant)
('comparing' = equivalentTo: hash)
('converting' asMethodReference asString)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allImplicitLiteralsDo: allLiterals allLiteralsDo: allLiteralsDo:scanForSpecial: codeLiterals codeLiteralsDo: hasLiteral: hasLiteralSuchThat: hasLiteralThorough: headerDescription implicitLiteralsDo: indexOfLiteral: literalAt:put: literalStrings literals literalsDo: objectAt:put: refersToLiteral: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesFieldCode:with:using: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('time stamp' author dateAndTime dateMethodLastSaved dateSortingValue timeStamp)
('tracing' outboundPointersDo:)
('user interface support' browse)
('private' penultimateLiteral:)
!


!Symbol reorganize!
('[mk] comparing' =)
('[mk] printing' storeOn:)
('[mkSymbol] accessing' at:put: replaceFrom:to:with:startingAt:)
('[mkSymbol] converting' asSymbol)
('[mkSymbol] testing' isSymbol)
('[mkSymbol] private' species string:)
('[mkObjectCopying] copying' shallowCopy)
('accessing' precedence)
('copying')
('converting' asPlainString asString capitalized numArgs: uncapitalized)
('printing' print)
('system primitives' flushCache)
('private' errorNoModification)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
!


!Set reorganize!
('[mkHashedCollections] accessing' size)
('[mkHashedCollections] testing' includes:)
('[mkHashedCollections] lookup' lookup:)
('[mkHashedCollections] enumerating' do:)
('[mkHashedCollections] private' atNewIndex:put: findElementOrNil: fullCheck grow growSize init: noCheckAdd: scanFor:)
('[mkObjectCopying] copying' postCopy)
('[mkObjectSerialization] object serialization' comeFullyUpOnReload:)
('accessing' atRandom: like:)
('adding' add: add:withOccurrences:)
('enumerating' collect: union:)
('comparing' emptyCollectionHash)
('private' array fixCollisionsFrom: keyAt: privateSwapArrayElement:with: rehash)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = is: occurrencesOf:)
('inspecting' inspectorClass)
!


!Set class reorganize!
('[mkHashedCollections] instance creation' new newWithRoomForMoreThan: sizeFor:)
('instance creation' newFrom:)
('initialization' quickRehashAllSets rehashAllSets)
!


!Dictionary reorganize!
('[mkHashedCollections] lookup' lookup:)
('[mkHashedCollections] accessing' associationClass at: at:ifAbsent: at:ifPresent: at:put:)
('[mkHashedCollections] enumerating' do: select:)
('[mkHashedCollections] testing' includesKey:)
('[mkHashedCollections] private' noCheckAdd: scanFor:)
('[mkObjectCopying] private' postCopy)
('accessing' associationAt: associationAt:ifAbsent: associations at:ifAbsentPut: at:ifPresent:ifAbsent: keyAtIdentityValue: keyAtIdentityValue:ifAbsent: keyAtValue: keyAtValue:ifAbsent: keys keysSortedSafely values)
('testing' hasBindingThatBeginsWith: is: keyForIdentity: occurrencesOf:)
('adding' add: addAll: declare:from:)
('removing' keysAndValuesRemove: remove: remove:ifAbsent: removeKey: removeKey:ifAbsent: removeUnreferencedKeys unreferencedKeys)
('enumerating' associationsDo: collect: keysAndValuesDo: keysDo: valuesDo:)
('printing' printElementsOn: storeOn:)
('private' errorKeyNotFound: errorValueNotFound keyAt: rehash)
('comparing' = emptyCollectionHash hash)
('inspecting' customizeExplorerContents explorerContents explorerContentsWithIndexCollect: inspectorClass)
('compiling' bindingOf: bindingsDo:)
!


!IdentityDictionary reorganize!
('[mkHashedCollections] private' scanFor:)
('private' keyAtValue:ifAbsent:)
!


!SystemDictionary reorganize!
('[mk] snapshot and quit' quitPrimitive:)
('[mkHashedCollections] globals' at:put:)
('[mkHashedCollections] system attributes' maxIdentityHash)
('[mkVMnImage] primitives' primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('[mkVMnImage] miscelaneous' getSystemAttribute: listBuiltinModule: listLoadedModule: platformName)
('[mkVMnImage] special objects' specialObjectsArray)
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: declare:)
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
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit processHasThreadIdInstVar: processPreemptionYields processPreemptionYields: processSuspensionUnblocks)
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


!IdentitySet reorganize!
('[mkHashedCollections] private' scanFor:)
('accessing' elementForIdentityHash:)
!


!DataStream reorganize!
('[mkObjectSerialization] initialization' setStream:)
('[mkObjectSerialization] reading' beginReference: getCurrentReference maybeBeginReference: next noteCurrentReference: objectAt: readArray readByteArray readCharacter readFalse readFloat readInstance readInteger readNil readReference readShortInst readShortRef readString readSymbol readTrue setCurrentReference:)
('[mkObjectSerialization] aux' typeIDFor: vacantRef)
('write and read' beginInstance:size: nextPut: objectIfBlocked: outputReference: readBitmap readClass readMethod readUnicodeString readUnicodeSymbol readWordArray readWordLike replace:with: tryToPutReference:typeID: writeArray: writeBitmap: writeByteArray: writeCharacter: writeClass: writeFalse: writeFloat: writeInstance: writeInteger: writeNil: writeString: writeSymbol: writeTrue: writeUnicodeString: writeUnicodeSymbol: writeWordLike:)
('other' atEnd byteStream contents errorRead errorWriteReference: flush next: reset rootObject rootObject: setStream:reading: size)
!


!DataStream class reorganize!
('[mkObjectSerialization] instance creation' on:)
('[mkObjectSerialization] cached state access' initCachedState typeMap)
('instance creation' new)
('services' streamedRepresentationOf: unStream:)
('cached state access' releaseClassCachedState)
('examples' example exampleWithPictures testWith:)
!


!ReferenceStream reorganize!
('[mkObjectSerialization] initialization' setStream:)
('[mkObjectSerialization] reading' beginReference: getCurrentReference maybeBeginReference: next noteCurrentReference: objectAt: setCurrentReference:)
('[mkObjectSerialization] aux' isAReferenceType:)
('writing' addSpecialReference: beginInstance:size: blockers blockers: nextPutWeak: objectIfBlocked: references replace:with: reset setStream:reading: tryToPutReference:typeID:)
('statistics' statisticsOfRefs)
!


!Boolean reorganize!
('[mk] logical operations' & | not)
('[mkObjectCopying] copying' shallowCopy)
('logical operations' eqv: xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('testing' is:)
('printing' isLiteral printAsLiteralOn: storeOn:)
('user interface support' asMenuItemTextPrefix)
!


!SmallFloat64 reorganize!
('[mkFloat] arithmetic' * + - /)
('[mkFloat] comparing' < <= = > >= ~= identityHash)
('[mkFloat] mathematical functions' ln timesTwoPower:)
('[mkFloat] truncation and round off' truncated)
('[mkFloat] accessing' exponent)
('[mkObjectCopying] copying' shallowCopy)
('mathematical functions' arcTan cos exp primSqrt profilerFriendlyPrimTimesTwoPower: profilerFriendlyTimesTwoPower: sin)
('accessing' fractionPart)
!


!SmallInteger reorganize!
('[mk] arithmetic' * + - // \\ quo:)
('[mk] comparing' < <= = > >= ~= identityHash)
('[mk] bit manipulation' bitAnd: bitOr: bitShift: bitXor: lowBit)
('[mk] testing' even)
('[mk] printing' decimalDigitLength printString)
('[mkObjectCopying] copying' shallowCopy)
('[mkFloat] converting' asFloat)
('arithmetic' / gcd:)
('bit manipulation' byteReversed hashMultiply)
('testing' isLarge odd)
('printing' numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
!


!LookupKey reorganize!
('[mk] accessing' key key:)
('[mk] comparing' < = hash)
('accessing' canAssign name)
('printing' printOn:)
('testing' isSpecialReadBinding isVariableBinding)
('bindings' beBindingOfType:announcing: beReadOnlyBinding beReadOnlyBindingAnnouncing: beReadWriteBinding beReadWriteBindingAnnouncing: recompileBindingsAnnouncing:)
('literals' allLiteralsDo:)
!


!UndefinedObject reorganize!
('[mk] testing' isNil notNil)
('[mkObjectCopying] copying' shallowCopy)
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isEmptyOrNil isLiteral)
('dependents access' addDependent: suspend)
('class hierarchy' addSubclass: allSuperclassesUpTo: literalScannedAs:notifying: removeSubclass: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclasses subclassesDo: superclassNotValidErrorDescriptionFor: typeOfClass withAllSuperclassesDownFrom:)
('bottom context' canHandleSignal: handleSignal:)
('accessing instances and variables' whichClassDefinesInstanceVariable:ifNone:)
!


!Character reorganize!
('[mk] accesing' codePoint)
('[mk] comparing' identityHash)
('[mk] testing' isCharacter isDigit isLetter isLowercase isUppercase)
('[mkObjectCopying] copying' shallowCopy)
('accessing' asciiValue digitValue)
('comparing' < <= > >= collatesBefore: collatingCategory hash)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isAscii isDiacriticalMark isLineSeparator isNullCharacter isSeparator isSymbolLike isUnaccented isValidInBinarySelectors isValidInFilenames isValidInIdentifiers isValidStartOfIdentifiers isVowel shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting: tokenish)
('printing' hex isLiteral printAsLiteralOn: printOn: storeOn:)
('converting' asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!Point reorganize!
('[mkObjectCopying] copying' shallowCopy)
('accessing' x y)
('arithmetic' * + - / // \\ abs)
('comparing' < <= = > >= between:and: hash max: min: min:max:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: asFloatPoint asIntegerPoint asPoint corner: extent: isPoint rect:)
('geometry' barycentricCoordinatesInTriangle:with:with:into:ifDegenerate: isInsideCircle:with:with: sideOf: to:intersects:to: to:sideOf: triangleArea:with:)
('interpolating' interpolateTo:at:)
('point functions' crossProduct: dist: dotProduct: eightNeighbors flipBy:centerAt: fourNeighbors insideTriangle:with:with: nearestPointAlongLineFrom:to: nearestPointOnLineFrom:to: normal normalized onLineFrom:to: onLineFrom:to:within: quadrantOf: squaredDistanceTo: transposed)
('polar coordinates' degrees magnitude r rho theta)
('printing' printOn: printStringFractionDigits: storeOn:)
('private - initialization' initializePvtX:y:)
('transforming' adhereTo: inverseRotatedBy:about: negated rotatedBy: scaledBy: translatedBy:)
('truncation and round off' ceiling floor rounded roundedHAFZ roundedToEven truncateTo: truncated)
('testing' isIntegerPoint isZero)
!

