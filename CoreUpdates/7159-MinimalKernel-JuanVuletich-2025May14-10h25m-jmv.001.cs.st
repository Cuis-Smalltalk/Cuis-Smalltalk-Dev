'From Cuis7.3 [latest update: #7158] on 14 May 2025 at 11:06:20 am'!

!CompiledMethod methodsFor: '[mk] printing' stamp: 'jmv 5/14/2025 10:40:39'!
printClassAndSelectorOn: aStream

	aStream
		print: self methodClass;
		nextPutAll: '>>'.
	self selector storeOn: aStream! !

!CompiledMethod methodsFor: '[mk] printing' stamp: 'jmv 5/14/2025 10:35:47'!
printOn: aStream
	"Overrides method inherited from the byte arrayed collection."

	aStream nextPut: $(.
	self printClassAndSelectorOn: aStream.
	aStream space; nextPut: $"; nextPut: $a; space.
	aStream nextPutAll: self class name.
	aStream nextPut: $:; print: self size.
	aStream nextPut: $(; print: self identityHash; nextPut: $); nextPut: $"; nextPut: $)! !


!Object reorganize!
('[mk] accessing' at: at:put: basicAt: basicAt:put: basicSize)
('[mk] associating' ->)
('[mk] class membership' class)
('[mk] comparing' = ~= hash)
('[mk] evaluating' value)
('[mk] message handling' perform: perform:with:)
('[mk] printing' printOn:)
('[mk] testing' is: isBehavior isByteString isCharacter isInteger isInterval isMethodProperties isNumber isString isSymbol isUnicodeString)
('[mk] system primitives' instVarAt:put:)
('[mk] error handling' shouldNotImplement)
('[mkObjectCopying] copying' copy postCopy shallowCopy)
('[mkObjectSerialization] object serialization' comeFullyUpOnReload: readDataFrom:size:)
('accessing' addInstanceVarNamed:withValue: rawBasicAt: rawBasicAt:put: size yourself)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' isKindOf: isMemberOf: respondsTo:)
('comparing' literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copyForClipboard copyFrom: copySameFrom: veryDeepCopy)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage subclassResponsibility)
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


!Behavior reorganize!
('[mk] accessing' definesInstanceVariableNamed: format instSize instSpec instVarNames name theNonMetaClass typeOfClass)
('[mk] testing' isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isPointers isVariable isWeak isWords)
('[mk] printing' printOn:)
('[mk] instance creation' basicNew basicNew: new new:)
('[mk] class hierarchy' superclass)
('[mk] instances' allInstancesOrNil someInstance)
('[mk] enumerating' allSubclassesDo:)
('[mk] comparing' identityHash)
('[mk] constants' maxInstSize)
('[mkClassAndMethodCreation] initialization' superclass:methodDictionary:format:)
('[mkClassAndMethodCreation] accessing' binding bindingOf: classBindingOf:)
('[mkClassAndMethodCreation] method dictionary' addSelectorSilently:withMethod: compiledMethodAt: includesSelector: lookupSelector: methodDict)
('[mkClassAndMethodCreation] private' flushCache)
('[mkClassAndMethodCreation] instances and variables' maxAccessibleInstSize)
('accessing' classDepth decompilerClass parserClass sourceCodeTemplate)
('testing' canZapMethodDictionary hasChangedComparedTo: isObsolete shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: initializedInstance)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> allSelectors compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages canUnderstand: hasMethods thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allSubInstancesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteSubclassesCollection removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('release' obsolete)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!


!ClassDescription reorganize!
('[mk] accessing' instVarNames theMetaClass theNonMetaClass)
('[mk] printing' printNameOn: printOn:)
('[mkClassAndMethodCreation] initialization' organization: setInstVarNames: superclass:methodDictionary:format:)
('[mkClassAndMethodCreation] accessing' localBindingOf:)
('accessing' classVersion comment comment: comment:stamp: hasComment smalltalkComment)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
!


!CompiledMethod reorganize!
('[mk] accessing' methodClass numLiterals selector withPropertiesDo:ifSelector:)
('[mk] literals' header literalAt: objectAt:)
('[mk] printing' printClassAndSelectorOn: printOn:)
('[mk] converting' asString)
('[mk] testing' is:)
('[mk] private' penultimateLiteral)
('[mkClassAndMethodCreation] accessing' endPC initialPC methodClass:)
('[mkClassAndMethodCreation] literals' literalAt:put: objectAt:put:)
('[mkClassAndMethodCreation] private' flushCache penultimateLiteral: voidCogVMState)
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass flag frameSize methodClassAssociation methodClassAssociation: methodReference numArgs numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector: setSignFlag signFlag trailer)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('closures' containsBlockClosures embeddedBlockClosures)
('code analysis' scanForEqSmallConstant)
('comparing' = equivalentTo: hash)
('converting' asMethodReference)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allImplicitLiteralsDo: allLiterals allLiteralsDo: allLiteralsDo:scanForSpecial: codeLiterals codeLiteralsDo: hasLiteral: hasLiteralSuchThat: hasLiteralThorough: headerDescription implicitLiteralsDo: indexOfLiteral: literalStrings literals literalsDo: refersToLiteral: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesFieldCode:with:using: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('time stamp' author dateAndTime dateMethodLastSaved dateSortingValue timeStamp)
('tracing' outboundPointersDo:)
('user interface support' browse)
!


!Symbol reorganize!
('[mk] accessing' at:put:)
('[mk] comparing' =)
('[mk] printing' storeOn:)
('[mkSymbol] accessing' replaceFrom:to:with:startingAt:)
('[mkSymbol] converting' asSymbol)
('[mkSymbol] testing' isSymbol)
('[mkSymbol] private' errorNoModification flushCache species string:)
('[mkObjectCopying] copying' shallowCopy)
('accessing' precedence)
('converting' asPlainString asString capitalized numArgs: uncapitalized)
('printing' print)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
!


!SystemDictionary reorganize!
('[mk] vm parameters' vmParameterAt: wordSize)
('[mk] snapshot and quit' quitPrimitive:)
('[mk] printing' printOn:)
('[mkHashedCollections] globals' at:put: flushClassNameCache)
('[mkHashedCollections] system attributes' maxIdentityHash)
('[mkVMnImage] primitives' primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('[mkVMnImage] miscelaneous' getSystemAttribute: listBuiltinModule: listLoadedModule: platformName)
('[mkVMnImage] special objects' specialObjectsArray)
('[mkClassAndMethodCreation] miscelaneous' voidCogVMState)
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: declare:)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imagePath primImageName: userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModules listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformSubtype unloadModule: vmFileName vmOptionsDo: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn:)
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
('image')
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt:put:)
('special selectors' hasSpecialSelector:ifTrueSetByte:)
!


!Stream reorganize!
('[mk] writing' next nextPutAll: print:)
('[mk] accessing' binary)
('[mk] normalized access' nextFloat32BigEndian: nextFloat32Put:bigEndian: nextFloat64BigEndian: nextFloat64Put:bigEndian: nextInt16BigEndian: nextInt16Put:bigEndian: nextInt32BigEndian: nextInt32Put:bigEndian: nextInt64BigEndian: nextInt64Put:bigEndian: nextUint16BigEndian: nextUint16Put:bigEndian: nextUint32BigEndian: nextUint32Put:bigEndian: nextUint64BigEndian: nextUint64Put:bigEndian:)
('[mk] nonhomogeneous accessing' nextString)
('[mk] testing' is:)
('accessing' basicNext contents flush localName next: next:put: nextMatchAll: nextPut: nextPut:when: nextPutAll:asCommaSeparated: nextPutAll:when: openReadOnly printOn: readOnly)
('testing' atEnd closed isReadOnly isWriteOnly nextUint32WordsPutAll:)
('enumerating' do:)
('printing' print:fractionDigits: print:when:)
('FileStream compatibility' close)
('nonhomogeneous accessing' nextNumber nextStringPut: nextUnicodeString nextUnicodeStringPut:)
!


!WriteStream reorganize!
('[mk] writing' next:into:startingAt: nextPut: nextPutAll: space)
('[mk] accessing' contents)
('[mk] positioning' reset)
('[mk] private' isCompatibleWithContents: on:)
('accessing' next next: next:putAll:startingAt: nextPutAllString:withAttributes: nextWordsInto: peek size upTo:delimiterIsTerminator:)
('positioning' position: resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: timeStamp)
('printing' store:)
('private' braceArray braceArray: growTo: pastEndPut: with: withAttribute:do: withAttributes:do:)
('testing' isEmpty isWriteOnly)
('services' padToEndIfCantTruncate)
!


!ContextPart reorganize!
('[mk] accessing' at: at:put: basicAt:put: basicSize)
('[mk] debugger access' methodClass selector sender)
('[mk] printing' printOn:)
('accessing' basicAt: client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' directedSuperSend:numArgs: doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself namedTempAt: namedTempAt:put: print:on: releaseTo: shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver: closureCopy:copiedValues: executeMethod:forSelector:withArgs:receiver: jump pop push: quickSend:to:with:lookupIn: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: insertSender: isPrimFailToken: privSender: push:fromIndexable: stackPtr stackp: stepToCalleeOrNil tryNamedPrimitiveIn:for:withArgs: unwindAndStop: willFailReturn)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext runUntilReturnFrom: unwindTo: unwindTo:safely:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead isDeadOrAtEnd isDone secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
('instruction decoding implicit literals' methodReturnSpecialConstant: pushSpecialConstant: sendSpecial:numArgs:)
('read-only objects' modificationForbiddenFor:at:put: modificationForbiddenFor:instVarAt:put: simulatedObject:instVarAt:put:)
!


!False reorganize!
('[mk] logical operations' & | not)
('[mk] printing' printOn:)
('logical operations' xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('user interface support' asMenuItemTextPrefix)
!


!True reorganize!
('[mk] logical operations' & | not)
('[mk] printing' printOn:)
('logical operations' xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('user interface support' asMenuItemTextPrefix)
!


!SmallInteger reorganize!
('[mk] arithmetic' * + - // \\ quo:)
('[mk] comparing' < <= = > >= ~= identityHash)
('[mk] bit manipulation' bitAnd: bitOr: bitShift: bitXor: lowBit)
('[mk] testing' even)
('[mk] printing' decimalDigitLength printString)
('[mk] system primitives' nextInstance)
('[mkObjectCopying] copying' shallowCopy)
('[mkFloat] converting' asFloat)
('arithmetic' / gcd:)
('bit manipulation' byteReversed hashMultiply)
('testing' isLarge odd)
('printing' numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextObject)
('mathematical functions' sqrt)
!


!LookupKey reorganize!
('[mk] accessing' key key:)
('[mk] comparing' < = hash)
('[mk] printing' printOn:)
('accessing' canAssign name)
('testing' isSpecialReadBinding isVariableBinding)
('bindings' beBindingOfType:announcing: beReadOnlyBinding beReadOnlyBindingAnnouncing: beReadWriteBinding beReadWriteBindingAnnouncing: recompileBindingsAnnouncing:)
('literals' allLiteralsDo:)
!


!Association reorganize!
('[mk] accessing' key:value: value value:)
('[mk] comparing' = hash)
('[mk] printing' printOn:)
('printing' storeOn:)
('objects from disk' objectForDataStream:)
('testing' analogousCodeTo: isSpecialWriteBinding literalEqual:)
!


!UndefinedObject reorganize!
('[mk] testing' isNil notNil)
('[mk] printing' printOn:)
('[mkObjectCopying] copying' shallowCopy)
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('printing' printAsLiteralOn: storeOn:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isEmptyOrNil isLiteral)
('dependents access' addDependent: suspend)
('class hierarchy' addSubclass: allSuperclassesUpTo: literalScannedAs:notifying: removeSubclass: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclasses subclassesDo: superclassNotValidErrorDescriptionFor: typeOfClass withAllSuperclassesDownFrom:)
('bottom context' canHandleSignal: handleSignal:)
('accessing instances and variables' whichClassDefinesInstanceVariable:ifNone:)
!


!Character class reorganize!
('[mk] instance creation' binarySelectorCharacters codePoint: digitValue: new)
('[mk] accessing untypeable characters' lf newLineCharacter space)
('class initialization' accentedLetters initialize initializeNamedCharactersMap uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' separators value:)
('accessing untypeable characters' backQuote backspace cr delete doubleQuote escape home insert newPage noBreakSpace null pageDown pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow tab zeroWidthSpace)
('converting' evaluate:withUtf16WordsOf: evaluate:withUtf8BytesOf: evaluateOnce:withUtf8BytesOf: macRomanToLatin1: utf8BytesAndCodePointAt:in:into:into: utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into: utf8BytesAndCodePointForIso8859s1:into:into: utf8BytesOf:)
('static state' basicLookupTables lowercaseMappingTable unaccentedTable uppercaseMappingTable)
('services' canonicalCompositionOf:and: canonicalDecomposition:on: combiningClass: isCombiningStarter: isDiacriticalMark:)
('constants' namedCharactersMap)
('private' constantNameFor: constantNames)
!

