'From Cuis7.9 [latest update: #7987] on 8 June 2026 at 4:28:15 pm'!


!Object reorganize!
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize rawBasicAt: rawBasicAt:put: size yourself)
('literals' allLiteralsDo: allLiteralsDo:scanForSpecial: hasLiteral: hasLiteralSuchThat: hasLiteralSuchThat:scanForSpecial:)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('dependency mechanism' addDependent: breakDependents changed: removeDependent:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent: removeAllActions)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor finalizationRegistry finalize mourn retryWithGC:until: toFinalizeSend:to: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard customizeExplorerContents explore hasContentsInExplorer inspect inspectWithLabel: inspectorClass)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:with:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
('pinning' isPinned pin unpin)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isByteString isCharacter isClosure isCollection isCompiledCode isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isUnicodeString isVariableBinding name renameTo: shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('morphic' activeHand flash runningWorld)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError:signalContext: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('profiler support' profilerFriendlyCall:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('updating' changed update:)
!


!Behavior reorganize!
('accessing' binding bindingOf: classBindingOf: classDepth decompilerClass definesInstanceVariableNamed: format instSize instSpec instVarNames name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allRegularInstVarNames allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: isInstanceVariableNamedReferencedInHierarchy: sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('accessing method dictionary' >> allSelectors compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('method dictionary' addSelectorSilently:withMethod: compiledMethodAt: includesSelector: lookupSelector: methodDict methodDict:)
('accessing instances' allInstances allInstancesDo: allInstancesOrNil allSubInstances allSubInstancesDo: inspectAllInstances inspectSubInstances instanceCount someInstance)
('auto complete' typeName)
('class hierarchy' deepSubclassNamed: superclass)
('comparing' identityHash)
('compiling' compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges variablesAndOffsetsDo:)
('constants' maxInstSize)
('copying' copyOfMethodDictionary postCopy)
('creating method dictionary' methodDictionary:)
('enumerating' allSubclassesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('instance creation' adoptInstance: basicNew basicNew: basicPinnedNew: initializedInstance new new: uninitializedPinnedNew:)
('methods' addMethodsTo:thatReferExplicitlyTo: addMethodsTo:thatReferTo: addTo:methodsThatReferInHierarchyTo: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: stampAt:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteClassesCollection removeObsoleteSubclass:)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('release' obsolete)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('system-support' allCallsOn)
('testing' canZapMethodDictionary hasChangedComparedTo: isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages canUnderstand: hasMethods whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferExplicitlyTo: whichSelectorsReferTo: whichSelectorsStoreInto:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingBasicPinnedNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: handleFailingFailingBasicPinnedNew: handleFailingFailingUninitializedPinnedNew: handleFailingUninitializedPinnedNew: setFormat: spaceUsed)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('initialization' superclass:methodDictionary:format:)
('instances and variables' maxAccessibleInstSize)
!


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment instVarNames localBindingOf: smalltalkComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inCategory:methodTimeStamp: allMethodCategoriesIntegratedThrough: allMethodsInCategory: extensionSelectorsDo: induceMDFault methodCategories methodCategoriesAsSortedCollection nonExtensionSelectorsDo: recordedChangesForSelector: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelector:ifIn: selectorsInCategory:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:libraryName:notifying: compile:classified:withStamp:libraryName:notifying:logSource: compile:classified:withStamp:notifying: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:library:stamp: methodsFor:library:stamp:prior: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on:withStamp:libraryName:priorMethod: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling organization: setInstVarNames: superclass:methodDictionary:format:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('organization' category category: organization whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printNameOn: printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:libraryName:notifying: newInstanceFrom:variable:size:map:)
!


!Metaclass reorganize!
('accessing' allClassInstVarNames allInstances bindingOf: category localBindingOf: name soleInstance theMetaClass theNonMetaClass)
('accessing variables' allClassVarNames)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo:)
('class variables' classPool)
('compiling' acceptsLoggingOfCompilation possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('enumerating' allInstancesDo:)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: nonTrivial objectForDataStream: storeDataOn:)
('initialization' adoptInstance:from: instanceVariableNames:)
('instance variables' addInstVarName: removeInstVarName:)
('printing' printNameOn:)
('testing' canZapMethodDictionary isMeta isObsolete)
('SUnit tests' testCaseClass)
('copying' postCopy)
('instance creation' new)
!


!Collection reorganize!
('accessing' anyOne atRandom atRandom: size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: reject:thenCollect: reject:thenDo: select: select:thenCollect: select:thenDo: union:)
('find an element' detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin:)
('filter streaming' contents)
('inspecting' hasContentsInExplorer)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: at:ifPresent: at:ifPresent:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('assertions' assertIsInBounds:)
('comparing' = hasEqualElements: hash hashQuick hashQuick: is:subcollectionAt:)
('converting' @ asByteArray asColorArray asNewArray concatenation printStringWithNewline readStreamFrom:to: reverse reversed)
('copying' , copyAfter: copyAfterLast: copyBlank: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: grownTo: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: do: do:separatedBy: do:without: from:to:do: intervalOfSubCollection: keysAndValuesDo: polynomialEval: replace: reverseDo: reverseWith:do: select: upTo: with:allSatisfy: with:anySatisfy: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy:)
('enum. subcollections' collectSlidingWindow: combinations:atATimeDo: groupsOf:atATimeDo: pairsCollect: pairsDo: permutationsDo: slidingWindowDo: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('find an element' findFirst: findFirst:startingAt: findFirstBasicAt:startingAt: findLast:)
('find in sorted receiver' findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: quickFindFirst: quickFindLast:)
('gui' do:displayingProgress:)
('inspecting' customizeExplorerContents explorerContents)
('math functions' += -=)
('removing' remove:ifAbsent:)
('sorting' sorted:)
('stream creation' asStreamResult readStream thatCanBeModified writeStream)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds: isSequenceable)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
!


!ArrayedCollection reorganize!
('accessing' size)
('adding' add:)
('inserting' insert:shiftingRightAt:)
('inspecting' explorerContents)
('math functions' derivative incrementFractions integral successiveRatios)
('objects from disk' byteSize restoreEndianness writeOn:)
('printing' printNameOn: storeOn:)
('sorting' isSorted isSortedBy: mergeFirst:middle:last:into:by: mergeSortFrom:to:by: mergeSortFrom:to:src:dst:by: quickSortFrom:to:by: sort sort:)
('testing' isEmpty)
('private' defaultElement storeElementsFrom:to:on:)
!


!CompiledMethod reorganize!
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass endPC flag frameSize headerWithoutSignFlag initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: setSignFlag signFlag trailer withPropertiesDo:ifSelector:)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('closures' containsBlockClosures embeddedBlockClosures)
('comparing' = equivalentTo: hash)
('converting' asMethodReference asString)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: storeDataOn: zapSourcePointer)
('initialization' copyWithBytecodes: copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allImplicitLiteralsDo: allLiterals allLiteralsDo: allLiteralsDo:scanForSpecial: codeLiterals codeLiteralsDo: hasLiteral: hasLiteralSuchThat: header headerDescription implicitLiteralsDo: indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: printQuickOn:indent: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:withStamp:libraryName:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasLiteralExplicitly: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper usesClosureBytecodes writesInstanceVariable:)
('time stamp' author authorAndStamp authorAndStampAsSaved dateAndTime dateMethodLastSaved)
('tracing' outboundPointersDo:)
('user interface support' browse)
('private' flushCache penultimateLiteral penultimateLiteral: voidCogVMState)
('object serialization' readDataFrom:size:)
!


!Semaphore reorganize!
('communication' signal wait waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical: critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('testing' isSignaled)
('initialization' terminateProcess)
('printing' printOn:)
('comparing' = hash)
('initialize-release' initSignals)
('private' species)
!


!Semaphore class reorganize!
('instance creation' forMutualExclusion new)
!


!CharacterSequence reorganize!
('accessing' findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findStringCaseInsensitive:startingAt: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string subStrings: substringsSeparatedBy: utf8BytesSize)
('adding' add:)
('copying' , append: appendToString: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: copyReplacing:withAll: copyReplacing:withAll:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('comparing' < <= = > >= alike: beginsWith: commonPartWith:startAt:stopAt:applying: commonPrefixWith: endsWith: hash is:substringAt: match: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asByteString asCamelCase asDate asFileName asIdentifier: asLowercase asMonth asNumber asPlainString asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUnicodeString asUnicodeStringOrText asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil integerOrNilAt: keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: translated truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withDoubleColonsDoubled withDoubleColonsUndoubled withLineEndings: withNewLines withoutEnclosing: withoutLeadingBlanks withoutPossiblePrefix: withoutPossibleSuffix: withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect: collectInArray:)
('printing' print printAsLiteralOn: printOn: storeOn:)
('testing' isAlphaNumeric isLineEndAt: isRemote isSeparators isString)
('user interface' edit editLabel:)
('fileman-converting' asDirectoryEntry asDriveName asFileEntry asFullDirectoryEntry asFullFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('arithmetic' * + - / // \\)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue color: cyan gray green magenta red yellow)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom:)
('formatting' format:)
('uCompletion' separateKeywords)
('paragraph support' endOfParagraphBefore: indentationIfBlank:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('fileman-actions' fileContents fileContents:)
('Unicode normalization' asNFC asNFD canonicalComposition canonicalDecomposition)
('stream creation' writeStream)
!


!String reorganize!
('accessing' at: at:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyIn:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: replaceFrom:to:with:startingAt: size utf8BytesSize)
('comparing' < = caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' appendToString: copyReplaceFrom:to:with: copyWith:)
('converting' asByteString asByteStringIfAscii asCodePoints asHex asLowercase asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldAnyCodePoint translateToLowercase translateToUppercase uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('services' numArgs)
('testing' canHoldAnyCodePoint is: isAscii isByteString isEmpty isInCanonicalCompositionForm isIntegerDenotation)
('paragraph support' encompassLine: encompassParagraph:)
!


!String class reorganize!
('instance creation' crString crlfString fromUtf8Bytes: fromUtf8Bytes:trimLastNull: lfString new: new:withAll: newFrom: newLineString percentEscapingNonAscii: percentEscapingNonAsciiAndThoseIn:bytes: tab with: with:with: with:with:with: withAll:)
('primitives' translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('stream creation' writeStream)
('services' compare:with: compare:with:collated: compareIgnoreCase:with: findStringIgnoreCase:in:startingAt: findSubstring:in:startingAt:matchTable: is:equalTo: isAscii: string:lineIndicesDo: string:withLineEndings: substringsIn: translate:table: translateToLowercase: translateToUppercase:)
('accessing' findString:in:startingAt:)
('static state' basicCaseInsensitiveOrder basicCaseSensitiveOrder caseInsensitiveOrder caseSensitiveOrder lowercasingTable tokenishTable uppercasingTable)
('system' symbolClassToUse)
!


!Symbol reorganize!
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('printing' print storeOn:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
('comparing' =)
('copying' shallowCopy)
('private' errorNoModification flushCache species string:)
!


!Set reorganize!
('accessing' atRandom: like: size)
('adding' add: add:withOccurrences:)
('enumerating' collect: do: union:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: privateSwapArrayElement:with: rehash scanFor:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' includes: is: occurrencesOf:)
('inspecting' inspectorClass)
('comparing' = emptyCollectionHash)
('lookup' lookup:)
('copying' postCopy)
('object serialization' comeFullyUpOnReload:)
!


!Dictionary reorganize!
('accessing' associationAt: associationAt:ifAbsent: associationClass associations at: at:ifAbsent: at:ifAbsentOrNilPut: at:ifAbsentPut: at:ifPresent: at:ifPresent:ifAbsent: at:put: bindingOf: keyAtIdentityValue: keyAtIdentityValue:ifAbsent: keyAtValue: keyAtValue:ifAbsent: keys keysSortedSafely values)
('testing' hasBindingThatBeginsWith: includesKey: is: keyForIdentity: occurrencesOf:)
('adding' add: addAll:)
('removing' keysAndValuesRemove: remove: remove:ifAbsent: removeKey: removeKey:ifAbsent: removeUnreferencedKeys unreferencedKeys)
('enumerating' associationsDo: collect: do: keysAndValuesDo: keysDo: select: valuesDo:)
('printing' printElementsOn: storeOn:)
('private' errorKeyNotFound: errorValueNotFound keyAt: noCheckAdd: postCopy rehash scanFor:)
('inspecting' customizeExplorerContents explorerContents explorerContentsWithIndexCollect: inspectorClass)
('compiling' bindingsDo:)
('lookup' lookup:)
('comparing' = emptyCollectionHash hash)
('variable bindings' declare:from:)
!


!PositionableStream reorganize!
('reading' back contents contentsOfEntireFile crLfNextLine next: next:into: next:into:startingAt: nextAvailable: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('writing' next:putAll: next:putAll:startingAt:)
('testing' atEnd atStart isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: matchStart: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('gui' untilEnd:displayingProgress:)
('accessing' originalContents)
('mode' useBytes)
!


!PositionableStream class reorganize!
('instance creation' on:)
!


!WriteStream reorganize!
('reading' contents next next: nextWordsInto: peek upTo:delimiterIsTerminator:)
('writing' next:into:startingAt: next:putAll:startingAt: nextPut: nextPutAll: nextPutAllString:withAttributes: space)
('testing' isEmpty isWriteOnly)
('positioning' position: reset resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: writeSystemTimeStamp)
('accessing' size)
('printing' store:)
('private' braceArray braceArray: growTo: isCompatibleWithContents: on: pastEndPut: with: withAttribute:do: withAttributes:do:)
('services' padToEndIfCantTruncate)
!


!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' directedSuperSend:numArgs: doDup doNop doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver: closureCopy:copiedValues: executeMethod:forSelector:withArgs:receiver: jump pop push: quickSend:to:with:lookupIn: restart resume resume: resumeEvaluating: return return: return:to: returnEvaluating: runUntilErrorOrReturnFrom: send:to:with: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: finishIncompleteUnwindsUpTo: insertSender: isPrimFailToken: privSender: push:fromIndexable: runUntilReturnFrom: stackPtr stackp: stepToCalleeOrNil stepToSenderOrNil tryNamedPrimitiveIn:for:withArgs: unwindAndResume:evaluating: unwindAndStop unwindTo:safely: willFailReturn)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext unwindTo:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead isDeadOrAtEnd isDone secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
('instruction decoding implicit literals' methodReturnSpecialConstant: pushSpecialConstant: sendSpecial:numArgs:)
('read-only objects' modificationForbiddenFor:at:put: modificationForbiddenFor:instVarAt:put: simulatedObject:instVarAt:put:)
!


!MethodContext reorganize!
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasNonLocalReturn home isExecutingBlock method methodReturnContext outerContext outerContextsDo: receiver removeSelf tempAt: tempAt:put:)
('closure support' asContext)
('initialization' privRefresh privRefreshWith:)
('inspecting' inspectorClass)
('instruction decoding (closures)' blockReturnTop callPrimitive: pushConsArrayWithElements:)
('printing' printDetails: printOn: printString)
('system simulation' failPrimitiveWith: pushArgs:from:)
('testing' atEnd:)
('private' aboutToReturn:through: cannotReturn: endpc instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-exceptions' isHandlerOrSignalingContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
!


!Message reorganize!
('accessing' argument arguments fullName keywords lookupClass numArgs selector sends:)
('printing' printOn: storeOn:)
('private' lookupClass: setSelector: setSelector:arguments:)
('sending' sendTo: sentTo:)
('stub creation' addGetterCodeOn: addSetterCodeOn:with: argumentNameAt:havingNamed: createStubMethodFor: writeMessageNameOn: writeOn:argumentNameAt:havingNamed: writeShouldBeImplementedOn:)
('testing' hasArguments hasRightNumberOfArguments isGetterFor: isSetterFor:)
('comparing' = analogousCodeTo: hasIdenticalContentsAs: hash)
!


!Boolean reorganize!
('logical operations' & | eqv: not xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing' isLiteral printAsLiteralOn: storeOn:)
('user interface support' asMenuItemTextPrefix)
('copying' shallowCopy)
('testing' is:)
!


!False reorganize!
('logical operations' & | not xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing' printOn:)
('user interface support' asMenuItemTextPrefix)
!


!True reorganize!
('logical operations' & | not xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing' printOn:)
('user interface support' asMenuItemTextPrefix)
!


!Magnitude reorganize!
('comparing' < <= = > >= between:and: hash max: min: min:max:)
!


!Number reorganize!
('arithmetic' * + - / // \\ ^ abs div: mod: negated quo: reciprocal rem:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToInteger: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('interpolation' advanceFractionOf:towards: interpolateTo:at:)
('comparing' % = hash)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asFloat asFraction asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printAsLiteralOn: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even ifNotZero: is: isDivisibleBy: isFinite isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
!


!Float class reorganize!
('instance creation' basicNew basicNew: fromIEEE32Bit: fromIEEE64Bit: new new: readFrom: signBit:mantissaBits:exponentBits: signPart:mantissaPart:exponentPart:)
('constants' e emax emin epsilon fmax fmin fminDenormalized fminNormalized halfPi infinity ln10 ln2 maxExactInteger nan negativeInfinity negativeZero one pi precision radix twoPi zero)
('class initialization' initClassCachedState initialize releaseClassCachedState)
('testing' denormalized nativeWordOrdering)
!


!Integer reorganize!
('arithmetic' * + - / // alignedTo: crossSumBase: gcd: quo: reciprocalModulo: reciprocalModulo2: xgcd:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash hashMultiply)
('converting' adaptToFraction:andSend: asColorOfDepth: asFloat asFraction asHexDigit asInteger asIntegerOrFloat moduloTwoPiAsFloat)
('enumerating' timesRepeat:)
('inspecting' customizeExplorerContents explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog floorLog: ifMultipleOf2And5Do:otherwise: lcm: log log: log2 nthRoot: nthRootRounded: nthRootTruncated: productTo: raisedTo:modulo: raisedToFraction: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printAsLiteralOn: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:length:zeroPadded: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' digitAt: digitAt:put: digitLength lastDigit replaceFrom:to:with:startingAt:)
('testing' even is: isInteger isLarge isPowerOfTwo isPrime isProbablyPrime nextPrime)
('truncation and round off' atRandom atRandom: ceiling floor normalize roundedHAFZ roundedHTE truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryDigitBase montgomeryDigitLength montgomeryDigitMax montgomeryNumberOfDigits montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
!


!SmallInteger reorganize!
('arithmetic' * + - / // \\ gcd: quo:)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed lowBit)
('testing' even odd)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
('comparing' < <= = > >= ~= hashMultiply identityHash)
('copying' shallowCopy)
('converting' asFloat)
!


!LookupKey reorganize!
('accessing' canAssign key key: name)
('testing' isSpecialReadBinding isVariableBinding)
('bindings' beBindingOfType:announcing: beReadOnlyBinding beReadOnlyBindingAnnouncing: beReadWriteBinding beReadWriteBindingAnnouncing: recompileBindingsAnnouncing:)
('literals' allLiteralsDo:)
('comparing' < = hash)
('printing' printOn:)
!


!LookupKey class reorganize!
('instance creation' key:)
!


!Association reorganize!
('printing' printOn: storeOn:)
('objects from disk' objectForDataStream:)
('testing' analogousCodeTo: isSpecialWriteBinding literalEqual:)
('accessing' key:value: value value:)
('comparing' = hash)
!


!Association class reorganize!
('instance creation' key:value:)
!


!UndefinedObject reorganize!
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isEmptyOrNil isLiteral isNil notNil)
('dependents access' addDependent: suspend)
('class hierarchy' addSubclass: allSuperclassesUpTo: literalScannedAs:notifying: removeSubclass: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclasses subclassesDo: superclassNotValidErrorDescriptionFor: typeOfClass withAllSuperclassesDownFrom:)
('bottom context' canHandleSignal: handleSignal:)
('accessing instances and variables' whichClassDefinesInstanceVariable:ifNone:)
('copying' shallowCopy)
!


!Character class reorganize!
('class initialization' accentedLetters initialize initializeNamedCharactersMap uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' binarySelectorCharacters codePoint: digitValue: new separators value:)
('accessing untypeable characters' backQuote backspace cr delete doubleQuote end escape home insert lf newLineCharacter newPage noBreakSpace null pageDown pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)
('converting' evaluate:withUtf16WordsOf: evaluate:withUtf8BytesOf: evaluateOnce:withUtf8BytesOf: isValidUtf8: macRomanToLatin1: utf8BytesAndCodePointAt:in:into:into: utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into:assumeLatinIfInvalid: utf8BytesAndCodePointForIso8859s1:into:into: utf8BytesOf:)
('static state' basicLookupTables lowercaseMappingTable unaccentedTable uppercaseMappingTable)
('services' canonicalCompositionOf:and: canonicalDecomposition:on: combiningClass: isCombiningStarter: isDiacriticalMark:)
('constants' namedCharactersMap)
('private' constantNameFor: constantNames)
!

