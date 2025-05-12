'From Cuis7.3 [latest update: #7132] on 9 May 2025 at 3:48:27 pm'!

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 5/9/2025 15:42:40'!
sharedPools
	"Answer a Set of the names of the pools (Dictionaries) that the receiver 
	shares.
	9/12/96 tk  sharedPools have an order now"

	^ #()! !


!Object reorganize!
('[mk] accessing' at: at:put: basicAt: basicSize)
('[mk] class membership' class)
('[mk] comparing' = ~= hash)
('[mk] copying' copy postCopy shallowCopy)
('[mk] evaluating' value)
('[mk] message handling' perform: perform:with:)
('[mk] printing' printOn:)
('[mk] testing' is: isBehavior isByteString isCharacter isInteger isInterval isMethodProperties isNumber isString isSymbol isUnicodeString)
('[mk] system primitives' instVarAt:put:)
('[mkObjectStorage] object serialization' comeFullyUpOnReload: readDataFrom:size:)
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


!Behavior reorganize!
('[mk] accessing' definesInstanceVariableNamed: instSize instSpec instVarNames name theNonMetaClass typeOfClass)
('[mk] testing' isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isPointers isVariable isWeak isWords)
('[mk] instance creation' basicNew basicNew: new new:)
('[mk] accessing class hierarchy' superclass)
('[mk] accessing instances' allInstancesOrNil someInstance)
('[mk] enumerating' allSubclassesDo:)
('[mk] comparing' identityHash)
('accessing' classDepth decompilerClass format parserClass sourceCodeTemplate)
('testing' canZapMethodDictionary hasChangedComparedTo: isObsolete shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: initializedInstance)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instanceCount isInstanceVariableNamedReferencedInHierarchy: maxAccessibleInstSize maxInstSize sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allSubInstancesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteSubclassesCollection removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!


!ClassDescription reorganize!
('[mk] accessing' instVarNames theMetaClass theNonMetaClass)
('[mk] printing' printNameOn:)
('accessing' classVersion comment comment: comment:stamp: hasComment smalltalkComment)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: localBindingOf: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling superclass:methodDictionary:format:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
!


!Class reorganize!
('[mk] accessing' name)
('[mk] accesing class hierarchy' subclassesDo:)
('[mk] printing' printNameOn:)
('[mk] copying' copy)
('[mk] class variables' classPool)
('[mk] pool variables' sharedPools)
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


!Metaclass reorganize!
('[mk] accessing' allClassInstVarNames name theMetaClass theNonMetaClass)
('[mk] testing' isMeta)
('[mk] class hierarchy' subclassesDo:)
('[mk] printing' printNameOn:)
('[mk] class variables' classPool)
('accessing' allInstances category soleInstance)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses)
('compiling' acceptsLoggingOfCompilation bindingOf: localBindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
('SUnit tests' testCaseClass)
('accessing variables' allClassVarNames)
!


!Collection reorganize!
('[mk] accessing' size)
('[mk] enumerating' allSatisfy: anySatisfy: count: reject: select:)
('[mk] testing' includes: isSequenceable)
('[mk] private' species)
('accessing' anyOne atRandom atRandom:)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' associationsDo: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject:thenCollect: reject:thenDo: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('inspecting' hasContentsInExplorer)
!


!SequenceableCollection reorganize!
('[mk] accessing' indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: replaceFrom:to:with:)
('[mk] comparing' hasEqualElements: hashQuick:)
('[mk] converting' asByteArray)
('[mk] copying' , copyBlank: copyFrom:to: copyReplaceFrom:to:with: grownTo:)
('[mk] enumerating' do: select:)
('[mk] testing' includes: isSequenceable)
('[mk] stream creation' asStreamResult readStream thatCanBeModified writeStream)
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('comparing' = hash hashQuick)
('converting' @ asColorArray asNewArray concatenation printStringWithNewline readStreamFrom:to: reverse reversed)
('copying' copyAfter: copyAfterLast: copyFrom:count: copyReplaceAll:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findFirst:startingAt: findFirstBasicAt:startingAt: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: upTo: with:allSatisfy: with:anySatisfy: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('inspecting' customizeExplorerContents explorerContents)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -=)
('assertions' assertIsInBounds:)
('sorting' sorted:)
!


!ByteArray reorganize!
('[mk] access - Numeric Types' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: float32At:bigEndian: float32At:put:bigEndian: float64At:bigEndian: float64At:put:bigEndian: int16At:bigEndian: int16At:put:bigEndian: int32At:bigEndian: int32At:put:bigEndian: int64At:bigEndian: int64At:put:bigEndian: int8At:bigEndian: int8At:put:bigEndian: uint16At:bigEndian: uint16At:put:bigEndian: uint32At:bigEndian: uint32At:put:bigEndian: uint64At:bigEndian: uint64At:put:bigEndian: uint8At:bigEndian: uint8At:put:bigEndian:)
('[mk] converting' asString)
('[mk] system primitives' replaceFrom:to:with:startingAt:)
('accessing' atAllPut: bitSize indexOf:startingAt:ifAbsent: indexOfAnyIn:startingAt:ifAbsent:)
('converting' asByteArray asHex asUtf8BytesOrByteStringOrByteArray base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement)
('comparing' = hash)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' is: isLiteral)
('initialization' readHexFrom:)
!


!Symbol reorganize!
('[mk] comparing' =)
('[mk] printing' storeOn:)
('[mkSymbol] accessing' at:put: replaceFrom:to:with:startingAt:)
('[mkSymbol] converting' asSymbol)
('[mkSymbol] testing' isSymbol)
('[mkSymbol] copying' shallowCopy)
('[mkSymbol] private' species string:)
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
('[mk] accessing' size)
('[mk] testing' includes:)
('[mk] lookup' lookup:)
('[mk] copying' postCopy)
('[mkCollections] enumerating' do:)
('[mkCollections] private' atNewIndex:put: findElementOrNil: fullCheck grow growSize init: noCheckAdd: scanFor:)
('[mkObjectStorage] object serialization' comeFullyUpOnReload:)
('accessing' atRandom: like:)
('adding' add: add:withOccurrences:)
('enumerating' collect: union:)
('comparing' emptyCollectionHash)
('private' array fixCollisionsFrom: keyAt: privateSwapArrayElement:with: rehash)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = is: occurrencesOf:)
('inspecting' inspectorClass)
!


!Dictionary reorganize!
('[mk] lookup' lookup:)
('[mk] private' postCopy)
('[mkCollections] accessing' associationClass at: at:ifAbsent: at:ifPresent: at:put:)
('[mkCollections] enumerating' do: select:)
('[mkCollections] testing' includesKey:)
('[mkCollections] private' noCheckAdd: scanFor:)
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


!Stream reorganize!
('[mk] accessing' binary next nextPutAll:)
('[mk] normalized access' nextFloat32BigEndian: nextFloat32Put:bigEndian: nextFloat64BigEndian: nextFloat64Put:bigEndian: nextInt16BigEndian: nextInt16Put:bigEndian: nextInt32BigEndian: nextInt32Put:bigEndian: nextInt64BigEndian: nextInt64Put:bigEndian: nextUint16BigEndian: nextUint16Put:bigEndian: nextUint32BigEndian: nextUint32Put:bigEndian: nextUint64BigEndian: nextUint64Put:bigEndian:)
('[mk] nonhomogeneous accessing' nextString)
('accessing' basicNext contents flush localName next: next:put: nextMatchAll: nextPut: nextPut:when: nextPutAll:asCommaSeparated: nextPutAll:when: openReadOnly printOn: readOnly)
('testing' atEnd closed is: isReadOnly isWriteOnly nextUint32WordsPutAll:)
('enumerating' do:)
('printing' print: print:fractionDigits: print:when:)
('FileStream compatibility' close)
('nonhomogeneous accessing' nextNumber nextStringPut: nextUnicodeString nextUnicodeStringPut:)
!


!PositionableStream reorganize!
('[mk] accessing' next: next:into:startingAt: nextInto: originalContents)
('[mk] positioning' position position: reset)
('[mk] private' on:)
('[mk] mode' useBytes)
('accessing' back contents contentsOfEntireFile crLfNextLine next:into: next:putAll: next:putAll:startingAt: nextAvailable: nextInto:startingAt: nextKeyword nextLine nextWordsInto: peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('testing' atEnd atStart isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: matchStart: padTo:put: padToNextLongPut: resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies positionError setFrom:to:)
('filein/out' copyPreamble:oldPreamble:)
('gui' untilEnd:displayingProgress:)
!


!ReadStream reorganize!
('[mk] accessing' next next: next:into:startingAt:)
('accessing' next:putAll:startingAt: nextPut: nextPutAll: readInto:startingAt:count: readStream size upTo:delimiterIsTerminator: upToEnd)
('private' on:from:to:)
('file stream compatibility' localName)
('testing' isReadOnly)
!


!WriteStream reorganize!
('[mk] accessing' contents next:into:startingAt: nextPut: nextPutAll:)
('[mk] positioning' reset)
('[mk] private' isCompatibleWithContents: on:)
('accessing' next next: next:putAll:startingAt: nextPutAllString:withAttributes: nextWordsInto: peek size upTo:delimiterIsTerminator:)
('positioning' position: resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: timeStamp)
('printing' store:)
('private' braceArray braceArray: growTo: pastEndPut: with: withAttribute:do: withAttributes:do:)
('testing' isEmpty isWriteOnly)
('services' padToEndIfCantTruncate)
!


!DataStream reorganize!
('[mkObjectStorage] initialization' setStream:)
('[mkObjectStorage] reading' beginReference: getCurrentReference maybeBeginReference: next noteCurrentReference: objectAt: readArray readByteArray readCharacter readFalse readFloat readInstance readInteger readNil readReference readShortInst readShortRef readString readSymbol readTrue setCurrentReference:)
('[mkObjectStorage] aux' typeIDFor: vacantRef)
('write and read' beginInstance:size: nextPut: objectIfBlocked: outputReference: readBitmap readClass readMethod readUnicodeString readUnicodeSymbol readWordArray readWordLike replace:with: tryToPutReference:typeID: writeArray: writeBitmap: writeByteArray: writeCharacter: writeClass: writeFalse: writeFloat: writeInstance: writeInteger: writeNil: writeString: writeSymbol: writeTrue: writeUnicodeString: writeUnicodeSymbol: writeWordLike:)
('other' atEnd byteStream contents errorRead errorWriteReference: flush next: reset rootObject rootObject: setStream:reading: size)
!


!DataStream class reorganize!
('[mkObjectStorage] instance creation' on:)
('[mkObjectStorage] cached state access' initCachedState typeMap)
('instance creation' new)
('services' streamedRepresentationOf: unStream:)
('cached state access' releaseClassCachedState)
('examples' example exampleWithPictures testWith:)
!


!ReferenceStream reorganize!
('[mkObjectStorage] initialization' setStream:)
('[mkObjectStorage] reading' beginReference: getCurrentReference maybeBeginReference: next noteCurrentReference: objectAt: setCurrentReference:)
('[mkObjectStorage] aux' isAReferenceType:)
('writing' addSpecialReference: beginInstance:size: blockers blockers: nextPutWeak: objectIfBlocked: references replace:with: reset setStream:reading: tryToPutReference:typeID:)
('statistics' statisticsOfRefs)
!


!Boolean reorganize!
('[mk] logical operations' & | not)
('[mk] copying' shallowCopy)
('logical operations' eqv: xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('testing' is:)
('printing' isLiteral printAsLiteralOn: storeOn:)
('user interface support' asMenuItemTextPrefix)
!


!SmallFloat64 reorganize!
('[mk] copying' shallowCopy)
('[mkFloat] arithmetic' * + - /)
('[mkFloat] comparing' < <= = > >= ~= identityHash)
('[mkFloat] mathematical functions' ln timesTwoPower:)
('[mkFloat] truncation and round off' truncated)
('[mkFloat] accessing' exponent)
('mathematical functions' arcTan cos exp primSqrt profilerFriendlyPrimTimesTwoPower: profilerFriendlyTimesTwoPower: sin)
('accessing' fractionPart)
!


!SmallInteger reorganize!
('[mk] arithmetic' * + - // \\ quo:)
('[mk] comparing' < <= = > >= ~= identityHash)
('[mk] bit manipulation' bitAnd: bitOr: bitShift: bitXor: lowBit)
('[mk] testing' even)
('[mk] printing' decimalDigitLength printString)
('[mk] copying' shallowCopy)
('[mkFloat] converting' asFloat)
('arithmetic' / gcd:)
('bit manipulation' byteReversed hashMultiply)
('testing' isLarge odd)
('printing' numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
!


!UndefinedObject reorganize!
('[mk] testing' isNil notNil)
('[mk] copying' shallowCopy)
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
('[mk] copying' shallowCopy)
('accessing' asciiValue digitValue)
('comparing' < <= > >= collatesBefore: collatingCategory hash)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isAscii isDiacriticalMark isLineSeparator isNullCharacter isSeparator isSymbolLike isUnaccented isValidInBinarySelectors isValidInFilenames isValidInIdentifiers isValidStartOfIdentifiers isVowel shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting: tokenish)
('printing' hex isLiteral printAsLiteralOn: printOn: storeOn:)
('converting' asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!Point reorganize!
('[mk] copying' shallowCopy)
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

