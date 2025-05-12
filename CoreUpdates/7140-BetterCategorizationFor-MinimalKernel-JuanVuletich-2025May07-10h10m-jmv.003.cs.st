'From Cuis7.3 [latest update: #7129] on 7 May 2025 at 12:33:42 pm'!

!ProtoObject reorganize!
('[mk] initialization' initialize)
('[mk] testing' isNil notNil)
('[mk] comparing' == ~~ identityHash)
('[mk] error handling' error: warning:)
('[mk] system primitives' become: doesNotUnderstand: nextInstance)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isObject)
('comparing' scaledIdentityHash)
('system primitives' cannotInterpret: nextObject statePointsTo:)
('objects from disk' rehash)
('debugging' flag: withArgs:executeMethod:)
('apply primitives' tryPrimitive:withArgs:)
('tracing' pointsTo:)
!


!Object reorganize!
('[mk] accessing' at: at:put: basicAt: basicSize)
('[mk] class membership' class)
('[mk] comparing' = ~= hash)
('[mk] evaluating' value)
('[mk] message handling' perform: perform:with:)
('[mk] printing' printOn:)
('[mk] testing' is: isBehavior isByteString isCharacter isInteger isInterval isMethodProperties isNumber isString isSymbol isUnicodeString)
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


!Behavior reorganize!
('[mk] testing' isMeta)
('[mk] instance creation' basicNew basicNew: new new:)
('[mk] accessing instances' allInstancesOrNil someInstance)
('[mk] enumerating' allSubclassesDo:)
('[mk] comparing' identityHash)
('accessing' classDepth decompilerClass format instSpec name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('testing' canZapMethodDictionary hasChangedComparedTo: isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: initializedInstance)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instSize instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: maxAccessibleInstSize maxInstSize sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
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
('[mk] accessing' theNonMetaClass)
('[mk] printing' printNameOn:)
('accessing' classVersion comment comment: comment:stamp: hasComment smalltalkComment theMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: localBindingOf: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling superclass:methodDictionary:format:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing variables' allUnreferencedInstanceVariables)
('pool variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
!


!Class reorganize!
('[mk] accesing class hierarchy' subclassesDo:)
('[mk] printing' printNameOn:)
('accessing' name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarBindingOf: classVarNames classVarValueOf: ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools classPool classPool: classPoolFrom: removeSharedPool: sharedPools)
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
('[mk] accessing' isMeta theNonMetaClass)
('[mk] class hierarchy' subclassesDo:)
('[mk] printing' printNameOn:)
('accessing' allInstances category name soleInstance theMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses)
('compiling' acceptsLoggingOfCompilation bindingOf: localBindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
('SUnit tests' testCaseClass)
('accessing variables' allClassInstVarNames allClassVarNames)
!


!Collection reorganize!
('[mk] enumerating' allSatisfy: anySatisfy: count:)
('[mk] testing' includes:)
('[mk] private' species)
('accessing' anyOne atRandom atRandom: size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' associationsDo: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: reject:thenCollect: reject:thenDo: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('inspecting' hasContentsInExplorer)
!


!SequenceableCollection reorganize!
('[mk] accessing' indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: replaceFrom:to:with:)
('[mk] comparing' hashQuick:)
('[mk] converting' asByteArray asStreamResult thatCanBeModified writeStream)
('[mk] copying' , copyBlank: copyFrom:to: copyReplaceFrom:to:with: grownTo:)
('[mk] enumerating' do:)
('[mk] testing' includes:)
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOf:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('comparing' = hasEqualElements: hash hashQuick)
('converting' @ asColorArray asNewArray concatenation isSequenceable printStringWithNewline readStream readStreamFrom:to: reverse reversed)
('copying' copyAfter: copyAfterLast: copyFrom:count: copyReplaceAll:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findFirst:startingAt: findFirstBasicAt:startingAt: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:allSatisfy: with:anySatisfy: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('inspecting' customizeExplorerContents explorerContents)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -=)
('assertions' assertIsInBounds:)
('sorting' sorted:)
!


!SequenceableCollection class reorganize!
('[mk] stream creation' streamContents: streamContents:estimatedSize: streamContents:on:)
('stream creation' streamContents:limitedTo: writeStream)
!


!ArrayedCollection reorganize!
('[mk] accessing' size)
('adding' add:)
('printing' printNameOn: storeOn:)
('private' defaultElement storeElementsFrom:to:on:)
('sorting' isSorted isSortedBy: mergeFirst:middle:last:into:by: mergeSortFrom:to:by: mergeSortFrom:to:src:dst:by: quickSortFrom:to:by: sort sort:)
('objects from disk' byteSize restoreEndianness writeOn:)
('inserting' insert:shiftingRightAt:)
('testing' isEmpty)
('inspecting' explorerContents)
('math functions' derivative incrementFraction integral)
!


!ArrayedCollection class reorganize!
('[mk] instance creation' new: withAll:)
('instance creation' instancesMustBeOfSpecificSize new new:from: new:withAll: newFrom: newFromStream: numElements with: with:with: with:with:with: with:with:with:with: with:with:with:with:with: with:with:with:with:with:with: with:withAll:)
('stream creation' writeStream)
('aux' bytesPerBasicElement)
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


!ByteArray reorganize!
('[mk] converting' asString)
('[mk] system primitives' replaceFrom:to:with:startingAt:)
('accessing' atAllPut: bitSize indexOf:startingAt:ifAbsent: indexOfAnyIn:startingAt:ifAbsent:)
('access - Numeric Types' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: float32At:bigEndian: float32At:put:bigEndian: float64At:bigEndian: float64At:put:bigEndian: int16At:bigEndian: int16At:put:bigEndian: int32At:bigEndian: int32At:put:bigEndian: int64At:bigEndian: int64At:put:bigEndian: int8At:bigEndian: int8At:put:bigEndian: uint16At:bigEndian: uint16At:put:bigEndian: uint32At:bigEndian: uint32At:put:bigEndian: uint64At:bigEndian: uint64At:put:bigEndian: uint8At:bigEndian: uint8At:put:bigEndian:)
('converting' asByteArray asHex asUtf8BytesOrByteStringOrByteArray base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement)
('comparing' = hash)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' is: isLiteral)
('initialization' readHexFrom:)
!


!ByteArray class reorganize!
('[mk] byte based hash' hashBytes:startingWith: minSizeForHashQuick)
('[mk] services' findFirstInBytes:inSet:startingAt: indexOfByte:inBytes:startingAt:)
('instance creation' newBits: newFrom: readHexFrom:)
('stream creation' streamUtf8Contents:)
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


!Semaphore reorganize!
('[mk] communication' signal wait)
('[mk] mutual exclusion' critical:)
('[mk] comparing' = hash)
('[mk] initialize-release' initSignals)
('communication' waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('testing' isSignaled)
('initialization' terminateProcess)
('private' species)
('printing' printOn:)
!


!Semaphore class reorganize!
('[mk] instance creation' forMutualExclusion new)
!


!CharacterSequence reorganize!
('[mk] accessing' indexOf: string)
('[mk] printing' printOn: storeOn:)
('[mk] testing' isString)
('[mkSymbol] converting' asSymbol)
('accessing' findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOfSubCollection:startingAt:ifAbsent: lineCount lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: subStrings: substringsSeparatedBy: utf8BytesSize)
('adding' add:)
('copying' append: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: copyReplacing:withAll: copyReplacing:withAll:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('comparing' < <= = > >= alike: beginsWith: commonPartWith:startAt:stopAt:applying: commonPrefixWith: endsWith: hash is:substringAt: match: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asByteString asCamelCase asDate asFileName asIdentifier: asLowercase asMonth asNumber asPlainString asSmalltalkComment asString asText asUnHtml asUnaccented asUnicodeString asUnicodeStringOrText asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil integerOrNilAt: keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: translated truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withDoubleColonsDoubled withDoubleColonsUndoubled withNewLines withoutEnclosing: withoutLeadingBlanks withoutPossiblePrefix: withoutPossibleSuffix: withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect: collectInArray:)
('printing' print printAsLiteralOn:)
('testing' isAlphaNumeric isLineEndAt: isRemote isSeparators)
('user interface' edit editLabel:)
('fileman-converting' asDirectoryEntry asDriveName asFileEntry asFullDirectoryEntry asFullFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('arithmetic' * + - / // \\)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom:)
('formatting' format:)
('uCompletion' separateKeywords)
('paragraph support' endOfParagraphBefore: indentationIfBlank:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('fileman-actions' fileContents fileContents:)
('Unicode normalization' asNFC asNFD canonicalComposition canonicalDecomposition)
!


!CharacterSequence class reorganize!
('[mkSymbol] testing' representsSymbols)
('instance creation' fromUtf8Bytes: new new:withAll: numElements readFrom:)
('fileman-constants' pathSeparators)
!


!String reorganize!
('[mk] accessing' at: at:put: indexOf:startingAt:ifAbsent: replaceFrom:to:with:startingAt: size)
('[mk] comparing' = hash)
('[mk] copying' , appendToString: copyReplaceFrom:to:with:)
('[mk] converting' asUtf8Bytes)
('[mk] testing' is: isAscii isByteString)
('accessing' byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOfAnyIn:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: utf8BytesSize)
('comparing' < caseInsensitiveLessOrEqual: crc16 is:substringAt: sameAs:)
('copying' copyWith:)
('converting' asByteString asByteStringIfAscii asCodePoints asHex asLowercase asUppercase asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldAnyCodePoint translateToLowercase translateToUppercase uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('services' numArgs)
('testing' canHoldAnyCodePoint isEmpty isInCanonicalCompositionForm isIntegerDenotation)
('paragraph support' encompassLine: encompassParagraph:)
!


!String class reorganize!
('[mk] instance creation' new: with:)
('[mk] services' compare:with:collated: is:equalTo: isAscii:)
('[mk] static state' caseSensitiveOrder)
('[mkSymbol] system' symbolClassToUse)
('instance creation' crString crlfString fromUtf8Bytes: fromUtf8Bytes:trimLastNull: lfString new:withAll: newFrom: newLineString percentEscapingNonAscii: percentEscapingNonAsciiAndThoseIn:bytes: tab with:with: with:with:with: withAll:)
('primitives' translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('stream creation' writeStream)
('services' compare:with: compareIgnoreCase:with: findStringIgnoreCase:in:startingAt: findSubstring:in:startingAt:matchTable: string:lineIndicesDo: string:withLineEndings: substringsIn: translate:table: translateToLowercase: translateToUppercase:)
('accessing' findString:in:startingAt:)
('static state' basicCaseInsensitiveOrder basicCaseSensitiveOrder caseInsensitiveOrder lowercasingTable tokenishTable uppercasingTable)
!


!Symbol reorganize!
('[mk] comparing' =)
('[mk] printing' storeOn:)
('[mkSymbol] accessing' at:put: replaceFrom:to:with:startingAt:)
('[mkSymbol] converting' asSymbol)
('[mkSymbol] testing' isSymbol)
('[mkSymbol] private' species string:)
('accessing' precedence)
('copying' shallowCopy)
('converting' asPlainString asString capitalized numArgs: uncapitalized)
('printing' print)
('system primitives' flushCache)
('private' errorNoModification)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
!


!Symbol class reorganize!
('[mk] services' isLiteralSymbol:)
('[mkSymbol] instance creation' intern: lookup:)
('[mkSymbol] class initialization' initialize)
('[mkSymbol] services' estimatedCount)
('[mkSymbol] testing' representsSymbols)
('[mkSymbol] private' accessProtect basicFromString:)
('access' allSymbolTablesDo: allSymbols allSymbolsSelect: findInterned: selectorsContaining: selectorsMatching: symbolCount thatStartsCaseSensitive:)
('instance creation' newFrom: readFrom:)
('private' rehash)
('services' hasInterned:and: hasInterned:ifTrue: hasInternedAndImplemented: hasInternedAndImplementedOrReferenced: possibleSelectorsFor:)
!


!SystemDictionary reorganize!
('[mk] snapshot and quit' quitPrimitive:)
('[mk] special objects' specialObjectsArray)
('[mkVMnImage] primitives' primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('[mkVMnImage] miscelaneous' getSystemAttribute: listBuiltinModule: listLoadedModule: platformName)
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


!WeakSet reorganize!
('[mkSymbol] accessing' estimatedSize)
('[mkSymbol] adding and finding' add: lookup: lookup:ifAbsentAddFrom:)
('[mkSymbol] initialization' init:)
('[mkSymbol] private' basicAdd:withHash: basicLookup:withHash: bucketIndexFor: grow indexInBucketFor:)
('[mkSymbol] private-equality or identity' areEqual:and: basicLook:inIterablePartOf: hashFor:)
('[mkSymbol] tuning constants' bucketDesiredMeanOccupation bucketDirectAccessMask bucketDirectAccessSize bucketIterablePartGrow bucketIterablePartStart bucketsHashShift)
('accessing' size)
('adding and finding' add:withOccurrences: includes: occurrencesOf:)
('enumerating' collect: do: union:)
('comparing' emptyCollectionHash)
('copying' postCopy)
('objects from disk' comeFullyUpOnReload:)
('private' rehash rehash:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = is:)
!


!WeakSet class reorganize!
('[mkSymbol] instance creation' new newWithRoomForMoreThan:)
('instance creation' newFrom:)
('experiments - tuning' experiment01)
!


!Stream reorganize!
('[mk] accessing' nextPutAll:)
('accessing' basicNext binary contents flush localName next next: next:put: nextMatchAll: nextPut: nextPut:when: nextPutAll:asCommaSeparated: nextPutAll:when: openReadOnly printOn: readOnly)
('testing' atEnd closed is: isReadOnly isWriteOnly nextUint32WordsPutAll:)
('enumerating' do:)
('printing' print: print:fractionDigits: print:when:)
('FileStream compatibility' close)
('normalized access' nextFloat32BigEndian: nextFloat32Put:bigEndian: nextFloat64BigEndian: nextFloat64Put:bigEndian: nextInt16BigEndian: nextInt16Put:bigEndian: nextInt32BigEndian: nextInt32Put:bigEndian: nextInt64BigEndian: nextInt64Put:bigEndian: nextUint16BigEndian: nextUint16Put:bigEndian: nextUint32BigEndian: nextUint32Put:bigEndian: nextUint64BigEndian: nextUint64Put:bigEndian:)
('nonhomogeneous accessing' nextNumber nextString nextStringPut: nextUnicodeString nextUnicodeStringPut:)
!


!PositionableStream reorganize!
('[mk] accessing' nextInto: originalContents)
('[mk] positioning' position position: reset)
('[mk] private' on:)
('[mk] mode' useBytes)
('accessing' back contents contentsOfEntireFile crLfNextLine next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextInto:startingAt: nextKeyword nextLine nextWordsInto: peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('testing' atEnd atStart isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: matchStart: padTo:put: padToNextLongPut: resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies positionError setFrom:to:)
('filein/out' copyPreamble:oldPreamble:)
('gui' untilEnd:displayingProgress:)
!


!PositionableStream class reorganize!
('[mk] instance creation' on:)
!


!WriteStream reorganize!
('[mk] accessing' contents nextPut: nextPutAll:)
('[mk] positioning' reset)
('[mk] private' isCompatibleWithContents: on:)
('accessing' next next: next:into:startingAt: next:putAll:startingAt: nextPutAllString:withAttributes: nextWordsInto: peek size upTo:delimiterIsTerminator:)
('positioning' position: resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: timeStamp)
('printing' store:)
('private' braceArray braceArray: growTo: pastEndPut: with: withAttribute:do: withAttributes:do:)
('testing' isEmpty isWriteOnly)
('services' padToEndIfCantTruncate)
!


!StdIOWriteStream reorganize!
('[mk] streaming' flush nextPut: nextPutAll:)
('[mk] character writing' newLine space)
('[mk] open / close' openOnHandle:name:)
('[mk] primitives' primFlush: primWrite:from:startingAt:count:)
('[mk] printing' printOn:)
!


!StdIOWriteStream class reorganize!
('[mk] instance creation' stdout)
('[mk] accessing' stdioHandles stdoutHandle)
('instance creation' stderr)
('accessing' stderrHandle)
('cached state access' releaseClassCachedState)
!


!AdditionalMethodState reorganize!
('[mk] testing' isMethodProperties)
('[mk] accessing' selector)
('testing' analogousCodeTo: hasAtLeastTheSamePropertiesAs: hasLiteralSuchThat: hasLiteralThorough: includes: includesKey: isEmpty notEmpty)
('accessing' at: at:ifAbsent: at:ifAbsentPut: at:put: keysAndValuesDo: pragmas properties removeKey:ifAbsent: selector: setMethod:)
('copying' copyWith: copyWithout:)
('properties' includesProperty: propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: removeKey:)
('decompiling' method:)
('literals' allLiteralsDo:)
!


!BlockClosure reorganize!
('[mk] accessing' outerContext)
('[mk] evaluating' value value: value:value: value:value:value: valueNoContextSwitch valueWithPossibleArgument:)
('[mk] exceptions' ensure:)
('[mk] printing' printOn:)
('accessing' argumentCount capturedValues copiedValueAt: endpc home isBlock method numArgs numCopiedValues outerContextsDo: receiver size startpc)
('controlling' repeat repeatWithGCIf: whileFalse whileFalse: whileNil: whileNotNil: whileTrue whileTrue:)
('evaluating' bench durationToRun ifError: microsecondsToRun millisecondsToRun millisecondsToRunWithoutGC timeToRun value:value:value:value: valueAt: valueNoContextSwitch: valueWithArguments: valueWithExit valueWithPossibleArgs: valueWithPossibleArgument:and: valueWithin:onTimeout:)
('exceptions' assert ifCurtailed: on:do: on:do:on:do: on:do:on:do:on:do: onDNU:do:)
('printing' decompile printStack: storeOn:)
('scheduling' asContext fork forkAndWait forkAt: forkAt:named: forkNamed: newProcess newProcessForDebugging newProcessWith:)
('private' asContextWithSender: copyForSaving grabProcessor grabProcessorFor:onTimeout: grabProcessorOnlyFor: reentrant valueAndWaitWhileUnwinding: valueEnsured valueUnpreemptively)
('testing' hasNonLocalReturn isCleanClosure isClosure isDead sendsToSuper)
('error handing' handles: numArgsError:)
('events-support' asMinimalRepresentation isReceiverOrAnyArgumentGarbage)
('debugger access' sender)
('objects from disk' asSerializable objectForDataStream:)
('services' timeProfile)
('initialization' outerContext:startpc:numArgs:copiedValues:)
('system simulation' simulateValueWithArguments:caller:)
('lambda calculus' curried withFirstArg:)
!


!ContextPart reorganize!
('[mk] accessing' basicSize)
('[mk] debugger access' methodClass selector sender)
('[mk] printing' printOn:)
('accessing' at: at:put: basicAt: basicAt:put: client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
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


!MethodContext reorganize!
('[mk] accessing' method outerContext receiver tempAt:)
('[mk] printing' printOn:)
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasNonLocalReturn home isExecutingBlock methodReturnContext outerContextsDo: removeSelf tempAt:put:)
('closure support' asContext)
('initialization' privRefresh privRefreshWith:)
('inspecting' inspectorClass)
('instruction decoding (closures)' blockReturnTop callPrimitive: pushConsArrayWithElements:)
('printing' printDetails: printString)
('system simulation' failPrimitiveWith: pushArgs:from:)
('testing' atEnd:)
('private' aboutToReturn:through: cannotReturn: endpc instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-exceptions' isHandlerOrSignalingContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
!


!Message reorganize!
('[mk] accessing' selector)
('accessing' argument arguments fullName keywords lookupClass numArgs sends:)
('printing' printOn: storeOn:)
('private' lookupClass: setSelector: setSelector:arguments:)
('sending' sendTo: sentTo:)
('stub creation' addGetterCodeOn: addSetterCodeOn:with: argumentNameAt:havingNamed: createStubMethodFor: writeMessageNameOn: writeOn:argumentNameAt:havingNamed: writeShouldBeImplementedOn:)
('testing' hasArguments hasRightNumberOfArguments isGetterFor: isSetterFor:)
('comparing' = analogousCodeTo: hasIdenticalContentsAs: hash)
!


!Boolean reorganize!
('[mk] logical operations' & | not)
('logical operations' eqv: xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('copying' shallowCopy)
('testing' is:)
('printing' isLiteral printAsLiteralOn: storeOn:)
('user interface support' asMenuItemTextPrefix)
!


!False reorganize!
('[mk] logical operations' & | not)
('logical operations' xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing' printOn:)
('user interface support' asMenuItemTextPrefix)
!


!True reorganize!
('[mk] logical operations' & | not)
('logical operations' xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing' printOn:)
('user interface support' asMenuItemTextPrefix)
!


!Magnitude reorganize!
('[mk] comparing' < <= = > >= between:and: hash max: min:)
('comparing' min:max:)
!


!Number reorganize!
('[mk] arithmetic' // \\ negated)
('[mk] comparing' =)
('[mk] mathematical functions' raisedToInteger:)
('[mk] truncation and round off' ceiling floor)
('[mk] converting' adaptToInteger:andSend: asInteger)
('[mk] testing' is: isNumber isZero negative)
('[mk] intervals' to:do:)
('[mk] printing' printString)
('[mkFloat] converting' adaptToFloat:andSend:)
('arithmetic' * + - / ^ abs div: mod: quo: reciprocal rem:)
('comparing' % hash)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('truncation and round off' detentBy:atMultiplesOf:snap: fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFraction:andSend: adaptToPoint:andSend: asFloat asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('testing' even ifNotZero: isDivisibleBy: isFinite isInfinite isNaN odd positive sign strictlyPositive)
('intervals' to: to:by: to:by:do: to:count: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
!


!Number class reorganize!
('[mk] constants' one)
('instance creation' readFrom: readRemainderOf:from:base:withSign:)
('constants' zero)
!


!Float reorganize!
('[mkFloat] arithmetic' negated)
('[mkFloat] mathematical functions' reciprocalLogBase2)
('[mkFloat] converting' adaptToInteger:andSend: partBits: partValues: partValues:ifInfinite:ifNaN:)
('[mkFloat] testing' isInfinite isNaN isZero)
('[mkFloat] accessing' basicAt: exponentPart mantissaPart nextTowardsZero signPart significandAsInteger)
('[mkFloat] printing' absPrintExactlyOn:base: absPrintOn:base: printOn:base:)
('arithmetic' abs)
('comparing' hash isWithin:floatsFrom: literalEqual:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan: arcTanNonPrimitive copySignTo: cos cosh degreeCos degreeSin expNonPrimitive floorLog: lnNonPrimitive log log: log2 nthRoot: reciprocal reciprocalFloorLog: safeArcCos sinNonPrimitive sinh smoothIsAbsBelow: smoothStep sqrt sqrtNonPrimitive tan tanh)
('converting' adaptToFraction:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asMatching32BitSingleDecimalRepresentation asTrueFraction degreesToRadians inMinusPiToPiRange radiansToDegrees withNegativeSign)
('testing' isDenormalized isFinite isFloat isFloatOrFloatComplex isLiteral sign)
('accessing' at: at:put: basicAt:put: byteSize exponent exponentBits floatsAwayFrom: integerPart mantissaBits nextAwayFromZero predecessor roundedHAFZ roundedHTE signBit significand successor ulp)
('printing' absPrintOn:base:mantissaSignificantBits: hex printAsIEEE32BitPrecisionFloatOn:base: printAsLiteralOn: printOn:fractionDigits: storeOn:base:)
('intervals' to:count:do: to:hops:do:)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('inspecting' hasContentsInExplorer)
!


!Float class reorganize!
('[mkFloat] instance creation' signPart:mantissaPart:exponentPart:)
('[mkFloat] constants' fmin fminDenormalized one pi)
('[mkFloat] class initialization' initClassCachedState initialize)
('[mkFloat] testing' denormalized)
('instance creation' basicNew basicNew: fromIEEE32Bit: new new: readFrom: signBit:mantissaBits:exponentBits:)
('constants' e emax emin epsilon fmax fminNormalized halfPi infinity ln10 ln2 maxExactInteger nan negativeInfinity negativeZero precision radix twoPi zero)
('class initialization' releaseClassCachedState)
('testing' nativeWordOrdering)
!


!BoxedFloat64 reorganize!
('[mkFloat] arithmetic' * + - /)
('[mkFloat] comparing' < <= = > >= ~=)
('[mkFloat] mathematical functions' ln timesTwoPower:)
('[mkFloat] truncation and round off' truncated)
('[mkFloat] accessing' exponent)
('mathematical functions' arcTan cos exp primSqrt raisedTo: raisedToInteger: sin)
('testing' isDenormalized isFinite isInfinite isNaN sign)
('accessing and round off' fractionPart)
('printing' storeOn:base:)
!


!SmallFloat64 reorganize!
('[mkFloat] arithmetic' * + - /)
('[mkFloat] comparing' < <= = > >= ~= identityHash)
('[mkFloat] mathematical functions' ln timesTwoPower:)
('[mkFloat] truncation and round off' truncated)
('[mkFloat] accessing' exponent)
('mathematical functions' arcTan cos exp primSqrt profilerFriendlyPrimTimesTwoPower: profilerFriendlyTimesTwoPower: sin)
('accessing' fractionPart)
('copying' shallowCopy)
!


!Integer reorganize!
('[mk] arithmetic' * + - //)
('[mk] comparing' < <= = > >=)
('[mk] truncation and round off' ceiling floor)
('[mk] converting' asInteger)
('[mk] bit manipulation' highBit highBitOfMagnitude)
('[mk] testing' is: isInteger)
('arithmetic' / alignedTo: crossSumBase: gcd: quo: reciprocalModulo: reciprocalModulo2: xgcd:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: lowBit noMask:)
('comparing' hash hashMultiply)
('converting' adaptToFraction:andSend: asColorOfDepth: asFloat asFraction asHexDigit asIntegerOrFloat moduloTwoPiAsFloat)
('enumerating' timesRepeat:)
('inspecting' customizeExplorerContents explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog floorLog: ifMultipleOf2And5Do:otherwise: lcm: log log: log2 nthRoot: nthRootRounded: nthRootTruncated: productTo: raisedTo:modulo: raisedToFraction: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printAsLiteralOn: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:length:zeroPadded: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even isPowerOfTwo isPrime isProbablyPrime nextPrime)
('truncation and round off' atRandom atRandom: normalize roundedHAFZ roundedHTE truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryDigitBase montgomeryDigitLength montgomeryDigitMax montgomeryNumberOfDigits montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
!


!SmallInteger reorganize!
('[mk] arithmetic' * + - // \\ quo:)
('[mk] comparing' < <= = > >= ~= identityHash)
('[mk] bit manipulation' bitAnd: bitOr: bitShift: bitXor: lowBit)
('[mk] testing' even)
('[mk] printing' decimalDigitLength printString)
('[mkFloat] converting' asFloat)
('arithmetic' / gcd:)
('bit manipulation' byteReversed hashMultiply)
('testing' isLarge odd)
('copying' shallowCopy)
('printing' numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
!


!Association reorganize!
('[mk] accessing' value)
('[mk] comparing' = hash)
('accessing' key:value: value:)
('printing' printOn: storeOn:)
('objects from disk' objectForDataStream:)
('testing' analogousCodeTo: isSpecialWriteBinding literalEqual:)
!


!UndefinedObject reorganize!
('[mk] testing' isNil notNil)
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('copying' shallowCopy)
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
('accessing' asciiValue digitValue)
('comparing' < <= > >= collatesBefore: collatingCategory hash)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isAscii isDiacriticalMark isLineSeparator isNullCharacter isSeparator isSymbolLike isUnaccented isValidInBinarySelectors isValidInFilenames isValidInIdentifiers isValidStartOfIdentifiers isVowel shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting: tokenish)
('copying' shallowCopy)
('printing' hex isLiteral printAsLiteralOn: printOn: storeOn:)
('converting' asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!Character class reorganize!
('[mk] instance creation' binarySelectorCharacters codePoint: digitValue:)
('[mk] accessing untypeable characters' lf newLineCharacter space)
('class initialization' accentedLetters initialize initializeNamedCharactersMap uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' new separators value:)
('accessing untypeable characters' backQuote backspace cr delete doubleQuote escape home insert newPage noBreakSpace null pageDown pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow tab zeroWidthSpace)
('converting' evaluate:withUtf16WordsOf: evaluate:withUtf8BytesOf: evaluateOnce:withUtf8BytesOf: macRomanToLatin1: utf8BytesAndCodePointAt:in:into:into: utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into: utf8BytesAndCodePointForIso8859s1:into:into: utf8BytesOf:)
('static state' basicLookupTables lowercaseMappingTable unaccentedTable uppercaseMappingTable)
('services' canonicalCompositionOf:and: canonicalDecomposition:on: combiningClass: isCombiningStarter: isDiacriticalMark:)
('constants' namedCharactersMap)
('private' constantNameFor: constantNames)
!

