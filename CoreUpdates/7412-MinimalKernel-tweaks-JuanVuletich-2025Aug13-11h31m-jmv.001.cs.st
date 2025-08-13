'From Cuis7.5 [latest update: #7411] on 13 August 2025 at 11:41:22 am'!

!EncoderForSistaV1 class reorganize!
('[mkDynamicLibraryLoad] services' displaceInstVarAccessesIn:starting:by:doIt: remapInstVarAccessesIn:asPer:doIt: withLongInstVarAccessBytecodes: withLongInstVarAccessConversion01On: withLongInstVarAccessConversion02On: withLongInstVarAccessConversion03On:)
('compiled method support' bindingReadScanBlockFor:using: bindingWriteScanBlockFor:using: createClosureScanBlock extensionsAt:in:into: instVarReadScanBlockFor:using: instVarReadScanBlockUsing: instVarWriteScanBlockFor:using: instVarWriteScanBlockUsing: interpretJumpIfCondIn: interpretJumpIn: markerOrNilFor: pushReceiverScanBlock)
('instruction stream support' bytecodeSize: interpretNextInstructionFor:in: isBlockReturnAt:in: isBranchIfFalseAt:in: isBranchIfTrueAt:in: isExtension: isJumpAt:in: isJustPopAt:in: isReturnAt:in: isReturnSelfFromMethodAt:in: isReturnTopFromMethodAt:in: isSendAt:in: isStoreAt:in: isStorePopAt:in: isSuperSendAt:in: isSyntheticStoreAt:in:for: isTempStoreAt:in: nonExtensionBytecodeAt:in: nonLocalReturnScanBlockUsing: selectorToSendOrItselfFor:in:at: superSendScanBlockUsing:)
('bytecode decoding' callPrimitiveCode createClosureCode emptyClosurePattern pcOfBlockCreationBytecodeForBlockStartingAt:in: pushClosureBytecodeSize pushNewArrayCode pushNilCode stackDeltaForPrimitive:in: unusedBytecode)
('scanning' scanBlockOrNilForLiteral:)
('testing' canBeSpecialLiteral:)
('behavior support' maxAccessibleInstSize)
!


!EncoderForV3PlusClosures class reorganize!
('[mkDynamicLibraryLoad] services' displaceInstVarAccessesIn:starting:by:doIt: remapInstVarAccessesIn:asPer:doIt: withLongInstVarAccessBytecodes:)
('bytecode decoding' callPrimitiveCode createClosureCode emptyClosurePattern pcFollowingBlockAt:in: pcOfBlockCreationBytecodeForBlockStartingAt:in: pushClosureBytecodeSize pushNewArrayCode pushNilCode unusedBytecode)
('instruction stream support' bytecodeSize: extensionsAt:in:into: extensionsFor:in:into: interpretJumpIfCondIn: interpretJumpIn: interpretNextInstructionFor:in: isBlockReturnAt:in: isBranchIfFalseAt:in: isBranchIfTrueAt:in: isExtension: isJumpAt:in: isJustPopAt:in: isReturnAt:in: isReturnSelfFromMethodAt:in: isReturnTopFromMethodAt:in: isSendAt:in: isStoreAt:in: isStorePopAt:in: isSuperSendAt:in: isSyntheticStoreAt:in:for: isTempStoreAt:in: nonExtensionBytecodeAt:in: selectorToSendOrItselfFor:in:at:)
('compiled method support' bindingReadScanBlockFor:using: bindingWriteScanBlockFor:using: createClosureScanBlock instVarReadScanBlockFor:using: instVarReadScanBlockUsing: instVarWriteScanBlockFor:using: instVarWriteScanBlockUsing: markerOrNilFor: nonLocalReturnScanBlockUsing: pushReceiverScanBlock superSendScanBlockUsing:)
('scanning' scanBlockOrNilForLiteral:)
('testing' canBeSpecialLiteral:)
('behavior support' maxAccessibleInstSize)
!


!Behavior reorganize!
('[mk] accessing' definesInstanceVariableNamed: format instSize instSpec instVarNames name theNonMetaClass typeOfClass)
('[mk] testing' isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isPointers isVariable isWeak isWords)
('[mk] printing' printOn:)
('[mk] instance creation' basicNew basicNew: new new:)
('[mk] class hierarchy' deepSubclassNamed: superclass)
('[mk] instances' allInstancesOrNil someInstance)
('[mk] enumerating' allSubclassesDo:)
('[mk] comparing' identityHash)
('[mk] constants' maxInstSize)
('[mkClassAndMethodCreation] initialization' superclass:methodDictionary:format:)
('[mkClassAndMethodCreation] accessing' binding bindingOf: classBindingOf:)
('[mkClassAndMethodCreation] method dictionary' addSelectorSilently:withMethod: compiledMethodAt: includesSelector: lookupSelector: methodDict methodDict:)
('[mkClassAndMethodCreation] private' flushCache)
('[mkClassAndMethodCreation] instances and variables' maxAccessibleInstSize)
('[mkClassAndMethodCreation] copying' postCopy)
('[mkClassAndMethodCreation] testing class hierarchy' includesBehavior: inheritsFrom:)
('accessing' classDepth decompilerClass parserClass sourceCodeTemplate)
('testing' canZapMethodDictionary hasChangedComparedTo: isObsolete shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary)
('printing' literalScannedAs:notifying: longPrintOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: initializedInstance)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> allSelectors compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' kindOfSubclass)
('testing method dictionary' allUnsentMessages canUnderstand: hasMethods whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferExplicitlyTo: whichSelectorsReferTo: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allSubInstancesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteClassesCollection removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferExplicitlyTo: addMethodsTo:thatReferTo: addTo:methodsThatReferInHierarchyTo: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: stampAt:)
('system-support' allCallsOn)
('release' obsolete)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!

