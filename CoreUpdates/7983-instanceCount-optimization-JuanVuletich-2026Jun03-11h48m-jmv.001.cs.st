'From Cuis7.9 [latest update: #7982] on 3 June 2026 at 11:51:57 am'!

!Behavior methodsFor: 'accessing instances' stamp: 'jmv 2/Jun/2026 17:37:12'!
instanceCount
	"Answer the number of instances of the receiver that are currently in use."

	^self allInstances size! !


!Behavior reorganize!
('accessing' binding bindingOf: classBindingOf: classDepth decompilerClass definesInstanceVariableNamed: instSize instSpec instVarNames name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
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
('printing' literalScannedAs:notifying: longPrintOn: printWithClosureAnalysisOn: storeLiteral:on:)
('release' obsolete)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('system-support' allCallsOn)
('testing' canZapMethodDictionary hasChangedComparedTo: isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isImmediateClass isMeta isObsolete isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages canUnderstand: hasMethods whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferExplicitlyTo: whichSelectorsReferTo: whichSelectorsStoreInto:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: handleFailingBasicNew handleFailingBasicNew: handleFailingBasicPinnedNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: handleFailingFailingBasicPinnedNew: handleFailingFailingUninitializedPinnedNew: handleFailingUninitializedPinnedNew: setFormat: spaceUsed)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('initialization' superclass:methodDictionary:format:)
('instances and variables' maxAccessibleInstSize)
('[mk] accessing' format)
('[mk] testing' isFixed isPointers)
('[mk] printing' printOn:)
('[mkClassAndMethodCreation] private' flushCache)
!

