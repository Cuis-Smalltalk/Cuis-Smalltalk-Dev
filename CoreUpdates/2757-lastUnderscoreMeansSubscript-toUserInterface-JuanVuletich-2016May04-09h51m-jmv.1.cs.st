'From Cuis 4.2 of 25 July 2013 [latest update: #2756] on 4 May 2016 at 9:51:54.472348 am'!

!Behavior methodsFor: 'user interface' stamp: 'jmv 5/4/2016 09:51'!
lastUnderscoreMeansSubscript
	"Redefine this method on the class side of those classes where you want
	a_1 to be shown as 'a subscript 1'"

	^ false! !


!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isFixed isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSuperclasses subclasses superclass superclass: withAllSubclasses withAllSuperclasses)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allowsSubInstVars classVarNames inspectAllInstances inspectSubInstances instVarNames instanceCount sharedPools someInstance)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: allUnreferencedInstanceVariables crossReference lastUnderscoreMeansSubscript unreferencedInstanceVariables withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('methods' closuresInfoAt: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
!

