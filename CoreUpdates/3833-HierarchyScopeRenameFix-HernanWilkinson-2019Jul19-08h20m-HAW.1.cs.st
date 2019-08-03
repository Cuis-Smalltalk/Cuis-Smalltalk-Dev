'From Cuis 5.0 [latest update: #3826] on 3 August 2019 at 9:21:49 am'!

!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 8/3/2019 09:16:52'!
highestClassImplementing: aSelector ifNone: aNoneBlock

	| highestImplementorClass |

	self withAllSuperclassesDo: [ :aBehavior | (aBehavior includesSelector: aSelector) ifTrue: [ highestImplementorClass := aBehavior ]].
	
	^ highestImplementorClass ifNil: aNoneBlock ! !


!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 8/3/2019 09:13:18'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass doingPerClass: aBlock

	| highestClassImplementingOldSelector |
	
	highestClassImplementingOldSelector := aClass highestClassImplementing: anOldSelector ifNone: [ aClass ].
	highestClassImplementingOldSelector theNonMetaClass withAllSubAndSuperclassesDo: [ :aPotentialClassToRefactor |
		aPotentialClassToRefactor isMeta ifFalse: [ 
			self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor.
			aBlock value: aPotentialClassToRefactor ]]
	! !


!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesPreviousTo: allSuperclassesUpTo: subclasses superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesPreviousTo: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allUnreferencedInstanceVariables allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
('*LiveTyping' indexOfInstanceVariable:ifAbsent: initializeMethodsTypeInformation isSubclassesEmpty methodVariablesTypesOf: typeCheck withAllSubclassesImplementing:)
!

