'From Cuis 5.0 of 7 November 2016 [latest update: #3389] on 2 August 2018 at 4:49:36 pm'!

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 8/2/2018 16:49:19'!
hasReferencesToInstanceVariableNamed: anInstanceVariableName

	"Returns true if only self has one or more methods referencing anInstanceVariableName - Hernan"

	^(self whichSelectorsAccess: anInstanceVariableName) notEmpty! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 8/2/2018 16:48:53'!
isInstanceVariableNamedReferencedInHierarchy: anInstanceVariableName

	"Returns true if self or any subclass has one or more methods referencing anInstanceVariableName - Hernan"

	^self withAllSubclasses anySatisfy: [ :aClass | aClass hasReferencesToInstanceVariableNamed: anInstanceVariableName ]! !


!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 8/2/2018 16:03:04'!
allUnreferencedInstanceVariables

	"Return a list of the instance variables known to the receiver which are not referenced in the receiver or any of its subclasses OR superclasses"

	^ self allInstVarNames reject: [ :instanceVariableName | | definingClass |
		definingClass _ self classThatDefinesInstanceVariable: instanceVariableName.
		definingClass isInstanceVariableNamedReferencedInHierarchy: instanceVariableName ]! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 8/2/2018 15:59:42'!
unreferencedInstanceVariables
	"Return a list of the instance variables defined in the receiver which are not referenced in the receiver or any of its subclasses

	Object unreferencedInstanceVariables
	"

	^ self instVarNames reject: [ :instanceVariableName | self isInstanceVariableNamedReferencedInHierarchy: instanceVariableName ]
		! !


!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: subclasses superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allUnreferencedInstanceVariables allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames elementSize hasReferencesToInstanceVariableNamed: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' addTo:referencesTo:special:byte: allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('methods' closuresInfoAt: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
!
