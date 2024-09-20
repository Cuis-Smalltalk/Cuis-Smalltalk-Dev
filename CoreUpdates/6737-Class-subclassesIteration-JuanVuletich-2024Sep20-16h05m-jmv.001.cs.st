'From Cuis7.1 [latest update: #6736] on 20 September 2024 at 4:11:01 pm'!

!Class methodsFor: 'enumerating' stamp: 'jmv 9/20/2024 16:08:28'!
allSubclassesSortedDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's subclasses.
	At each level, sort subclasses."

	self subclassesSortedDo: [ :cl | 
		aBlock value: cl.
		cl allSubclassesSortedDo: aBlock ].! !

!Class methodsFor: 'enumerating' stamp: 'jmv 9/20/2024 16:10:25'!
subclassesSortedDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses.
	Sort by name."

	subclasses notNil ifTrue: [
		((subclasses reject: [ :c | c isMeta ]) sort: [:c1 :c2 | c1 name < c2 name ]) do: aBlock ].! !

!methodRemoval: ClassDescription #subclassesSortedDo: stamp: 'jmv 9/20/2024 16:08:33'!
ClassDescription removeSelector: #subclassesSortedDo:!
!methodRemoval: ClassDescription #allSubclassesSortedDo: stamp: 'jmv 9/20/2024 16:08:36'!
ClassDescription removeSelector: #allSubclassesSortedDo:!
!methodRemoval: Behavior #subclassesSortedDo: stamp: 'jmv 9/20/2024 16:07:51'!
Behavior removeSelector: #subclassesSortedDo:!
!methodRemoval: Behavior #allSubclassesSortedDo: stamp: 'jmv 9/20/2024 16:07:22'!
Behavior removeSelector: #allSubclassesSortedDo:!

!Behavior reorganize!
('accessing' classDepth decompilerClass format name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('testing' canZapMethodDictionary hasChangedComparedTo: instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesDoAlt: allInstancesOrNil allSubInstancesDo: allSubInstancesDoAlt: allSubclassesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: localBindingOf: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling superclass:methodDictionary:format:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printNameOn: printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing variables' allUnreferencedInstanceVariables)
('pool variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
!


!Class reorganize!
('accessing' name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarBindingOf: classVarNames classVarValueOf: ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools classPool classPool: classPoolFrom: removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutHierarchy fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: hasSubclasses removeSubclass: subclasses subclassesDo: superclass:)
('private' setName: spaceUsed)
('initialization' declareClassVarString: declareClassVariables: declareSharedPoolString: declareSharedPools: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('SUnit tests' testCaseClass)
('printing' printNameOn:)
('enumerating' allSubclassesSortedDo: subclassesSortedDo:)
!


!Metaclass reorganize!
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo:)
('compiling' acceptsLoggingOfCompilation bindingOf: localBindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
('SUnit tests' testCaseClass)
('accessing variables' allClassInstVarNames allClassVarNames)
('printing' printNameOn:)
!

