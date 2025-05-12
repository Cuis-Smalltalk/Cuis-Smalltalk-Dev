'From Cuis7.3 [latest update: #7148] on 12 May 2025 at 5:06:45 pm'!

!ProtoObject methodsFor: '[mk] system primitives' stamp: 'jmv 5/12/2025 16:57:21'!
doesNotUnderstand: aMessage
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"
	| title context count |
	StdIOWriteStream stdout flush.
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Message not understood.'; newLine;
		nextPutAll: 'Receiver: '.

	"
	self class isMeta
		ifFalse: [ StdIOWriteStream stdout nextPutAll: 'a ' ]
		ifTrue: [ StdIOWriteStream stdout nextPutAll: 'class ' ].
	self class theNonMetaClass printNameOn: StdIOWriteStream stdout.
	"
	title := self class name.
	StdIOWriteStream stdout
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title.

	StdIOWriteStream stdout
		nextPutAll: ' selector: #';
		nextPutAll: aMessage selector; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit now.';
		newLine;
		flush.

	Smalltalk quitPrimitive: 127.! !


!Object methodsFor: '[mk] printing' stamp: 'jmv 5/12/2025 16:58:04'!
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver.
	String streamContents: [ :strm | Object new printOn: strm ]
	"
	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title.! !


!Behavior reorganize!
('[mk] accessing' definesInstanceVariableNamed: instSize instSpec instVarNames name theNonMetaClass typeOfClass)
('[mk] printing' printOn:)
('[mk] testing' isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isPointers isVariable isWeak isWords)
('[mk] instance creation' basicNew basicNew: new new:)
('[mk] accessing class hierarchy' superclass)
('[mk] accessing instances' allInstancesOrNil someInstance)
('[mk] enumerating' allSubclassesDo:)
('[mk] comparing' identityHash)
('accessing' classDepth decompilerClass format parserClass sourceCodeTemplate)
('testing' canZapMethodDictionary hasChangedComparedTo: isObsolete shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
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
('[mk] printing' printNameOn: printOn:)
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
('printing' classVariablesString instanceVariablesString printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
!

