'From Cuis 5.0 of 7 November 2016 [latest update: #3461] on 15 October 2018 at 10:06:14 pm'!

!Behavior methodsFor: 'as yet unclassified' stamp: 'HAW 10/15/2018 20:57:14'!
addMethodsTo: methodsReferencingLiteral thatReferenceTo: aLiteral special: specialFlag byte: specialByte
	
	| selectors |
	
	selectors _ self whichSelectorsReferTo: aLiteral special: specialFlag byte: specialByte.
	selectors do: [ :sel | methodsReferencingLiteral add: (MethodReference class: self selector: sel) ]! !

!Behavior methodsFor: 'as yet unclassified' stamp: 'HAW 10/15/2018 20:57:14'!
addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte.
	
	self withAllSuperAndSubclassesDoGently: [ :class |
		class addMethodsTo: aSet thatReferenceTo: aSymbol special: special byte: byte ] 
		! !

!Behavior methodsFor: 'as yet unclassified' stamp: 'HAW 10/15/2018 20:59:04'!
rejectSelectorsFrom: selectors thatReferenceTo: aLiteral byte: specialByte 

	"For special selectors, look for the literal in the source code.
	Otherwise, for example, searching for senders of #== will include senders of #ifNil.
	Except for #at:put:, because it has two arguments and won't find it in the source code like that."

	^ (specialByte isNil or: [ aLiteral = #at:put: ]) 
		ifTrue: [ selectors ]
		ifFalse: [ selectors select: [ :sel | ((self sourceCodeAt: sel) findString: aLiteral) > 0]]! !


!Behavior methodsFor: 'testing method dictionary' stamp: 'HAW 10/15/2018 21:56:17'!
whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal."

	| who |

	Preferences thoroughSenders 
		ifTrue: [ who _ self thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte ]
		ifFalse: [ 
			who _ Set new.
			self selectorsAndMethodsDo: [:sel :method |
				((method hasLiteral: literal) or: [specialFlag and: [method scanFor: specialByte]]) ifTrue: [
					((literal isVariableBinding) not or: [method sendsToSuper not
					"N.B. (method indexOfLiteral: literal) < method numLiterals copes with looking for
					Float bindingOf: #NaN, since (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)."
						or: [(method indexOfLiteral: literal) ~= 0]]) ifTrue: [who add: sel]]]].
		
	^self rejectSelectorsFrom: who thatReferenceTo: literal byte: specialByte ! !

!Behavior methodsFor: 'user interface' stamp: 'HAW 10/15/2018 20:58:17'!
allLocalCallsOn: aSymbol
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."

	| aSet special byte cls |
	
	aSet _ Set new.
	cls _ self theNonMetaClass.
	special _ Smalltalk hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | byte _ b ].
	
	cls addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte.
	cls class addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte.
	
	^aSet! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 10/15/2018 20:57:14'!
allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:."
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection special byte |

	#(23 48 'fred' (new open:label:)) size.
	"Example above should find #open:label:, though it is deeply embedded here."

	aCollection _ OrderedCollection new.
	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class | class addMethodsTo: aCollection thatReferenceTo: aLiteral special: special byte: byte ].
	
	^ aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 10/15/2018 21:07:56'!
numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	"
	| count specialFlag specialByte |

	count _ 0.
	specialFlag _ self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte _ b ].
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			((method hasLiteral: aSymbol) or: [specialFlag and: [(method scanFor: specialByte) and: [ ((class sourceCodeAt: sel) findString: aSymbol) > 0 ]]])
				ifTrue: [ count _ count + 1 ]]].
	^ count! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 10/15/2018 20:57:14'!
browseViewReferencesFromNonViews
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aLiteral aCollection |

	aCollection _ OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral _ view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) & false ifFalse: [
			Smalltalk allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					class addMethodsTo: aCollection thatReferenceTo: aLiteral special: false byte: nil ]]]].
	
	Smalltalk
		browseMessageList: aCollection asSet asArray sort
		name: 'References to Views from non-Views'
		autoSelect: ''.! !

!methodRemoval: Behavior #addMethodsTo:thatReferTo:special:byte:!
Behavior removeSelector: #addMethodsTo:thatReferTo:special:byte:!
!methodRemoval: Behavior #addTo:referencesInHierarchyTo:special:byte:!
Behavior removeSelector: #addTo:referencesInHierarchyTo:special:byte:!
!methodRemoval: Behavior #addTo:referencesTo:special:byte:!
Behavior removeSelector: #addTo:referencesTo:special:byte:!
!methodRemoval: Behavior #rejectSelectorsFrom:thatReferTo:byte:!
Behavior removeSelector: #rejectSelectorsFrom:thatReferTo:byte:!

!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: allSuperclassesUpToButNotIncluding: subclasses superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesUpTo: withAllSuperclassesUpToButNotIncluding:)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allUnreferencedInstanceVariables allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames elementSize hasReferencesToInstanceVariableNamed: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('methods' closuresInfoAt: methodNodeFor: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
('*Refactoring' definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: indexOfInstanceVariable: methodsSelect: whichClassDefinesInstanceVariable:ifNone: withSuperclassThatIncludesSelector:do:ifNone:)
('as yet unclassified' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: rejectSelectorsFrom:thatReferenceTo:byte:)
!


!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClass:from:to: renameClassNamed:as:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmParameterAt: vmParameterAt:put: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSnapshot:andQuit: nopTag okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitNoSaveTag quitPrimitive quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit: snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles datedVersion endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects hasSpecialSelector:ifTrueSetByte: primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate' getFileNameFromUser)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do:)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit isSpur maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: namesBeginningWith:do: namesBeginningWith:do:in:)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' doStartUp: processCommandLineArgument:storeStartUpScriptArgsOn: processCommandLineArguments readCommandLineArguments setStartupStamp startUpArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
!

