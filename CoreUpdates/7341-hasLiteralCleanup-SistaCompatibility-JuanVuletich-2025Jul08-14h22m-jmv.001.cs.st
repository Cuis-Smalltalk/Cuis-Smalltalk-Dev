'From Cuis7.5 [latest update: #7340] on 8 July 2025 at 2:29:39 pm'!

!Behavior methodsFor: 'testing method dictionary' stamp: 'jmv 7/7/2025 16:00:14'!
whichSelectorsReferExplicitlyTo: literal
	"Answer a set of selectors whose methods access the argument as a literal,
	and refer explicitly to in Source Code.
	Useful for finding senders of selectors.
	See #whichSelectorsReferTo:."

	| who |
	who := Set new.
	self selectorsAndMethodsDo: [ :sel :method |
		(method hasLiteralExplicitly: literal)
			ifTrue: [
				who add: sel]].
	^who
	"
	Rectangle whichSelectorsReferExplicitlyTo: #==.
	"! !

!Behavior methodsFor: 'methods' stamp: 'jmv 7/8/2025 14:28:23'!
addMethodsTo: methodsReferencingLiteral thatReferExplicitlyTo: aLiteral
	
	| selectors |
	
	selectors := self whichSelectorsReferExplicitlyTo: aLiteral.
	selectors do: [ :sel | methodsReferencingLiteral add: (MethodReference class: self selector: sel) ]! !

!Behavior methodsFor: 'methods' stamp: 'jmv 7/8/2025 14:26:05'!
addMethodsTo: methodsReferencingLiteral thatReferTo: aLiteral

	(self whichSelectorsReferTo: aLiteral)
		do: [ :sel |
			methodsReferencingLiteral add: (MethodReference class: self selector: sel) ]! !

!Behavior methodsFor: 'methods' stamp: 'jmv 7/8/2025 14:28:51'!
addTo: aSet methodsThatReferInHierarchyTo: aSymbol
	
	self withAllSuperAndSubclassesDo: [ :class |
		class addMethodsTo: aSet thatReferExplicitlyTo: aSymbol ] 
		! !


!CompiledMethod methodsFor: 'testing' stamp: 'jmv 7/7/2025 15:45:36'!
hasLiteralExplicitly: aLiteral
	"Similar to #hasLiteral:
	But do not include methods that (for example) #hasLiteral: #==
	only because the Compiler implements #ifNil: that way.
	See #hasLiteral:.
	Useful for finding senders of selectors."

	| canBeSpecialLiteral |
	canBeSpecialLiteral := self encoderClass canBeSpecialLiteral: aLiteral.

	"If we don't hasLiteral: it, just answer false."
	(self
		hasLiteralSuchThat: [:lit | lit literalEqual: aLiteral]
		scanForSpecial: canBeSpecialLiteral)
			ifFalse: [ ^false ].

	"Any literal but these appears for sure in source code, so it appears explicitly."
	canBeSpecialLiteral
		ifFalse: [ ^true ].

	"#at:put: is the only special selector that can't be found with #findString:
	But, it is also not generated implicitly by the Compiler.
	So we know it is actually sent in source code anyway."
	aLiteral = #at:put: ifTrue: [
		^true ].

	"Other special selectors, like #== must be found in source code to be considered."
	^(self getSource findString: aLiteral) > 0! !


!MethodSet methodsFor: 'filtering' stamp: 'jmv 7/7/2025 15:57:33'!
filterToNotSendersOf
	"Filter the receiver's list down to only those items which do not send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol |
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) isNil or:
							[(aMethod hasLiteralExplicitly: aSymbol) not]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 7/7/2025 15:57:37'!
filterToSendersOf
	"Filter the receiver's list down to only those items which send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) notNil and:
							[aMethod hasLiteralExplicitly: aSymbol]]]]! !


!Behavior methodsFor: 'testing method dictionary' stamp: 'jmv 7/7/2025 16:00:05'!
whichSelectorsReferTo: literal
	"Answer a set of selectors whose methods access the argument as a literal.
	Useful for finding references to literals.
	See #whichSelectorsReferExplicitlyTo:."

	| who |
	who := Set new.
	self selectorsAndMethodsDo: [ :sel :method |
		(method hasLiteral: literal)
			ifTrue: [
				who add: sel]].
	^who
	"
	Rectangle whichSelectorsReferTo: #==.
	"! !

!Behavior methodsFor: 'user interface' stamp: 'jmv 7/8/2025 14:28:57'!
allLocalCallsOn: aSymbol
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."

	| aSet cls |
	
	aSet := Set new.
	cls := self theNonMetaClass.
	
	cls addTo: aSet methodsThatReferInHierarchyTo: aSymbol.
	cls class addTo: aSet methodsThatReferInHierarchyTo: aSymbol.
	
	^aSet! !


!CompiledMethod methodsFor: 'literals' stamp: 'eem 7/8/2024 15:15'!
hasLiteral: aLiteral
	"Overwrite this method to invoke the bytecode encoder scanner explicitly.
	 This might be removed if there would be a way to enumerate special literals in a compiled-code object.
	See #allLiteralsDo:.
	See #hasLiteralExplicitly:. Prefer this for finding senders of selectors."

	^ self
		hasLiteralSuchThat: [:lit | lit literalEqual: aLiteral]
		scanForSpecial: (self encoderClass canBeSpecialLiteral: aLiteral)! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/8/2025 14:28:34'!
allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:."
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection |

	#(23 48 'fred' (new open:label:)) size.
	"Example above should find #open:label:, though it is deeply embedded here."

	aCollection := OrderedCollection new.
	self allBehaviorsDo: [ :class |
		class addMethodsTo: aCollection thatReferExplicitlyTo: aLiteral ].
	
	^ aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/7/2025 15:37:33'!
allCallsOn: firstLiteral and: secondLiteral
	"Answer a SortedCollection of all the methods that call on both aLiteral
	and secondLiteral."

	| aCollection secondArray |
	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :class |
		secondArray := class
			whichSelectorsReferExplicitlyTo: secondLiteral.
				((class whichSelectorsReferExplicitlyTo: firstLiteral ) select: [ :aSel |
			(secondArray includes: aSel)]) do: [ :sel |
				aCollection add: (MethodReference class: class selector: sel )]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/8/2025 14:26:13'!
allReferencesToLiteral: aLiteral
	| coll |
	coll := OrderedCollection new.
	self allBehaviorsDo: [ :eaClass |
		eaClass
			addMethodsTo: coll
			thatReferTo: aLiteral ]..
	^ coll.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/7/2025 16:37:36'!
isThereAReferenceTo: aLiteral
	"Answer a Collection of all the methods that call on aLiteral."
	"
	Smalltalk isThereAReferenceTo: #open:label:
	"

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			(method hasLiteral: aLiteral)
				ifTrue: [ ^true ]]].
	^ false! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/7/2025 16:19:53'!
numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	Note: For some selectors, like #==, this method will give an incorrect answer, much larger than the correct one.
	The correct answer would require calling the more expensive #hasLiteralExplicitly: method.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	"
	| count |
	count := 0.
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			"
			(method hasLiteralExplicitly: aSymbol)
			"
			(method hasLiteral: aSymbol)
				ifTrue: [ count := count + 1 ]]].
	^ count! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 7/8/2025 14:26:22'!
browseViewReferencesFromNonViews
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aLiteral aCollection |

	aCollection := OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral := view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) & false ifFalse: [
			self allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					class addMethodsTo: aCollection thatReferTo: aLiteral ]]]].
	
	self
		browseMessageList: aCollection asSet
		name: 'References to Views from non-Views'.! !


!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'jmv 7/7/2025 14:55:57'!
askAndAddSenderOf: classOfSenderToAdd 
		
	| senderSelector senderToAdd |
	
	senderSelector := self request: 'Selector of sender of #', self oldSelector initialAnswer: '' orCancel: [^self ].
	senderToAdd := classOfSenderToAdd 
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].
		
	(senderToAdd sendsSelector: self oldSelector) ifFalse: [
		^self inform: senderToAdd classAndSelector, ' does not refer to #', self oldSelector ].
	
	self addToList: senderToAdd! !


!InlineMethodUsagesStepWindow methodsFor: 'actions' stamp: 'jmv 7/7/2025 14:55:54'!
askAndAddAllUsagesInSenderOf: classOfSenderToAdd

	"Adds all usages of the selected sender to the usages to refactor"
	| senderSelector senderToAdd |

	senderSelector := self request: 'Selector of sender of #', self selectorToInline initialAnswer: '' orCancel: [^self ].
	senderToAdd := classOfSenderToAdd
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].

	(senderToAdd sendsSelector: self selectorToInline) ifFalse: [
		^self inform: senderToAdd classAndSelector, ' does not refer to #', self selectorToInline].

	(applier refactoringClass findReferencesToSelector: self selectorToInline in: senderToAdd asMethodReference)
		do: [:aMessageNodeReference | self addToList: aMessageNodeReference]! !


!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'jmv 7/7/2025 14:55:40'!
addToSendersIfOldSelectorIsSentIn: newImplementor
	
	(newImplementor sendsSelector: oldSelector) ifTrue: [
		senders add: newImplementor ].! !


!ChangeSelector class methodsFor: 'pre-conditions' stamp: 'jmv 7/7/2025 14:55:31'!
assertAllSenders: senders send: aSelector

	| invalidSenders |
	
	invalidSenders := senders reject: [ :aSender | aSender sendsSelector: aSelector ].
	invalidSenders notEmpty ifTrue: [ self signalInvalidSenders: invalidSenders of: aSelector ].! !

!methodRemoval: SystemDictionary #hasSpecialSelector:ifTrueSetByte: stamp: 'jmv 7/8/2025 14:22:56'!
SystemDictionary removeSelector: #hasSpecialSelector:ifTrueSetByte:!
!methodRemoval: CompiledMethod #refersToLiteral: stamp: 'jmv 7/8/2025 14:22:56'!
CompiledMethod removeSelector: #refersToLiteral:!
!methodRemoval: CompiledMethod #sendsOrRefersTo: stamp: 'jmv 7/8/2025 14:22:56'!
CompiledMethod removeSelector: #sendsOrRefersTo:!
!methodRemoval: Behavior #addMethodsTo:thatReferenceTo: stamp: 'jmv 7/8/2025 14:26:31'!
Behavior removeSelector: #addMethodsTo:thatReferenceTo:!
!methodRemoval: Behavior #addTo:methodsThatReferenceInHierarchyTo:special:byte: stamp: 'jmv 7/8/2025 14:23:06'!
Behavior removeSelector: #addTo:methodsThatReferenceInHierarchyTo:special:byte:!
!methodRemoval: Behavior #addTo:methodsThatReferenceInHierarchyTo: stamp: 'jmv 7/8/2025 14:28:59'!
Behavior removeSelector: #addTo:methodsThatReferenceInHierarchyTo:!
!methodRemoval: Behavior #addMethodsTo:thatReferenceTo:special:byte: stamp: 'jmv 7/8/2025 14:23:13'!
Behavior removeSelector: #addMethodsTo:thatReferenceTo:special:byte:!
!methodRemoval: Behavior #addMethodsTo:thatReferenceExplicitlyTo: stamp: 'jmv 7/8/2025 14:28:38'!
Behavior removeSelector: #addMethodsTo:thatReferenceExplicitlyTo:!
!methodRemoval: Behavior #whichSelectorsReferTo:special:byte: stamp: 'jmv 7/8/2025 14:23:19'!
Behavior removeSelector: #whichSelectorsReferTo:special:byte:!

!CompiledMethod reorganize!
('[mk] accessing' methodClass numLiterals selector withPropertiesDo:ifSelector:)
('[mk] literals' header literalAt: objectAt:)
('[mk] printing' printClassAndSelectorOn: printOn:)
('[mk] converting' asString)
('[mk] testing' is:)
('[mk] private' penultimateLiteral)
('[mkObjectSerialization] object serialization' readDataFrom:size:)
('[mkClassAndMethodCreation] accessing' endPC initialPC methodClass:)
('[mkClassAndMethodCreation] literals' literalAt:put: objectAt:put:)
('[mkClassAndMethodCreation] private' flushCache penultimateLiteral: voidCogVMState)
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass flag frameSize methodClassAssociation methodClassAssociation: methodReference numArgs numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector: setSignFlag signFlag trailer)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('closures' containsBlockClosures embeddedBlockClosures)
('comparing' = equivalentTo: hash)
('converting' asMethodReference)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: storeDataOn: zapSourcePointer)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allImplicitLiteralsDo: allLiterals allLiteralsDo: allLiteralsDo:scanForSpecial: codeLiterals codeLiteralsDo: hasLiteral: hasLiteralSuchThat: headerDescription implicitLiteralsDo: indexOfLiteral: literalStrings literals literalsDo: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasLiteralExplicitly: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper usesClosureBytecodes writesInstanceVariable:)
('time stamp' author dateAndTime dateMethodLastSaved timeStamp)
('tracing' outboundPointersDo:)
('user interface support' browse)
!


!SystemDictionary reorganize!
('[mk] vm parameters' vmParameterAt: wordSize)
('[mk] snapshot and quit' quitPrimitive:)
('[mk] printing' printOn:)
('[mkHashedCollections] globals' at:put: flushClassNameCache)
('[mkHashedCollections] system attributes' maxIdentityHash)
('[mkVMnImage] primitives' primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('[mkVMnImage] miscelaneous' cachedSystemAttribute: getSystemAttribute: listBuiltinModule: listLoadedModule: platformName primGetSystemAttribute:)
('[mkVMnImage] special objects' specialObjectsArray)
('[mkClassAndMethodCreation] miscelaneous' voidCogVMState)
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: declare:)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 undeclaredReferences verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imagePath primImageName: userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModules listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformSubtype unloadModule: vmFileName vmOptionsDo: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allImplementorsOf:subTo: allImplementorsOf:superTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithSourceString:matchCase:scopeClass:includeSubclasses:includeSuperclasses: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSelect:localTo: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers rootClasses unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit filenameForUpdatedImage: lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitTag readAndApplyUserPrefs removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs: saveAs:clearAllClassState: saveImage saveRollingReleaseAndQuit saveStableRelease send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: logToUserChanges:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: versionString)
('special objects' clearExternalObjects externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: confirmRemovalOf:on:andSubclasses: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing - main' browse browseFull: browseFull:selector: browseHierarchy: browseHierarchy:selector: browserWindowClass findAndBrowseClass findInSourceCode)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllClasses browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOf:subTo: browseAllImplementorsOf:superTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:localTo: browseAllSelect:name:autoHighlight: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqEqSentToCharacterConstants browseEqEqSentToNumericConstants browseEqEqSentToSmallIntegerConstants browseEqSentToCharacterConstants browseEqSentToNumericConstants browseEqSentToSmallIntegerConstants browseInstVarRefs: browseLikelyUnneededRedefinitions browseMessageList:name: browseMessageList:name:autoHighlight:allOccurrences: browseMessageListUnsorted:name:autoHighlight: browseMethodsWithSourceString: browseMethodsWithSourceString:in: browseMethodsWithSourceString:scopeClass:includeSubclasses:includeSuperclasses: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseSendersOf:notTo: browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit processHasThreadIdInstVar: processPreemptionYields processPreemptionYields: processSuspensionUnblocks)
('query' classesWithExtensionCategories hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' checkIfAlreadyRunningOrStoppedNoExit displayCommandLineUsageOn: displaySystemInformationStringOn: doStartUp: lastCommandLineArguments logStartupDebugAid: logStartupDebugAids logStartupError: logStartupNotice: processCommandLineArguments: processFinalCommandLineOption:optionArgument:raiseExceptions: processInitialCommandLineOption:optionArgument: readCommandLineArguments setStartupStamp startUpArguments startUpScriptAllArguments startUpScriptArguments)
('startup - recover user changes' warnAndOfferLastUserChanges)
('startup - restore lost changes' isQuitNoSaveRecord: isQuitRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt:put:)
!

