'From Cuis 5.0 of 7 November 2016 [latest update: #3083] on 25 May 2017 at 8:17:34 pm'!

!Integer methodsFor: 'benchmarks' stamp: 'jmv 5/25/2017 20:10:06'!
tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results
	0 tinyBenchmarks
		292 MHz G3 Mac: 				        			 22,727,272 bytecodes/sec; 			    984,169 sends/sec
		400 MHz PII/Win98:  			        			 18,028,169 bytecodes/sec; 			1,081,272 sends/sec

		RasPi2 - StackVM: 								 37,758,112 bytecodes/sec; 		       2,412,667 sends/sec
		Raspi2- CogSSpur: 							     157,441,574 bytecodes/sec; 		     10,946,039 sends/sec
		
		C.H.I.P. (*1) - StackVM: 							55,315,471 bytecodes/sec;  			3,348,667 sends/sec
		C.H.I.P. (*1) - CogSpur: 					 	    253,716,551 bytecodes/sec; 		    16,853,816 sends/sec

		1.66GHz Atom N450 - Cog: 					     244,274,809 bytecodes/sec; 		    28,795,277 sends/sec
		1.66GHz Atom N450 - CogSpur: 			     469,724,770 bytecodes/sec; 		    30,754,699 sends/sec

		1.33GHz Atom 3735G - Cog: 				     326,114,649 bytecodes/sec; 		    34,985,976 sends/sec
		1.33GHz Atom 3735G - CogSpur: 			     632,098,765 bytecodes/sec; 		    33,692,910 sends/sec

		1.5GHz AMD A4-5000 APU - Cog: 			    390,243,902 bytecodes/sec; 		    47,507,997 sends/sec
		1.5GHz AMD A4-5000 APU - CogSpur: 		    675,907,590 bytecodes/sec; 		    40,669,724 sends/sec
		1.5GHz AMD A4-5000 APU - CogSpur64:	    659,368,963 bytecodes/sec; 		    50,338,916 sends/sec

		2.3GHz Tegra (*2) - StackVM: 				    258,847,320 bytecodes/sec; 		    13,014,759 sends/sec	
		2.3GHz Tegra (*2) - CogSpur: 				1,083,024,854 bytecodes/sec; 		    64,289,750 sends/sec

		3.1GHz Core i3-2100 - Cog:  				1,203,290,246 bytecodes/sec; 		165,723,327 sends/sec
		3.1GHz Core i3-2100 - CogSpur:  			2,042,892,768 bytecodes/sec; 		127,837,794 sends/sec

		(*1) C.H.I.P. $9 Computer [Next Thing Co], Allwinner R8 Single-Core ARM Cortex-A8 - 1 GHz
		(*2) ACER Chromebook CB5-311 - NVIDIA Tegra K1 Quad-Core ARM Cortex-A15 'r3'  - 2.3 GHz
		
	It is interesting to note that Spur for the first time brings ARM hardware (RasPi and C.H.I.P.) into a 'Morphic is confortable and nice to use' level of performance.
	"
	| t1 t2 r n1 n2 |
	n1 _ 1.
	[
		t1 _ Time millisecondsToRun: [n1 benchmark].
		t1 < 1000] 
			whileTrue:[n1 _ n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 _ 28.
	[
		t2 _ Time millisecondsToRun: [r _ n2 benchFib].
		t2 < 1000] 
			whileTrue:[n2 _ n2 + 1]. 
	"Note: #benchFib's runtime is about O(k^n),
		where k is the golden number = (1 + 5 sqrt) / 2 = 1.618...."

	^ ((n1 * 500000 * 1000) // t1) printStringWithCommas, ' bytecodes/sec; ',
	  ((r * 1000) // t2) printStringWithCommas, ' sends/sec'! !


!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 5/14/2017 23:20:19'!
isRunningCogit
	"Returns true if we're running on the Cog JIT
	 (vmParameterAt: 46 is the size of the machine code zone)
	Smalltalk isRunningCogit
	"

	^(self vmParameterAt: 46)
		ifNotNil: [ :machineCodeZoneSize  | machineCodeZoneSize > 0 ]
		ifNil: [ false ]! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 5/14/2017 23:20:40'!
isSpur
	"Answer true if we are a Spur ObjectMemory.
	Spur introduces a new format of header for objects, new format for classes, etc.
	Smalltalk isSpur
	"

	^ self compactClassesArray isNil! !


!SystemDictionary class methodsFor: 'copyright' stamp: 'jmv 5/14/2017 23:13:07'!
copyright
	"The Cuis Smalltalk copyright.
	Parts are copyright of many contributors to Squeak and Cuis projects."

	^
'Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982.
Copyright (c) Apple Computer, Inc. 1985-1996.
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2017.'! !

!methodRemoval: Utilities class #compileUsingClosures!
Utilities class removeSelector: #compileUsingClosures!
!methodRemoval: Utilities class #decommissionTheAllCategory!
Utilities class removeSelector: #decommissionTheAllCategory!
!methodRemoval: Utilities class #initializeClosures!
Utilities class removeSelector: #initializeClosures!
!methodRemoval: SystemVersion class #currentPluginVersion!
SystemVersion class removeSelector: #currentPluginVersion!
!methodRemoval: SystemVersion class #parseVersionString:!
SystemVersion class removeSelector: #parseVersionString:!
!methodRemoval: SystemVersion class #pluginVersion:newerThan:!
SystemVersion class removeSelector: #pluginVersion:newerThan:!
!methodRemoval: SystemDictionary #fixSourceCodeLineEndings!
SystemDictionary removeSelector: #fixSourceCodeLineEndings!
!methodRemoval: SystemDictionary #makeInternalRelease!
SystemDictionary removeSelector: #makeInternalRelease!
!methodRemoval: SystemDictionary #removeTextCode!
SystemDictionary removeSelector: #removeTextCode!

!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shoutAboutToStyle: toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' autoCompleterClass editorClass textStylerClass)
!


!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForHierarchyFor: annotationForSelector:ofClass: annotationRequests annotationSeparator)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText commentContents contentsSymbol contentsSymbol:)
('commands' hierarchyBrowser removeClass)
('controls' contentsSymbolQuints sourceAndDiffsQuintsOnly)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showDiffs: showLineDiffs: showPrettyLineDiffs: showPrettyWordDiffs: showWordDiffs: showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleDiffing toggleLineDiffing togglePlainSource togglePrettyLineDiffing togglePrettyPrint togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' prettyPrintString showByteCodes: showComment showDecompile: showDocumentation: showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingDocumentation showingDocumentationString showingPlainSource showingPlainSourceString showingPrettyPrint showingSource toggleDecompile toggleShowDocumentation toggleShowingByteCodes)
('message list' decompiledSource selectedBytecodes selectedMessage sourceStringPrettifiedAndDiffed validateMessageSource:forSelector:inClass:)
('message list menu' copySelector fileOutMessage revertToPreviousVersion timeStamp unusedMethods)
('message category functions' canShowMultipleMessageCategories)
('testing' is:)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext)
('shout styling' isModeStyleable shoutAboutToStyle:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
!


!Browser class reorganize!
('instance creation' new)
('cached state access' recentClasses releaseClassCachedState)
!


!MessageSet reorganize!
('message list' addMethodReference: growable messageList messageListIndex: selectedMessageName sortByDate)
('message functions' deleteFromMessageList: methodCategoryChanged reformulateList removeMessage removeMessageFromBrowser)
('class list' metaClassIndicated selectedClass selectedClassOrMetaClass selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText selectedMessage)
('private' autoSelectString autoSelectString: contents:notifying: initializeMessageList: selection setFilteredList:)
('filtering' filterFrom: filterToAnyChangeSet filterToCommentedMethods filterToCurrentAuthor filterToImplementorsOf filterToMessagesInChangesFile filterToMessagesInSourcesFile filterToMessagesWithPriorVersions filterToMessagesWithoutPriorVersions filterToNotAnyChangeSet filterToNotCurrentAuthor filterToNotImplementorsOf filterToNotSendersOf filterToSendersOf filterToUncommentedMethods filterToUnsentMessages)
('message category functions' canShowMultipleMessageCategories)
('accessing' labelString reformulateListNoting:)
('shout styling' shoutAboutToStyle:)
!


!ProtocolBrowser reorganize!
('accessing' growable)
('private' initListFrom:highlighting: on: onSubProtocolOf:)
!


!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText codeFile: contents:notifying: contentsSymbol selectedCodeFile)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('class list' classList renameClass selectedClass)
('edit pane' selectedBytecodes selectedMessage)
('diffs' methodDiffFor:class:selector:meta: modifiedClassDefinition)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' changeMessageCategories: didCodeChangeElsewhere labelString)
('menus' contentsSymbolQuints)
('shout styling' shoutAboutToStyle:)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('notifier menu' storeLog)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shoutAboutToStyle:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUndertand implement:inClass:context: shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnSubclassResponsibility)
!


!FileList reorganize!
('file list' fileList fileListIndex fileListIndex: fileName selectedFileEntry)
('user interface support' autoCompleterClassFor: editorClassFor:)
('file list menu' itemsForAnyFile1 itemsForAnyFile2 itemsForFile: itemsForNoFile)
('file menu action' addNew:byEvaluating: addNewDirectory addNewFile deleteFile get getHex renameFile sortByDate sortByName sortBySize)
('initialization' buttonSelectorsToSuppress directory: dynamicButtonServices initialDirectoryList initialize labelString universalButtonServices updateDirectory)
('menu messages' copyName)
('own services' serviceAddNewDirectory serviceAddNewFile serviceCopyName serviceDeleteFile serviceGet serviceGetHex serviceRenameFile serviceSortByDate serviceSortByName serviceSortBySize serviceViewContentsInWorkspace viewContentsInWorkspace)
('volume list and pattern' deleteDirectory directory doesPattern:allow: fileNameFormattedFrom:namePad:sizePad:sizeWithCommasPad: listForSelectingPatterns:rejectingPatterns: pattern pattern:)
('private' acceptedStringOrText currentDirectorySelected defaultContents fileNameFromFormattedItem: folderString fullName isFileSmalltalkCode postOpen put: readContentsBrief: readContentsHex: resort: setSelectedDirectoryTo: sortBlock sortingByDate sortingByName sortingBySize updateFileList)
('accessing' brevityState)
!


!TestRunner reorganize!
('accessing' completedFraction details errors errorsList failures failuresList listSelectionAt: listSelectionAt:put: passFail progressLabel result selectedTests tests timeSinceLastPassAsString:)
('constants' runButtonState stopButtonState)
('menus' deselectAll invertSelections selectAll setFilter terminateRun)
('processing' addTestsFor:toSuite: debugErrorTest: debugFailureTest: refreshTests runOneTest runProfiledTests runSuite: runSuiteProfiled: runTests selectedErrorTest selectedFailureTest selectedSuite selectedSuite:)
('updating' displayDetails: displayErrors: displayFailures: displayPassFail: refreshTR runWindow update: updateDetails: updateErrors: updateFailures: updatePassFail: updateWindow:)
('initialization' gatherTestNames initialize testCases)
('logging' errorLog showResult showResultDefects showResultSummary)
!


!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: subclasses superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames elementSize inspectAllInstances inspectSubInstances instVarNames instanceCount sharedPools someInstance)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: allUnreferencedInstanceVariables crossReference lastUnderscoreMeansSubscript unreferencedInstanceVariables withAllSubAndSuperclassesDo:)
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


!Class class reorganize!
('instance creation' template: templateForSubclassOf:category:)
!


!Number reorganize!
('arithmetic' * + - / // \\ abs arg div: mod: negated quo: reciprocal rem:)
('comparing' closeTo: isWithin:ulpsFrom:)
('converting' @ adaptToCollection:andSend: adaptToComplex:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asComplex asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours i milliSeconds minutes nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log2 log: magnitude nthRoot: raisedTo: raisedToInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase:)
('testing' even isDivisibleBy: isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart reduce roundDownTo: roundTo: roundUpTo: rounded truncateTo: truncated)
!


!Integer reorganize!
('arithmetic' * + - / // \\\ alignedTo: crossSumBase: gcd: quo: reciprocalModulo2: reciprocalModulo: xgcd:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash)
('converting' adaptToFraction:andSend: asCharacter asColorOfDepth: asFloat asFraction asHexDigit asInteger asIntegerOrFloat)
('enumerating' timesRepeat:)
('inspecting' explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog ifMultipleOf2And5Do:otherwise: lcm: ln log log2 log: nthRoot: nthRootRounded: nthRootTruncated: productTo: raisedTo:modulo: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:length:zeroPadded: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even isInteger isPowerOfTwo isPrime isProbablyPrime nextPrime)
('truncation and round off' atRandom atRandom: ceiling floor normalize rounded truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryDigitBase montgomeryDigitLength montgomeryDigitMax montgomeryNumberOfDigits montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
!


!Integer class reorganize!
('instance creation' basicNew initializedInstance new new:neg: readBaseOneFrom: readFrom: readFrom:base:)
('prime numbers' largePrimesUpTo: largePrimesUpTo:do: primesUpTo: primesUpTo:do: verbosePrimesUpTo: verbosePrimesUpTo:do:)
!


!SmallInteger reorganize!
('arithmetic' * + - / // \\ gcd: quo:)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed hashMultiply lowBit)
('testing' even isLarge odd)
('comparing' < <= = > >= ~= identityHash)
('copying' shallowCopy)
('converting' asFloat)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength instVarAt: nextInstance nextObject)
('mathematical functions' sqrt)
!


!Random class reorganize!
('instance creation' concreteRandomClass new newDefault seed:)
('default instance' withDefaultDo:)
('cached state access' releaseClassCachedState)
!


!BlockClosure reorganize!
('accessing' argumentCount copiedValueAt: home isBlock method numArgs numCopiedValues outerContext receiver size startpc)
('controlling' repeat repeatWithGCIf: whileFalse whileFalse: whileNil: whileNotNil: whileTrue whileTrue:)
('evaluating' bench durationToRun ifError: timeToRun timeToRunWithoutGC value value: value:value: value:value:value: value:value:value:value: valueAt: valueNoContextSwitch valueNoContextSwitch: valueWithArguments: valueWithExit valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and: valueWithin:onTimeout:)
('exceptions' assert ensure: ifCurtailed: on:do: on:do:on:do: on:do:on:do:on:do: onDNU:do: valueUninterruptably)
('printing' decompile printOn: printStack:)
('scheduling' asContext fork forkAndWait forkAt: forkAt:named: forkNamed: newProcess newProcessWith:)
('private' asContextWithSender: copyForSaving grabProcessor grabProcessorFor:onTimeout: grabProcessorOnlyFor: reentrant valueUnpreemptively)
('testing' hasMethodReturn isClosure isDead isTrivialClosure)
('error handing' handles: numArgsError:)
('events-support' asMinimalRepresentation isReceiverOrAnyArgumentGarbage)
('debugger access' sender)
('objects from disk' objectForDataStream:)
('services' timeProfile)
('initialization' outerContext:startpc:numArgs:copiedValues:)
('system simulation' simulateValueWithArguments:caller:)
!


!InstructionStream class reorganize!
('instance creation' on:)
('compiling' instVarNamesAndOffsetsDo: isContextClass)
('cached class state' initSpecialConstants releaseClassCachedState specialConstants)
!


!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables home method methodNode methodReturnContext receiver size tempAt: tempAt:put:)
('instruction decoding' doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: longStack messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' closureCopy:copiedValues: hasSender: jump pop push: quickSend:to:with:super: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: insertSender: isPrimFailToken: privSender: push:fromIndexable: stackPtr stackp: tryNamedPrimitiveIn:for:withArgs:)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext unwindTo:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
!


!MethodContext reorganize!
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasMethodReturn home isExecutingBlock method methodReturnContext outerContext receiver removeSelf tempAt: tempAt:put:)
('closure support' asContext contextTag)
('initialization' privRefresh privRefreshWith:)
('inspecting' inspectorClass)
('instruction decoding (closures)' blockReturnTop pushConsArrayWithElements:)
('printing' printDetails: printOn: printString)
('system simulation' failPrimitiveWith: pushArgs:from:)
('testing' atEnd)
('private' aboutToReturn:through: cannotReturn: instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-exceptions' isHandlerOrSignalingContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
!


!MethodContext class reorganize!
('instance creation' sender:receiver:method:arguments:)
('private' allInstancesDo:)
('enumerating' allInstances)
!


!InputSensor reorganize!
('buttons' buttons)
('joystick' joystickButtons: joystickOn: joystickXY: testJoystick:)
('keyboard' flushKeyboard kbdTest keyboard keyboardPeek keyboardPressed)
('modifier keys' anyModifierKeyPressed commandAltKeyPressed controlKeyPressed macOptionKeyPressed rawMacOptionKeyPressed shiftPressed)
('mouse' isAnyButtonPressed isMouseButton1Pressed isMouseButton2Pressed isMouseButton3Pressed mouseButtons mousePoint mousePoint: noButtonPressed peekButtons peekMousePt waitButton waitButtonOrKeyboard waitClickButton waitNoButton)
('tablet' hasTablet tabletExtent tabletPoint tabletPressure tabletTimestamp)
('user interrupts' eventTicklerProcess installInterruptWatcher interruptWatcherProcess setInterruptKey: userInterruptWatcher)
('private' characterForKeycode: primCursorLocPut: primCursorLocPutAgain: primKbdNext primKbdPeek primMouseButtons primMousePt primReadJoystick: primSetInterruptKey: primTabletGetParameters: primTabletRead:)
('initialization' flushEvents)
!


!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: appendToText: copyReplaceTokens:with: join: padded:to:with:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asCamelCase asCharacter asDate asFileName asHex asIdentifier: asInteger asLegalSelector asLowercase asMonth asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector initialIntegerOrNil keywords prefixAndSuffix: romanNumber squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withFirstCharacterDownshifted withLineEndings: withNewLines withoutEnclosing: withoutPrefix: withoutSuffix: withoutTrailingBlanks)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral print printOn: storeOn:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer isAlphaNumeric isRemote isString)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('user interface' edit editLabel:)
('formatting' format:)
('uCompletion' separateKeywords)
('*fileman-core-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('*fileman-core-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('*fileman-core-actions' fileContents fileContents:)
('*fileman-core-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('text conversion helpers' bold italic struck sub super under)
!


!Array reorganize!
('comparing' literalEqual:)
('converting' asArray elementsExchangeIdentityWith: elementsForwardIdentityTo: elementsForwardIdentityTo:copyHash: evalStrings)
('printing' isLiteral printOn: storeOn:)
('private' hasLiteral: hasLiteralSuchThat: replaceFrom:to:with:startingAt:)
('accessing' atWrap: atWrap:put:)
('testing' identityIncludes: is: isArray)
!


!FloatArray reorganize!
('accessing' at: at:put: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= adaptToNumber:andSend: dot: negated normalize sum \\=)
('comparing' = hash primitiveEqual:)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('private' replaceFrom:to:with:startingAt:)
('inspecting' inspectorClass)
('testing' is:)
('interpolating' interpolatedValueAt:)
!


!IntegerArray reorganize!
('accessing' at: at:put: atAllPut: defaultElement)
('private' primFill:)
!


!WordArray reorganize!
('accessing' atAllPut: byteSize bytesPerElement defaultElement)
('private' replaceFrom:to:with:startingAt:)
!


!Interval reorganize!
('accessing' anyOne at: at:put: extent first increment last rangeIncludes: size)
('comparing' = hash)
('adding' add:)
('removing' remove:)
('enumerating' do: permutationsDo: reverseDo:)
('printing' printOn: storeOn:)
('private' setFrom:to:count: species)
('testing' includes: isInterval)
('arithmetic' + -)
!


!SortedCollection reorganize!
('accessing' at:put: indexOf:startingAt:ifAbsent: median quickIndexOf: sortBlock sortBlock:)
('adding' add: addAll: addFirst:)
('comparing' =)
('copying' copy postCopy)
('enumerating' select:)
('private' defaultSort:to: indexForInserting: insert:before: reSort)
!


!Text reorganize!
('accessing' append: at: at:put: commandForReplaceFrom:to:with: customizeExplorerContents embeddedMorphs embeddedMorphsFrom:to: findString:startingAt: findString:startingAt:caseSensitive: lineCount prepend: rangeOf:startingAt: replaceFrom:to:with: size string)
('comparing' = hash)
('copying' , appendToString: appendToText: copyFrom:to: copyReplaceFrom:to:with: copyReplaceTokens:with: postCopy)
('converting' asNumber asString asText displayStringOrText optimizedForMutationSpeed replaceFrom:to:with:startingAt: replaceFrom:to:withString:attributes:startingAt: reversed withCuisLineEndings)
('emphasis' addAttribute: addAttribute:from:to: alignmentAt: attributesAt: attributesFrom:to:do: characterStyleOrNilAt: characterStyleOrNilIfApplying: commandForAddAttribute:from:to: commandForRemoveAttributes:from:to: emphasisAt: find: font: fontAt:default: fontIfApplying:default: initialFont: initialFontFrom: paragraphStyleOrNilAt: paragraphStyleOrNilIfApplying: removeAttributes:from:to: removeAttributesThat: runLengthFor: withAttributeValues:do:)
('testing' is: isRemote)
('printing' printOn: storeOn:)
('private' basicReplaceAttributesFrom:to:with: fixParagraphAttributesFrom:to: privateSetParagraphAttributes:from:to: privateSetRuns: runs setString:setRuns:)
('attributes' canJoin:and: hasAnyAttribute hasAnyParagraphAttribute)
('user interface' edit editLabel:)
('paragraph support' encompassLine: encompassParagraph: paragraphStyleChunksDo:)
('adding emphasis' allBold bold italic struck sub super under)
('adding' add:)
!


!Bag reorganize!
('accessing' at: at:put: cumulativeCounts size sortedCounts sortedElements)
('adding' add: add:withOccurrences: addAll:)
('copying' postCopy)
('enumerating' asSet detect:ifNone: do: select:)
('private' contents setContents:)
('removing' remove:ifAbsent:)
('testing' includes: is: occurrencesOf:)
('comparing' =)
('math functions' sum)
!


!Set reorganize!
('accessing' atRandom: like: size)
('adding' add: add:withOccurrences:)
('copying' postCopy)
('enumerating' collect: do: union:)
('objects from disk' comeFullyUpOnReload:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: rehash scanFor: swap:with: withArray:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = includes: is: occurrencesOf:)
('explorer' hasContentsInExplorer)
('inspecting' inspectorClass)
!


!MethodDictionary reorganize!
('accessing' add: associationAt:ifAbsent: at:ifAbsent: at:put: keyAtIdentityValue:ifAbsent: keyAtValue:ifAbsent:)
('removing' removeAll removeKey:ifAbsent:)
('enumeration' associationsDo: keysAndValuesDo: keysDo: valuesDo:)
('private' fixCollisionsFrom: grow keyAt: methodArray postCopy rehash rehashWithoutBecome removeDangerouslyKey:ifAbsent: scanFor: swap:with:)
!


!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClass:from:to: renameClassNamed:as:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmParameterAt: vmParameterAt:put: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSapshot:andQuit: okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitPrimitive quitPrimitive: removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotPrimitive unbindExternalPrimitives)
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
('image' wordSize)
!


!LiteralDictionary reorganize!
('as yet unclassified' scanFor:)
!


!IdentitySet reorganize!
('private' scanFor:)
!


!WriteStream reorganize!
('accessing' contents next next:putAll:startingAt: nextPut: nextPutAll: nextPutAllString:withAttributes: size)
('positioning' position: reset resetToStart setToEnd)
('character writing' ensureASpace ensureNoSpace lf newLine newLineTab: nextPutKeyword:withArg: peekLast space space: tab tab:)
('fileIn/Out' nextChunkPut: timeStamp)
('printing' store:)
('private' braceArray braceArray: growTo: isCompatibleWithContents: on: on:from:to: pastEndPut: with: withAttribute:do: withAttributes:do:)
('testing' isEmpty)
!


!ReadWriteStream reorganize!
('accessing' contents name next next: truncateAtPosition)
('fileIn/Out' fileNameEndsWith: fileOutObject:)
('converting' readStream)
('testing' = hash)
!


!FileStream reorganize!
('accessing' contents contentsOfEntireFile next next: nextPut: nextPutAll: size)
('testing' atEnd isFileStream)
('positioning' position position: reset setToEnd skip: truncate:)
('printing' longPrintOn: longPrintOn:limitedTo:indent:)
('file open/close' close closed flush reopen)
('file modes' readOnly readWrite text useBytes useCharacters)
('file accessing' localName name)
('fileIn/Out' fileIn)
!


!FileStream class reorganize!
('instance creation' new)
('concrete classes' concreteStream)
!


!QAbstractTally reorganize!
('converting' asArray)
('report' fullPrintOn:linesOn:talliesOn:threshold:time:reportOnly: printOn: printOn:linesOn:talliesOn:tabs:total:totalTime:parent: rootPrintOn:linesOn:talliesOn:total:totalTime:threshold:reportOnly: sonsOver: treeDFSDo:afterChildrenDo:threshold:parent:)
('tallying' addTallies: tallyPrimInMethod:by:)
('accessing' process process: tally)
('misc' sortCollectionFrom:)
('initialization' initialize)
!


!Exception reorganize!
('Camp Smalltalk' sunitExitWith:)
('defaults' defaultResumeValue)
('exceptionBuilder' messageText: tag:)
('exceptionDescription' defaultAction description messageText noHandler tag)
('handling' isNested outer pass resignalAs: resume resume: resumeUnchecked: retry retryUsing: return return:)
('printing' printOn: receiver signalerContext)
('priv handling' handlerContexts handlerContexts: initialize isResumable popHandlerContext pushHandlerContext: topHandlerContext)
('signaling' signal signal: signalIn:)
!


!NotYetImplemented reorganize!
('accessing' receiverClass selector)
('initialization' receiverClass:selector:)
!


!Compiler class reorganize!
('accessing' decompilerClass new parserClass)
('evaluating' evaluate: evaluate:for:logged: evaluate:for:notifying:logged: evaluate:logged: evaluate:notifying:logged: format:in:notifying:)
('utilities' recompileAll recompileAllFrom:)
('user interface support' autoCompleterClass editorClass textStylerClass)
!


!Scanner reorganize!
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('multi-character scans' typeTableAt: xBacktick xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('initialization' initScanner initScannerForTokenization scan:)
!


!BytecodeEncoder reorganize!
('temps' bindAndJuggle: bindBlockArg:within: bindBlockTemp: bindBlockTemp:within: bindTemp: blockExtentsToTempsMap noteBlockExtent:hasLocals:)
('testing' hasGeneratedMethod supportsClosureOpcodes)
('special literal encodings' if:isSpecialLiteralForPush: if:isSpecialLiteralForReturn:)
('accessing' methodStreamPosition rootNode rootNode:)
('opcode sizing' nextPut: sizeBranchPopFalse: sizeBranchPopTrue: sizeCallPrimitive: sizeDup sizeJump: sizeJumpLong: sizeOpcodeSelector:withArguments: sizePop sizePushClosureCopyNumCopiedValues:numArgs:jumpSize: sizePushConsArray: sizePushInstVar: sizePushInstVarLong: sizePushLiteral: sizePushLiteralVar: sizePushNClosureTemps: sizePushNewArray: sizePushReceiver sizePushRemoteTemp:inVectorAt: sizePushSpecialLiteral: sizePushTemp: sizePushTempLong: sizePushThisContext sizeReturnReceiver sizeReturnSpecialLiteral: sizeReturnTop sizeReturnTopToCaller sizeSend:numArgs: sizeSendSpecial:numArgs: sizeSendSuper:numArgs: sizeStoreInstVar: sizeStoreInstVarLong: sizeStoreLiteralVar: sizeStorePopInstVar: sizeStorePopInstVarLong: sizeStorePopLiteralVar: sizeStorePopRemoteTemp:inVectorAt: sizeStorePopTemp: sizeStoreRemoteTemp:inVectorAt: sizeStoreTemp:)
('bytecode generation' outOfRangeError:index:range:to:)
('initialization' streamToMethod:)
('method generation' computeMethodHeaderForNumArgs:numTemps:numLits:primitive:)
!


!AssignmentNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('testing' isAssignmentNode isComplex)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('equation translation' variable)
('initialization' toDoIncrement: value variable:value: variable:value:from: variable:value:from:sourceRange:)
('code generation' emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCodeForEffect: sizeCodeForValue:)
!


!LiteralVariableNode reorganize!
('visiting' accept:)
('code generation' emitCodeForLoad:forValue:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForLoad:forValue: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('testing' assignmentCheck:at:)
!


!DecompilerConstructor reorganize!
('visiting' accept:)
('constructor' codeAnyLitInd: codeAnyLiteral: codeAnySelector: codeArguments:block: codeArguments:temps:block: codeAssignTo:value: codeBlock:returns: codeBrace: codeCascade:messages: codeCascadedMessage:arguments: codeConstants codeEmptyBlock codeInst: codeMessage:selector:arguments: codeMethod:block:tempVars:primitive:class: codeRemoteTemp:remoteTemps: codeSelector:code: codeSuper codeTemp: codeTemp:named: codeThisContext decodeIfNilWithReceiver:selector:arguments: decodeLiteralVariableValueDereferenceWithReceiver:selector:arguments:)
('initialization' method:class:literals:)
!


!FileEntry reorganize!
('actions-rio' < <<)
('enumeration' assureExistence)
('actions-file' appendContents: binaryContents binaryContents: copyTo: delete fileContents fileContents: formContents rename: textContents textContents:)
('accessing-stream' appendStreamDo: forceWriteStreamDo: readStreamDo: writeStreamDo:)
('accessing' appendStream fileSize fileSize: readStream writeStream)
('private' forceWriteStream initValuesFrom:)
('testing' exists isFile)
('initialize-release' refresh)
('actions-pipe' pipe:to: pipeRepeat:to: pipeRepeat:while:to:)
!


!Feature reorganize!
('accessing' name requirementOfMe revision version)
('testing' satisfies:)
('printing' printDetailsOn: printOn: printVersionRevisionOn:)
('incrementing' newRevision newVersion)
('initialization' name:version:revision:)
!


!ExternalSemaphoreTable class reorganize!
('accessing' accessProtect clearExternalObjects externalObjects registerExternalObject: releaseClassCachedState safelyRegisterExternalObject: safelyUnregisterExternalObject: unregisterExternalObject:)
!


!SystemVersion class reorganize!
('accessing' current)
('instance creation' newVersionMajor:versionMinor: versionMajor:versionMinor:)
!


!Utilities class reorganize!
('common requests' saveDisplay saveScreenshot)
('identification' authorInitials authorInitialsPerSe authorName changeStamp clearAuthor dateTimeSuffix fixStamp: methodsWithInitials: monthDayTime24StringFrom: monthDayTimeStringFrom: setAuthor)
('miscellaneous' awaitMouseUpIn:repeating:ifSucceed: awaitMouseUpIn:whileMouseDownDo:whileMouseDownInsideDo:ifSucceed: garbageCollectAndReport)
('summer97 additions' browseVersionsForClass:selector: classFromPattern:withCaption: hierarchyOfClassesSurrounding: hierarchyOfImplementorsOf:forClass: inviolateInstanceVariableNames isLegalInstVarName: wellFormedInstanceVariableNameFrom:)
('support windows' codeManagementInCuisContents cuisAndGitHubContents openCodeManagementInCuis openCuisAndGitHub openUsefulExpressions usefulExpressionsContents)
('user interface' informUser:during: informUserDuring:)
('vm statistics' reportCPUandRAM textMarkerForShortReport vmStatisticsReportString vmStatisticsShortString)
('tailoring system' stripMethods:messageCode:)
('cached state access' releaseClassCachedState releaseClassState)
!


!Form class reorganize!
('instance creation' dotOfSize: extent: extent:depth: extent:depth:bits: extent:depth:fromArray:offset: extent:fromArray:offset: extent:nativeDepthFrom: extent:nativeDepthFrom:bits: extent:offset: fakeSphereAt:diameter:color: fromBinaryStream: fromDisplay: fromFileEntry: fromFileNamed: fromFileNamedOrNil: fromUser fromUserWithExtent: verticalGradient:topColor:bottomColor:)
('mode constants' and blend blendAlpha erase erase1bitShape oldErase1bitShape oldPaint over paint paintAlpha reverse rgbMul under)
('examples' toothpaste: xorHack:)
('class initialization' unload)
('creation - anti aliased' bottomLeftCorner:height:gradientTop:gradientBottom: bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth: bottomRightCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom:borderWidth: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom:)
!


!Cursor reorganize!
('displaying' activateCursor showWhile:)
('printing' printOn:)
('testing' hasMask is:)
('converting' asBigCursor asCursorForm enlargedBy: withMask)
('primitives' installCursor installCursorWithMask:)
!


!DisplayScreen reorganize!
('displaying' flash: flash:with: forceDamageToScreen:)
('other' deferUpdates: forceDisplayUpdate forceToScreen forceToScreen: fullScreenMode: newDepth: primitiveDeferUpdates: restore restoreAfter: supportedDisplayDepths supportsDisplayDepth:)
('disk I/O' objectForDataStream:)
('private' beDisplay findAnyDisplayDepth findAnyDisplayDepthIfNone: newDepthNoRestore: primRetryShowRectLeft:right:top:bottom: primShowRectLeft:right:top:bottom: setExtent:depth:)
('copying' copy:from:in:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule: copyBits:from:at:clippingBox:rule:map: eraseShape: fill:rule:fillColor: fillFromXYColorBlock: fillShape:fillColor:at:)
!


!BMPReadWriter class reorganize!
('image reading/writing' typicalFileExtensions)
!


!WarpBlt reorganize!
('setup' cellSize cellSize:)
('smoothing' mixPix:sourceMap:destMap: rgbMap:from:to:)
('primitives' copyQuad:toRect: deltaFrom:to:nSteps: sourceForm:destRect: sourceQuad:destRect: startFrom:to:offset: warpBits warpBitsSmoothing:sourceMap:)
!


!AbstractFont class reorganize!
('instance accessing' default default: familyName:aroundPointSize: familyName:pointSize: familyNames fromUser fromUser: pointSizesFor:)
('class initialization' initialize)
('constants' boldCode italicCode normalCode struckThroughCode subscriptCode superscriptCode underlinedCode withST80GlyphsCode)
!


!CharacterBlockScanner reorganize!
('stop conditions' crossedX doNewLine endOfRun paddedSpace setFont setStopConditions tab)
('scanning' characterBlockAtPoint:index:in: placeEmbeddedObject:)
!


!BorderedRectMorph reorganize!
('accessing' borderColor borderColor: borderWidth borderWidth:)
('drawing' drawOn:)
('initialization' defaultBorderColor defaultBorderWidth initialize)
('testing' is: isOpaqueMorph)
!


!PasteUpMorph reorganize!
('accessing' activeHand color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:from: redrawNeeded removedMorph:)
('classification' isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: allowsMorphDrop allowsSubmorphDrag)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: mouseButton1Down:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' mouseButton2Activity wantsWindowEvent: windowEventHandler)
('geometry' externalizeDisplayBounds: externalizeToWorld: internalizeFromWorld: morphPositionInWorld privateExtent:)
('initialization' clearCanvas clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor)
('interaction loop' doOneCycleNow)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: viewBox)
('stepping' startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('stepping and presenter' wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' addMorph:centeredNear: allNonWindowRelatedSubmorphs deleteAllHalos displayWorld displayWorldSafely doOneCycle doOneMinimalCycleNow fillRects:color: flashRects:color: fullRepaintNeeded haloMorphs privateOuterDisplayWorld restoreMorphicDisplay worldState:)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
('object serialization' objectForDataStream:)
('windows' findATranscript)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
!


!FillInTheBlankMorph class reorganize!
('instance creation' initializedInstance request: request:initialAnswer: request:initialAnswer:centerAt:onCancelReturn:acceptOnCR: request:initialAnswer:onCancel: request:onCancel:)
!


!MorphicEvent reorganize!
('accessing' hand timeStamp wasHandled wasHandled:)
('comparing' hash)
('dispatching' dispatchWith:localPosition: sentTo:localPosition: startDispatchFrom:)
('testing' is: isDraggingEvent isDropEvent isKeyboard isKeystroke isMouse isMouseOver isWindowEvent)
('private' setTimeStamp:)
!


!UserInputEvent reorganize!
('accessing' buttons eventPosition wasHandled wasHandled:)
('modifier state' anyModifierKeyPressed commandAltKeyPressed controlKeyPressed macOptionKeyPressed rawMacOptionKeyPressed shiftPressed)
!


!MouseButtonEvent reorganize!
('accessing' mouseButton1Changed mouseButton2Changed mouseButton3Changed)
('dispatching' dispatchWith:localPosition: sentTo:localPosition:)
('private' setType:position:which:buttons:hand:stamp:)
!


!TheWorldMenu class reorganize!
('as yet unclassified')
!


!BitBltCanvas class reorganize!
('cached forms' bottomLeftCorner:height:gradientTop:gradientBottom: bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth: bottomRightCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom:borderWidth: steButtonForm: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom: verticalGrayGradient:gradientTop:gradientBottom:)
('cached arrow forms' arrowOfDirection:size: buildArrowOfDirection:size: buildArrowWith:borderForm:)
('cached state access' accessProtect cachedForms releaseClassCachedState)
!


!MethodReference reorganize!
('queries' actualClass category classIsMeta classSymbol compiledMethod isValid methodSymbol printOn: selector sourceCode)
('setting' setClass:methodSymbol:stringVersion: setClassSymbol:classIsMeta:methodSymbol:stringVersion: setStandardClass:methodSymbol:)
('string version' stringVersion)
('comparisons' <= = hash)
('services' updateReferencesTo:toBe:)
('printing' displayStringOrText)
!


!ChangeSet class reorganize!
('class initialization' initialize)
('instance creation' named: new)
('system change notifications' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('change set to use' baseSystemNameFor: changeSetForBaseSystem changeSetForPackage: currentBaseCSNumber incrementCurrentBaseCSNumber installing:do: isInstalling: notInstallOrTestRun runningTest:do:)
('file list services' fileReaderServicesForFile:suffix: serviceFileIn serviceInstall)
('services' fileIn: install: installNewUpdates installNewUpdates: newUpdates:)
('enumerating' allChangeSets allChangeSetsWithClass:selector: changeSetNamed: countOfChangeSetsWithClass:andSelector: doesAnyChangeSetHaveClass:andSelector: existingOrNewChangeSetForPackage: existingOrNewChangeSetNamed:forBaseSystem:)
('removing' removeChangeSet: zapAllChangeSets)
!


!CodeFile class reorganize!
('as yet unclassified')
!


!TestResult reorganize!
('Accessing' correctCount defects errorCount errors failureCount failures passed passedCount runCount tests)
('Testing' hasErrors hasFailures hasPassed isError: isFailure: isPassed:)
('Printing' printOn:)
('Running' runCase:)
('Inspecting' forDebuggingAndInspection)
!


!SHParserST80 reorganize!
('accessing' classOrMetaClass: last3Ranges lastRange source source: workspace:)
('scan' currentChar nextChar peekChar scanBinary scanComment scanIdentifier scanNext scanNumber scanPast: scanPast:level: scanPast:start:end: scanWhitespace skipBigDigits: skipDigits)
('parse support' enterBlock initializeInstanceVariables leaveBlock pushArgument: pushTemporary:)
('error handling' error failUnless: failWhen:)
('token testing' isAnsiAssignment isAssignment isBinary isKeyword isName isTokenExternalFunctionCallingConvention)
('character testing' isBigDigit:base: isBinarySelectorCharacter:)
('identifier testing' isBlockArgName: isBlockTempName: isClassVarName: isGlobal: isIncompleteBlockArgName: isIncompleteBlockTempName: isIncompleteClassVarName: isIncompleteGlobal: isIncompleteInstVarName: isIncompleteMethodArgName: isIncompleteMethodTempName: isIncompletePoolConstantName: isIncompleteReservedName: isIncompleteWorkspaceVarName: isInstVarName: isMethodArgName: isMethodTempName: isPoolConstantName: isReservedName: isWorkspaceVarName: resolve: resolvePartial: resolvePartialPragmaArgument: resolvePragmaArgument:)
('parse' parse parse: parseArray parseBacktick parseBinary parseBinaryMessagePattern parseBlock parseBlockArguments parseBlockTemporaries parseBraceArray parseByteArray parseCascadeAndChain parseCharSymbol parseExpression parseExternalCall parseKeyword parseKeywordMessagePattern parseLiteral: parseLiteralArrayElement parseMessagePattern parseMethodTemporaries parsePragmaBinary parsePragmaKeyword parsePragmaSequence parsePrimitive parseStatement parseStatementList parseStatementListForBraceArray parseString parseStringOrSymbol parseSymbol parseSymbolIdentifier parseSymbolSelector parseSymbolString parseTerm parseUnary parseUnaryMessagePattern rangesIn:classOrMetaClass:workspace:)
('recording ranges' rangeType: rangeType:start:end:)
('indentifier iteration' blockArgNamesDo: blockTempNamesDo: classVarNamesDo: instVarNamesDo: methodArgNamesDo: methodTempNamesDo: namesBeginningWith:do: nonGlobalNamesDo: poolConstantNamesDo: reservedNamesDo: workspaceNamesDo:)
('testing' isMessage: isPartialOrFullIdentifier:)
!


!PopUpMenu class reorganize!
('instance creation' labelArray: labelArray:lines: labels: labels:lines: withCaption:chooseFrom:)
('dialogs' confirm: confirm:orCancel: confirm:trueChoice:falseChoice: inform: notify:)
!

