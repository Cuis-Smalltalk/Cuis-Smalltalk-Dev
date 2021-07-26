'From Cuis 5.0 [latest update: #4695] on 26 July 2021 at 9:13:32 am'!

!Object methodsFor: 'updating' stamp: 'ST-80 5/31/1983 9:10:35'!
changed
	"Receiver changed in a general way; inform all the dependents by sending 
	each dependent an update: message."

	self changed: self! !

!Object methodsFor: 'updating' stamp: 'ST-80 5/31/1983 9:10:35'!
update: aParameter 
	"Receive a change notice from an object of whom the receiver is a dependent. 
	The default behavior is to do nothing;  a subclass might want to change
	itself in some way."

	^self! !


!FileList methodsFor: 'file name list' stamp: 'ST-80 5/31/1983 9:10:35'!
fileName
	^ fileName! !


!Inspector methodsFor: 'doIt/accept/explain' stamp: 'ST-80 5/31/1983 9:10:35'!
doItReceiver
	"Answer the object that should be informed of the result of evaluating a
	text selection."
	^object! !


!Behavior methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
format
	"Answer an Integer that encodes the kinds and numbers of variables of instances
	of the receiver."

	^format! !

!Behavior methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: 'a descendent of '.
	superclass printOn: aStream! !

!Behavior methodsFor: 'compiling' stamp: 'ST-80 5/31/1983 9:10:35'!
compilerClass
	"Return a compiler class appropriate for source methods of this class."

	^Compiler! !

!Behavior methodsFor: 'compiling' stamp: 'ST-80 5/31/1983 9:10:35'!
decompile: selector 
	"Find the compiled code associated with the argument, selector, as a message selector
	in the receiver's method dictionary and decompile it.  Answer the resulting source
	code as a string.  Create an error if the selector is not in the receiver's method
	dictionary."

	^self decompilerClass new decompile: selector in: self! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'ST-80 5/31/1983 9:10:35'!
classVarNames
	"Answer a Set of the receiver's class variable names.  Since the receiver does
	not retain knowledge of class variables, the method fakes it by creating an empty set."

	^Set new! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'ST-80 5/31/1983 9:10:35'!
instanceCount
	"Answer the number of instances of the receiver that are currently in use."

	| count |
	count _ 0.
	self allInstancesDo: [:x | count _ count + 1].
	^count! !

!Behavior methodsFor: 'creating method dictionary' stamp: 'ST-80 5/31/1983 9:10:35'!
methodDictionary: aDictionary 
	"Store the argument, aDictionary, as the method dictionary of the receiver."

	methodDict _ aDictionary! !


!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents rawBasicAt: rawBasicAt:put: size yourself)
('as yet unclassified' revisar)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed: removeDependent:)
('error handling' assert: assert:description: caseError doesNotUnderstand: error: halt halt: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: explore hasContentsInExplorer inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('debugging-haltOnce' clearHaltOnce haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('updating' changed update:)
!


!FileList reorganize!
('file list' fileList fileListIndex fileListIndex: selectedFileEntry)
('user interface support' autoCompleterClassFor: editorClassFor:)
('file list menu' itemsForAnyFile1 itemsForAnyFile2 itemsForFileEntry: itemsForNoFile)
('file menu action' addNew:byEvaluating: addNewDirectory addNewFile deleteFile get getHex renameFile sortByDate sortByName sortBySize)
('initialization' buttonSelectorsToSuppress directory: dynamicButtonServices initialDirectoryList initialize labelString universalButtonServices updateDirectory)
('menu messages' copyName)
('own services' serviceAddNewDirectory serviceAddNewFile serviceCopyName serviceDeleteFile serviceGet serviceGetHex serviceRenameFile serviceSortByDate serviceSortByName serviceSortBySize serviceViewContentsInWorkspace viewContentsInWorkspace)
('volume list and pattern' deleteDirectory directory doesPattern:allow: fileNameFormattedFrom:namePad:sizePad:sizeWithCommasPad: listForSelectingPatterns:rejectingPatterns: pattern pattern:)
('private' acceptedStringOrText currentDirectorySelected defaultContents fileNameFromFormattedItem: folderString fullName isFileSmalltalkCode postOpen put: readContentsBrief: readContentsHex: resort: setSelectedDirectoryTo: sortBlock sortingByDate sortingByName sortingBySize updateFileList)
('accessing' brevityState)
('file name list' fileName)
!


!Inspector reorganize!
('accessing' baseFieldList context: fieldList object: selectedClass selectedClassOrMetaClass update wantsSteps)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('code' doItContext)
('constants' indexHead indexLabelsForSize: indexSize indexTail lastMetaField printStringLimit)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize initializeEvents inspect:)
('selecting' accept: replaceSelectionValue: safelyPrintWith: selectedObjectIndex selectedSlotName selection selectionIndex selectionIndexNonMeta selectionIsMeta selectionIsUnmodifiable selectionMeta selectionMetaUnknown selectionPrintString toggleIndex:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('stepping' stepAt:)
('testing' is:)
('user commands' inspectSelection)
('user interface support' autoCompleterClassFor: displayLabel editorClassFor: suggestObjectSpecificMenuItemsFor:from: textStylerClassFor:)
('private' changedSelectionIndex classDefinitionChangedFrom:to: objectClass objectClassInstSize objectSize printStringErrorFieldName printStringErrorText)
('private - mirror interface' object:basicAt: object:instVarAt: objectClass: objectSize:)
('doIt/accept/explain' doItReceiver)
!


!Behavior reorganize!
('accessing' classDepth decompilerClass format name parserClass sourceCodeTemplate theNonMetaClass typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary hasChangedComparedTo: instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesPreviousTo: allSuperclassesUpTo: subclasses subclassesDo: subclassesDoGently: superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesPreviousTo: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!

