'From Cuis 5.0 of 7 November 2016 [latest update: #3558] on 28 December 2018 at 11:02:37 am'!

!ClassDescription class methodsFor: 'utilities' stamp: 'jmv 12/28/2018 10:34:16'!
printPackageExtensionCategories
	"In a bare image, without any packages, should print nothing
	ClassDescription printPackageExtensionCategories
	ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories].
	"
	ClassDescription allSubInstances do: [ :cls |
		cls organization categories do: [ :cat |
			(cat beginsWith:  '*') ifTrue: [
				{cls. cat} print ]]].! !


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
('events-old protocol' addDependent: breakDependents changed changed: removeDependent: update:)
('error handling' assert: assert:description: caseError confirm: confirm:orCancel: doesNotUnderstand: error: halt halt: handles: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isFloat isFraction isInteger isInterval isMethodProperties isNumber isPoint isPseudoContext isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy explore hasContentsInExplorer inform: notYetImplemented notify:)
('private' errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
!


!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit removeClass renameClass)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:messageAt: categorizeUnderCategoryAt:selector: categorizeUnderNewCategoryMessageAt: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newCategoryNameIfNone: potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
!


!MessageNames class reorganize!
('as yet unclassified')
!


!CodeFileBrowser class reorganize!
('instance creation' browseCode: browsePackage: fileReaderServicesForFile:suffix: serviceBrowseCode serviceBrowsePackage)
!


!ChangeList class reorganize!
('public access' browseContents: browseFrom:on:labeled: browsePackageContents: browseRecent:on: browseRecentLog browseRecentLogOn: browseRecentLogOn:startingFrom:)
('fileIn/Out' fileReaderServicesForFile:suffix: serviceContents servicePackageContents serviceRecentChanges)
!


!ChangeSorter class reorganize!
('as yet unclassified')
!


!FileList class reorganize!
('file reader registration' itemsForFile: registerFileReader: unregisterFileReader:)
!


!SmallInteger class reorganize!
('instance creation' basicNew new)
('constants' maxVal minVal)
('fileIn/Out' definition)
('class initialization' initMinValAndMaxVal)
!


!ArrayedCollection reorganize!
('accessing' size)
('adding' add:)
('printing' storeOn:)
('private' defaultElement storeElementsFrom:to:on:)
('sorting' isSorted isSortedBy: mergeFirst:middle:last:into:by: mergeSortFrom:to:by: mergeSortFrom:to:src:dst:by: quickSortFrom:to:by: sort sort:)
('objects from disk' byteSize bytesPerBasicElement bytesPerElement restoreEndianness writeOn:)
('inserting' insert:shiftingRightAt:)
!


!StandardFileStream reorganize!
('open/close' close closed ensureOpen open:forWrite: openReadOnly reopen)
('properties-setting' isBinary isReadOnly readOnly readWrite useBytes useCharacters)
('access' directory fileEntry fullName isDirectory localName name peekFor: printOn: reset size)
('read, write, position' atEnd basicNext crc16 findString: findStringFromEnd: flush next next: next:into:startingAt: next:putAll:startingAt: nextPut: nextPutAll: nextWordsInto: padToEndIfCantTruncate peek peekLast position position: readInto:startingAt:count: setToEnd skip: truncate truncate: upTo: upToEnd)
('primitives' primAtEnd: primClose: primCloseNoError: primFlush: primGetPosition: primOpen:writable: primRead:into:startingAt:count: primSetPosition:to: primSize: primSizeNoError: primTruncate:to: primWrite:from:startingAt:count:)
('registry' register unregister)
('finalization' actAsExecutor finalize)
('private' collectionSpecies disableReadBuffering enableReadBuffering flushReadBuffer)
!


!UndeclaredVariableReference reorganize!
('accessing' parser parser: varEnd varEnd: varName varName: varStart varStart:)
('handling' declareTempAndResume defaultAction)
!


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:in: bindTemp:range: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: globalSourceRanges messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parseNodeIncluding:ifAbsent: rangesForInstanceVariable:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForTemporaryVariable:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap sourceRangeFor: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' arguments arguments: body primitiveErrorVariableName removeProperty: temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' parseNodeIncluding:ifAbsent: rangesForInstanceVariable:ifAbsent: rangesForTemporaryVariable:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordsRanges selectorLastPosition)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
!


!TextEditor class reorganize!
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcutsSpec)
('class initialization' abandonChangeText initializeShortcuts)
('misc' openHelp)
('cached state access' changeText findText releaseClassCachedState)
('menu-world' worldMenuForOpenGroup)
('instance creation' openTextEditor)
!


!Form class reorganize!
('instance creation' dotOfSize: extent: extent:depth: extent:depth:bits: extent:depth:fromArray:offset: extent:fromArray:offset: extent:nativeDepthFrom: extent:nativeDepthFrom:bits: extent:offset: fakeSphereAt:diameter:color: fromBinaryStream: fromDisplay: fromFileEntry: fromFileNamed: fromFileNamedOrNil: fromUser fromUserWithExtent: verticalGradient:topColor:bottomColor:)
('mode constants' and blend blendAlpha erase erase1bitShape oldErase1bitShape oldPaint over paint paintAlpha reverse rgbMul under)
('examples' toothpaste: xorHack:)
('creation - anti aliased' bottomLeftCorner:height:gradientTop:gradientBottom: bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth: bottomRightCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom:borderWidth: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom:)
!


!Morph class reorganize!
('instance creation' initializedInstance)
('new-morph participation' includeInNewMorphMenu)
!


!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: verticalScrollBar)
('access options' hideScrollBarsIndefinitely)
('drawing' clipsLastSubmorph)
('events' doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition: mouseScroll:localPosition: scroller)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseScroll:)
('geometry' borderWidth: focusIndicatorExtent focusIndicatorRectangle hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar minimumExtent privateExtent: scrollDeltaHeight scrollDeltaWidth scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableExtent viewableHeight viewableWidth xtraBorder)
('geometry testing' hIsScrollbarShowing vIsScrollbarShowing)
('initialization' initialize innerMorphClass scrollBarClass)
('scroll bar events' mouseButton2Activity)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollSelectionIntoView scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('testing' is:)
!


!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' acceptDropsFrom:performing:whenOutsideList: makeItemsDraggable makeItemsUndraggable rejectDrops)
('drawing' drawOn: flashRow:)
('dropping/grabbing' acceptDroppingMorph:atRow:withEvent: acceptDroppingMorph:event: acceptDroppingMorph:outsideListWithEvent: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: mouseButton2Activity)
('event handling testing' allowsMorphDrop handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter:)
('events-processing' processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction: textColor)
('keyboard navigation' arrowKey:)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: scrollSelectionIntoView selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' is:)
('updating' update: updateList verifyContents)
('private' itemsAreDraggable listMorph)
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' addPossiblyUncoveredAreasIn:to: drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minPaneHeightForReframe minPaneWidthForReframe minimumExtent rescaleButtons)
('initialization' boxExtent createCloseBox createCollapseBox createExpandBox createMenuBox defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: buildWindowMenu changeColor fullScreen makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor:)
('printing' printOn:)
('resize/collapse' expandBoxHit resize resize: resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing' submorphsDrawingOutsideReverseDo:)
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is: isOpaqueMorph)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: isItOkToLooseChanges okToChange okToChangeDueTo:)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumn buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCodePane buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses findClass openSystemCategoryBrowser)
('keyboard shortcuts' systemCatListKey:from: systemCatSingletonKey:from:)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
!


!HierarchyBrowserWindow reorganize!
('keyboard shortcuts' systemCatSingletonKey:from:)
!


!ObjectExplorerWindow reorganize!
('GUI building' buildMorphicWindow expandAll initialExtent)
('building menus' genericMenu)
('keyboard shortcuts' explorerKey:from:)
('menu commands' browseClassRefs browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelection monitor: openWeightExplorer stopMonitoring)
('menu commands - reference finder' objectsToSkipDuringRescan openReferencesFinder rescan)
('stepping' stepAt:)
!


!WorkspaceWindow reorganize!
('menu' addCustomMenuItems:hand: offerWindowMenu)
!


!UpdatingMenuItemMorph reorganize!
('stepping' stepAt: wantsSteps)
('testing' is: stepTime)
('wording' wordingProvider:wordingSelector:)
('world' updateContents)
!


!Theme class reorganize!
('instance creation' beCurrent)
('accessing' content content: current)
('importing/exporting' bootstrap)
('class initialization' currentTheme:)
('user interface' changeFontSizes changeIcons changeTheme)
('consistency verification' verifyShoutConfig)
!


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntry)
('entries - private ' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: canComputeMessageEntriesFor:and: classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: computeIdentifierEntries computeMessageEntries: computeMessageEntriesFor:at:in:and: computeMessageEntriesForSelectors: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and: insert:and:at:to: parse:in:and: returnTypeOfEnclosedExpressionAt: returnTypeOfUnaryMessage:at: selectedClassOrMetaClassIn: sortByLastUsedTime: textProviderOrModel withParserSourceMethodNodeDo:ifError:)
('testing' opensWithTab)
!


!SHParserST80 reorganize!
('accessing' blockDepths blockDepthsStartIndexes classOrMetaClass classOrMetaClass: last3Ranges lastRange ranges source source: workspace:)
('scan' currentChar nextChar peekChar scanBinary scanComment scanIdentifier scanNext scanNumber scanPast: scanPast:level: scanPast:start:end: scanWhitespace skipBigDigits: skipDigits)
('parse support' enterBlock initializeInstanceVariables leaveBlock pushArgument: pushTemporary:)
('error handling' error failUnless: failWhen:)
('token testing' isAnsiAssignment isAssignment isBinary isKeyword isName isTokenExternalFunctionCallingConvention)
('character testing' isBigDigit:base: isBinarySelectorCharacter:)
('identifier testing' isBlockArgName: isBlockTempName: isClassVarName: isGlobal: isIncompleteBlockArgName: isIncompleteBlockTempName: isIncompleteClassVarName: isIncompleteGlobal: isIncompleteInstVarName: isIncompleteMethodArgName: isIncompleteMethodTempName: isIncompletePoolConstantName: isIncompleteReservedName: isIncompleteWorkspaceVarName: isInstVarName: isMethodArgName: isMethodTempName: isPoolConstantName: isWorkspaceVarName: resolve: resolvePartial: resolvePartialPragmaArgument: resolvePragmaArgument:)
('parse' parse parse: parseArray parseBacktick parseBinary parseBinaryMessagePattern parseBlock parseBlockArguments parseBlockTemporaries parseBraceArray parseByteArray parseCascadeAndChain parseCharSymbol parseExpression parseExternalCall parseKeyword parseKeywordMessagePattern parseLiteral: parseLiteralArrayElement parseMessagePattern parseMethodTemporaries parsePragmaBinary parsePragmaKeyword parsePragmaSequence parsePrimitive parseStatement parseStatementList parseStatementListForBraceArray parseString parseStringOrSymbol parseSymbol parseSymbolIdentifier parseSymbolSelector parseSymbolString parseTerm parseUnary parseUnaryMessagePattern)
('recording ranges' rangeType: rangeType:start:end:)
('indentifier iteration' blockArgNamesDo: blockTempNamesDo: classVarNamesDo: instVarNamesDo: methodArgNamesDo: methodTempNamesDo: namesBeginningWith:do: nonGlobalNamesDo: poolConstantNamesDo: reservedNames workspaceNamesDo:)
('testing' isMessage: isPartialOrFullIdentifier:)
!

