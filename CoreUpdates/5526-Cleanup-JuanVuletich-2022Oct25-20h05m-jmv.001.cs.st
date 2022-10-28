'From Cuis 6.0 [latest update: #5515] on 25 October 2022 at 8:16:05 pm'!

!InlineMethodMessageSendsSet reorganize!
('initialization' addMessageNodeReference:ifIncluded: initialize initializeApplier:)
('message list' messageListIndex messageListIndex:)
!


!DictionaryInspector reorganize!
('accessing' fieldList)
('selecting' addEntry: calculateKeyArray nonFixedSelectionIndex refreshView replaceSelectionValue: selectedKey selection selectionIndexFixed)
('menu' removeSelection renameEntryTo:)
('private' numberOfFixedFields)
('initialization' initialize)
('user interface support' suggestObjectSpecificMenuItemsFor:from:)
!


!MessageTally reorganize!
('collecting leaves' bump: bump:fromSender: into:fromSender: leavesInto:fromSender:)
('comparing' < = > hash isPrimitives sonsOver:)
('printing' fullPrintExactOn: leavesPrintExactOn: printOn: printSenderCountsOn: treePrintExactOn:tabs:thisTab:)
('tallying' bumpBy: tally:by: tallyPath:by:)
('private' class:method: copyWithTally: method primitives: tally)
!


!Categorizer reorganize!
('accessing' addCategory: addCategory:before: allMethodSelectors categories categories: categoryOfElement: changeFromCategorySpecs: changeFromString: classify:under: classify:under:suppressIfDefault: classifyAll:under: elementCategoryDict isEmptyCategoryNumber: listAtCategoryNamed: listAtCategoryNumber: moveCategoryBottom: moveCategoryDown: moveCategoryTop: moveCategoryUp: numberOfCategoryOfElement: removeCategory: removeElement: removeEmptyCategories renameCategory:to: sortCategories)
('printing' printCategory:at:with:on: printOn: printOn:excludingExtensions:)
('fileIn/Out' scanFrom:)
('private' elementArray firstIndexOfCategoryNumber: lastIndexOfCategoryNumber: setDefaultList:)
('copying' postCopy)
('testing' hasAnyCategoriesSuchThat: isPackageCategoryExtension:)
!


!OrderedCollection reorganize!
('accessing' at: at:put: isEmpty size)
('adding' add: add:after: add:afterIndex: add:before: add:beforeIndex: addAllFirst: addAllLast: addFirst: addLast: at:ifAbsentPut: growSize)
('copying' copy copyFrom:to: copyReplaceFrom:to:with: copyWith: reversed)
('enumerating' collect: collect:from:to: collect:thenSelect: do: reverseDo: select: select:thenCollect: with:collect: with:with:collect: withIndexCollect:)
('inspecting' inspectorClass)
('removing' remove:ifAbsent: removeAll removeAllSuchThat: removeAt: removeFirst removeFirst: removeLast removeLast:)
('sorting' sort sort:)
('private' errorNoSuchElement find: insert:before: makeRoomAtFirst makeRoomAtLast removeIndex: reset resetTo: setCollection: setContents: species)
('comparing' =)
('converting' asNewArray)
('initialization' initializeOfSize:)
!


!Set reorganize!
('accessing' atRandom: like: size)
('adding' add: add:withOccurrences:)
('copying' postCopy)
('enumerating' collect: do: union:)
('comparing' emptyCollectionHash)
('objects from disk' comeFullyUpOnReload:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: rehash scanFor: swap:with: withArray:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = includes: is: occurrencesOf:)
('inspecting' inspectorClass)
!


!Trie reorganize!
('accessing' at: at:ifAbsent: at:ifAbsentPut: at:ifPresent: at:put:)
('adding' add:)
('removing' remove:ifAbsent: removeKey: removeKey:ifAbsent:)
('enumerating' do: forPrefix:keysAndValuesDo: keysAndValuesDo: keysDo: valuesDo:)
('testing' includes: includesKey: includesValue: isEmpty)
('private' errorKeyNotFound)
('printing' storeOn:)
!


!ReadWriteStream class reorganize!
('as yet unclassified')
!


!UniFileStream class reorganize!
('instance creation' new)
('registry' register: registry retryWithGC:until:forFileNamed: unregister:)
('error handling' fileDoesNotExistUserHandling: fileExistsUserHandling: readOnlyFileDoesNotExistUserHandling:)
('cached state access' releaseClassCachedState)
('file creation' crc16OfFileNamed:)
!


!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend suspendAndUnblock terminate)
('changing suspended state' activateReturn:value: canSearchForSignalerContextOf: complete: completeStep: completeStepUpTo: completeTo: ifSuspendedContextIsUnhandledErrorDo: install: isSuspendedContextSignalUnhandledError popTo: popTo:value: restartTop restartTopWith: return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString storeOn:)
('private' complete:to: suspendAndReleaseCriticalSection suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debugContext:title: debugWithTitle:)
('signaling' signalException:)
('test support' suspendPrimitivelyOrFail terminateFromForkedProcess)
('testing' isActiveProcess isBlocked isReady isRunning isSuspended isTerminated isTheLastPreempted)
!


!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken matchReturn startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression isLiteralExpression isLiteralNumberExpression keywordPattern literalExpression literalNumberExpression messagePart:repeat: method:doIt:context: newMethodNode parenthesisExpression pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable variableExpression)
('temps' bindArg: bindArg:range: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: performInteractiveChecks: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('backtick' addToSentInLiterals: backtickExpression)
('comparing' match: matchToken:)
('temporaries' bindTemp:)
!


!Parser class reorganize!
('accessing' chainExamplesDoubleColon primitivePragmaSelectors)
('services' methodHeaderLengthFrom: selectorFrom:)
('parsing' parse:class: parse:class:noPattern:)
!


!ParseNode reorganize!
('code generation' pc pc:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('converting' asReturnNode)
('encoding' encodeSelector:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printSourceCode printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRange:basedOn: expandRange:basedOnChildRange: expandRanges:basedOn:using:)
('testing' assignmentCheck:at: canCascade ensureCanCascade: equivalentTo: isArg isAssignmentNode isAssignmentToTemporary isBacktickNode isBlockNode isBraceNode isCascadeNode isComplex isConstantCharacter isConstantNumber isFalsePseudoVariable isInstanceVariableNode isJust: isLiteralNode isLiteralVariableNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isMethodNode isNilPseudoVariable isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelectorNode isSelfBasicNewMessageSend isSelfNewMessageSend isSelfPseudoVariable isSpecialConstant isSuperPseudoVariable isTemp isTempOrArg isTemporariesDeclaration isTemporaryDeclaration isThisContextPseudoVariable isTruePseudoVariable isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine referencesSelf referencesSuper toDoIncrement:)
('tiles' currentValueIn:)
('visiting' accept: nodesDo:)
('private' consolidateAsCollection: ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
('accessing' comment comment:)
!


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:range: fixTemp: floatTemp: newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: completeSourceRangesBasedOn: criteriaToSortSourceRangeBetween:and: globalSourceRanges messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: parseNodesPathAt:using:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangeForNode:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap withMessageSendKeywordPositionsOf:do:ifAbsent: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables temps:literals:class:)
('initialize-release' noteSuper release)
('temporaries' maxTemp)
!


!ReturnNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('converting' asReturnNode)
('code generation' code)
('code generation (new scheme)' emitCodeForReturn:encoder: emitCodeForValue:encoder: sizeCodeForReturn: sizeCodeForValue:)
('printing' expr printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' equivalentTo: isImplicitSelfReturnIn: isReturn isReturnSelf isSpecialConstant isVariableReference referencesSelf referencesSuper)
('initialize-release' expr: expr:encoder:sourceRange:)
!


!DecompilerConstructor reorganize!
('visiting' accept:)
('constructor' codeAnyLitInd: codeAnyLiteral: codeAnySelector: codeArguments:block: codeArguments:temps:block: codeAssignTo:value: codeBlock:returns: codeBrace: codeCascade:messages: codeCascadedMessage:arguments: codeConstants codeEmptyBlock codeInst: codeMessage:selector:arguments: codeMethod:block:tempVars:primitive:class: codeRemoteTemp:remoteTemps: codeSelector:code: codeSuper codeTemp: codeTemp:named: codeThisContext decodeIfNilWithReceiver:selector:arguments: decodeLiteralVariableValueDereferenceWithReceiver:selector:arguments:)
('initialize-release' method:class:literals:)
!


!ParseStack reorganize!
('accessing' pop: position: push: size)
('results' position)
('printing' printOn:)
('initialize-release' init)
!


!SmalltalkEditor reorganize!
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualExtractAsParameter: contextualRemoveParameter contextualRemoveParameter: extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do:for:in:ifFail: afterCompiling:do:ifFail: debug:receiver:in: debugIt doIt doItProfiling: doItSourceCodeFor:in: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu openMenu:)
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' contextualInlineMethod contextualInlineMethod: contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable inlineTemporaryVariable:)
!


!TrieLeaf reorganize!
('accessing' at:ifAbsent:characterIndex: at:put:characterIndex:)
('removing' removeKey:ifAbsent:characterIndex:)
('enumerating' keysAndValuesDo:)
('testing' isEmpty)
!


!TrieNode reorganize!
('accessing' at:ifAbsent:characterIndex: at:put:characterIndex:)
('removing' removeKey:ifAbsent:characterIndex:)
('enumerating' forPrefix:keysAndValuesDo:characterIndex: keysAndValuesDo:)
('testing' isEmpty)
('private' childFor1:characterIndex:orAdd: childFor:characterIndex:orAdd: childForN:characterIndex:orAdd: removeChildFor:characterIndex: splitIfNeededFor:characterIndex:)
('private accessors' getChildren getChildrenFirstChars getSegmentSize getSomeKey setChildrenFirstChars:children: setSegmentSize:someKey:)
!


!GeometryTransformation reorganize!
('testing' doesMirror doesNotRotate is: isPureMirroring isPureTranslation)
('modifying' invertingYAxis: withCurrentYAxisInverted)
!


!StringRequestMorph reorganize!
('accessing' emptyTextDisplayMessage: layerNumber response response:)
('drawing' drawOn:)
('initialization' acceptBlock: addTextPane addTitle: cancelBlock: defaultBorderWidth defaultColor initialize setQuery:initialAnswer: validationBlock:)
('private' cancel getUserResponseOrCancel: selectionInterval textBox)
!


!HaloMorph reorganize!
('accessing' target target:)
('drawing' drawCoordinateSystemOn: drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:handPosition:hand: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' rejectsEvent: rejectsEventFully:)
('geometry testing' knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addExploreHandle: addGrabHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addResizeHandle: addRotateHandle: addScaleHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor delete)
('testing' is: isIncludedInTaskbar wantsHalo)
('private' addHandle: addHandles addHandles: addNameString: doDebug:with: doDrag:with: doDup:with: doExplore:with: doGrab:with: doMenu:with: doRecolor:with: doResize:with: doRot:with: doScale:with: endInteraction handlesBox maybeCollapse:with: maybeDismiss:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startResize:with: startRot:with: startScale:with:)
('forward to target' deleteBalloon mouseDownOnHelpHandle:)
('stepping' step stepTime wantsSteps)
!


!HaloMorph class reorganize!
('accessing - icons' haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloResizeIcon haloRotateIcon haloScaleIcon)
('cached state access' icons releaseClassCachedState)
('new-morph participation' includeInNewMorphMenu)
!


!ImageMorph reorganize!
('accessing' color: form image:)
('drawing' drawOn: minimumExtent)
('initialization' defaultBorderWidth initialize)
('menu commands' grabFromScreen readFromFile)
!


!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: contents: contentsAsIs: crAction crAction: disableEditing editor enableEditing escAction escAction: hasEditingConflicts hasEditingConflicts: isWrapped keystrokeAction: model: model:wrappedTo: positionInTextOf: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR confirmAcceptAnyway enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEditing handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeystrokeEvent:)
('events-processing' processKeystroke: processMouseMove:localPosition:)
('geometry' adjustExtent extentChanged: minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mouseButton2Activity mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' fontPreferenceChanged possiblyChanged someSubmorphPositionOrExtentChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnAny: acceptOnCR:)
('macpal' flash)
('miscellaneous' disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded stylerStyled)
('selection' scrollSelectionIntoView)
('classification' is:)
('copying' okayToDuplicate)
!


!UpdatingLabelMorph reorganize!
('accessing' getSelector: showPrintStringFor: stepTime stepTime: target:)
('initialization' initialize)
('stepping' stepAt: wantsSteps)
('geometry' fitContents)
!


!PluggableButtonMorph reorganize!
('accessing' actWhen: action: actionSelector adoptWidgetsColor: icon: iconDrawSelector iconDrawSelector: iconName iconName: label: label:font: magnifiedIcon: mouseOverColor: performAction roundButtonStyle:)
('drawing' draw3DLookOn: drawEmbossedLabelOn: drawOn: drawRegularLabelOn: drawRoundLookOn: enableColorWith: fontToUse)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition: mouseEnter: mouseLeave: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate)
('initialization' actionArgument: defaultBorderColor defaultBorderWidth enableSelector: initialize model:stateGetter:action:label: model:stateGetter:stateArgument:action:actionArgument:label: secondaryActionSelector: stateArgument:)
('updating' update:)
('private' getEnabled getState magnifiedIcon)
('testing' is: isDisabled isEnabled isPressed isRoundButton mouseIsOver)
('geometry' extentChanged: minimumExtent)
('enabling' disable enable)
!


!DraggeableButtonMorph reorganize!
('initialization' initialize)
('accessing' dragSelector: grabSelector:)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
!


!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' rejectDrops)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: keyboardFocused mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseLeave:)
('events-processing' clickedAt:maybeDeselect: invalidDrop: processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: deleteAction navigateDown navigateOnePageDown navigateOnePageUp navigateToBottom navigateToTop navigateUp)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' allowsMultipleSelection is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo:)
('scrolling' scrollBy: scrollSelectionIntoView scrollSiblings: scrollToShow: updateSiblingsScroller vScrollBarValue:)
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minimumExtent rescaleButtons)
('geometry services' on:ifCovered:uncoveredPartsDo:else:)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: addTileResizerMenuTo: addWindowControlTo: buildWindowMenu changeColor makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor: windowFrameColor)
('printing' printOn:)
('moving' moveBottom moveBottomLeft moveBottomRight moveLeft moveRight moveTop moveTopLeft moveTopRight)
('resize/collapse' allowedArea expandBoxHit resize resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!


!ChangeSelectorWizardStepWindow reorganize!
('actions' add addToList: do:withEnteredClassLabeled: inform:doesNotImplement: justRefactor refactor remove withClassNamed:do:)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('compile methods' compiledMethodsFrom:)
('button creation' createAddButton createCancelButton createJustRefactorButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:)
('accessing' oldSelector)
!


!InspectorWindow reorganize!
('GUI building' buildContentsText buildEvaluatorText buildList buildMorphicWindow initialExtent)
('menu building' basicMenuOptions fieldListMenu menuOptionsForBrowsing menuOptionsForDictionary menuOptionsForSet)
('menu commands' addEntry browseClassRefs browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelectedKey inspectSelection openReferencesFinder openWeightExplorer renameEntry sendersOfSelectedKey)
('keyboard shortcuts' inspectorKey:from:)
!


!FillInTheBlankMorph reorganize!
('accessing' emptyTextDisplayMessage: layerNumber response response: selectionInterval)
('initialization' createAcceptButton createCancelButton createQueryTextMorph: createTextPaneAcceptOnCR: defaultColor delete initialize responseUponCancel: setQuery:initialAnswer:acceptOnCR: sizeUnit)
('invoking' getUserResponse)
('menu' acceptClicked cancelClicked)
('drawing' drawOn:)
!


!AutoCompleterMorph reorganize!
('accessing' entryCount itemHeight itemsPerPage layerNumber selected selected:)
('actions' goDown goHome goPageDown goPageUp goToEnd goUp help resetMenu)
('activity tracking' lastActivity stillActive timeOfLastActivity timeout updateColor)
('drawing' adjust:ifOutOfScreenWith:xOffset:yOffset: calculateItemsPerPage calculateWidth colorOf: downButtonPosition drawContainingRectangle: drawDownArrowOn:thickness: drawItemOf:on:width:top: drawItemsOn:width: drawOn: drawScrollBarMovementBarOn:thickness: drawScrollBarOn: drawScrollBarRectangleOn:thickness: drawUpArrowOn:thickness: maxItemsPerPage setDefaultColors upButtonPosition)
('events' mouseButton1Up:localPosition: mouseEnter:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor setCompleter:position:)
('stepping' stepAt: stepTime wantsSteps)
('private' firstSelectableEntryIndex firstVisible firstVisible: lastSelectableEntryIndex lastVisible lastVisible: nextSelectableEntryIndexFrom: nextSelectableEntryIndexFrom:goingForwards: nextSelectableEntryIndexFromAndIncluding: previousSelectableEntryIndexFrom: wrapIndex:by:)
('testing' canSelectEntryAt: isXOutOfScreen:with: isYOutOfScreen:with:)
!


!LayoutSpec reorganize!
('initialization' initialize)
('accessing' fixedHeight fixedHeight: fixedOrMorphHeight: fixedOrMorphWidth: fixedWidth fixedWidth: morph morph: offAxisEdgeWeight offAxisEdgeWeight: proportionalHeight: proportionalHeight:minimum: proportionalWidth: proportionalWidth:minimum: setProportionalHeight: setProportionalWidth: useMorphHeight useMorphWidth)
('layout' fixedOrMinimumLayoutHeight fixedOrMinimumLayoutWidth minimumShrinkHeight minimumShrinkWidth minimumSpecExtent minimumSpecHeight minimumSpecWidth proportionaLayoutlHeight proportionalLayoutHeight proportionalLayoutWidth)
('testing' isProportionalHeight isProportionalWidth usesMorphExtent usesMorphHeight usesMorphWidth)
('private' privateFixedHeight privateFixedWidth privateProportionalHeight privateProportionalWidth)
!


!LayoutSpec class reorganize!
('instance creation' fixedExtent: fixedHeight: fixedWidth: fixedWidth:fixedHeight: fixedWidth:fixedHeight:offAxisEdgeWeight: fixedWidth:proportionalHeight: fixedWidth:proportionalHeight:offAxisEdgeWeight: keepMorphExtent morphHeightFixedWidth: morphHeightProportionalWidth: morphWidthFixedHeight: morphWidthProportionalHeight: proportionalHeight: proportionalWidth: proportionalWidth:fixedHeight: proportionalWidth:fixedHeight:offAxisEdgeWeight: proportionalWidth:proportionalHeight: proportionalWidth:proportionalHeight:offAxisEdgeWeight: useAll)
!

