'From Cuis6.3 [latest update: #6227] on 14 February 2024 at 10:29:03 am'!

!Workspace class reorganize!
('window color' windowColor)
('menu-world' worldMenuOptions)
('ui' open)
!


!SmallFloat64 class reorganize!
('instance creation' basicNew basicNew:)
!


!SmallInteger class reorganize!
('instance creation' basicNew new)
('constants' maxVal minVal)
('class initialization' initMinValAndMaxVal)
!


!UnicodeSymbol reorganize!
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('comparing' =)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('testing' isInfix isInitializePvtSelector isKeyword isPvtSelector isSymbol isTestSelector isUnary isValidBinarySelector isValidKeywordSelector isValidSelector isValidUnarySelector testSelectorPrefix)
('printing' isLiteral print storeOn:)
('uCompletion' separateKeywords)
('private' errorNoModification species)
('system primitives' flushCache)
('evaluating' value:)
!


!TextEditor class reorganize!
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcutsSpec)
('class initialization' initializeShortcuts)
('misc' openHelp)
('cached state access' changeText findText releaseClassCachedState)
('menu-world' worldMenuOptions)
('ui' open)
!


!TextCommand reorganize!
('actions' doOn: undoOn:)
!


!SystemVersion reorganize!
('accessing' highestUpdate highestUpdate:lastUpdateDate: registerUpdate: updatesPrefix versionMajor versionMinor versionRevision versionString)
('printing' printOn:)
('testing' isCuis isPharo isRelease isSqueak lastUpdateDate releaseDate)
('initialization' beNumberedRelease beRollingRelease versionMajor:versionMinor:)
!


!LabelMorph reorganize!
('accessing' contents contents: contentsFromPrintString: font font:emphasis: fontToUse label)
('drawing' drawOn:)
('font' emphasis: font: fontFamily:)
('initialization' defaultBorderWidth defaultColor initWithContents:font:emphasis: initialize)
('menu' addCustomMenuItems:hand: changeEmphasis changeFont changeFontSize)
('printing' printOn:)
('geometry' fitContents fontPreferenceChanged measureContents minimumExtent)
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: isHighlighted makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minimumExtent rescaleButtons)
('geometry services' fullyCoveredRectangle)
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
('user interface' canDiscardEditsOf: forceSaveContents forceSaveContentsTo: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!


!MessageNamesWindow class reorganize!
('menu-world' worldMenuOptions)
!


!ProcessBrowserWindow class reorganize!
('menu-world' worldMenuOptions)
!


!TranscriptWindow class reorganize!
('menu-world' worldMenuOptions)
!


!TextParagraphMorph reorganize!
('accessing' contents: extentForComposing setHeightOnContent shrinkFromExtent:)
('initialization' initialize)
('drawing' drawOn:)
('geometry' adjustExtent borderWidth: extentChanged: minimumExtent privateExtent: reflowAndMaybeResize)
!


!SHParserST80 reorganize!
('accessing' blockDepths blockDepthsStartIndexes classOrMetaClass classOrMetaClass: last3Ranges lastRange ranges rangesWithoutExcessCode source source: workspace:)
('scan' currentChar nextChar peekChar scanBinary scanComment scanIdentifier scanNext scanNumber scanPast: scanPast:level: scanPast:start:end: scanWhitespace skipBigDigits: skipDigits)
('parse support' enterBlock initializeInstanceVariables leaveBlock pushArgument: pushTemporary:)
('error handling' error failUnless: failWhen:)
('token testing' isAnsiAssignment isAssignment isBinary isKeyword isName isTokenExternalFunctionCallingConvention)
('character testing' isBigDigit:base: isBinarySelectorCharacter:)
('identifier testing' isBlockArgName: isBlockTempName: isClassVarName: isGlobal: isIncompleteBlockArgName: isIncompleteBlockTempName: isIncompleteClassVarName: isIncompleteGlobal: isIncompleteInstVarName: isIncompleteMethodArgName: isIncompleteMethodTempName: isIncompletePoolConstantName: isIncompleteReservedName: isIncompleteWorkspaceVarName: isInstVarName: isMethodArgName: isMethodTempName: isPoolConstantName: isWorkspaceVarName: resolve: resolvePartial: resolvePartialPragmaArgument: resolvePragmaArgument:)
('parse' parse parse: parseArray parseBacktick parseBinary parseBinaryMessagePattern parseBlock parseBlockArguments parseBlockTemporaries parseBraceArray parseByteArray parseCascadeAndChain parseCharSymbol parseExpression parseExternalCall parseKeyword parseKeywordMessagePattern parseLiteral: parseLiteralArrayElement parseMessagePattern parseMethodTemporaries parsePragmaBinary parsePragmaKeyword parsePragmaSequence parsePrimitive parseStatement parseStatementList parseStatementListForBraceArray parseString parseStringOrSymbol parseSymbol parseSymbolIdentifier parseSymbolSelector parseSymbolString parseTerm parseUnary parseUnaryMessagePattern)
('recording ranges' rangeType: rangeType:start:end:)
('indentifier iteration' blockArgNamesDo: blockTempNamesDo: classVarNamesDo: instVarNamesDo: methodArgNamesDo: methodTempNamesDo: namesBeginningWith:do: nonGlobalNamesDo: poolConstantNamesDo: reservedNames workspaceNamesDo:)
('aux' namesBeginningWith:do:in:)
!


!SHTextStylerST80 class reorganize!
('style table' attributeArrayForColor:emphasis: attributesFor: defaultStyleTable initialTextAttributes styleTable)
('class initialization' initialize releaseClassCachedState)
!

