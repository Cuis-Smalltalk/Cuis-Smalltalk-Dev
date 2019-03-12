'From Cuis 5.0 of 7 November 2016 [latest update: #3652] on 11 March 2019 at 3:33:12 pm'!
"Change Set:		3653-CuisCore-AuthorName-2019Mar11-14h53m
Date:			11 March 2019
Author:			Nahuel Garbezza

Categorize a bunch of uncategorized methods"!


!BasicInspector reorganize!
('initialization' inspect:)
!


!CodePackageList reorganize!
('accessing' description description: packageDirtyFlags packageFullNames packageNames packages selection selectionIndex selectionIndex: summary)
('events' updateDirtyFlags updatePackageList)
('commands' save)
('initialization' initialize)
('method nodes' methodNodeOf:ifErrorsParsing:)
('user interface support' editorClassFor:)
!


!ClassCommentReader reorganize!
('fileIn/Out' scanFrom:)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: customizeExplorerContents eighth fifth fillWith: first first: fourth from:to:put: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate quickIndexOf: replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: third)
('comparing' = hasEqualElements: hash)
('converting' @ asByteArray asColorArray asNewArray concatenation isSequenceable printStringWithNewline readStream reverse reversed writeStream)
('copying' , copyAfter: copyAfterLast: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withNextDo: withPreviousDo:)
('private' asDigitsAt:in:do: checkedAt: combinationsAt:in:after:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -= derivative incrementFraction integral)
('assertions' assertIsInBounds:)
!


!Color reorganize!
('access' alpha blue brightness chroma green hue icon iconOrThumbnailOfSize: luminance red saturation swatch)
('conversions' asNontranslucentColor bitPatternForDepth: bitPatternForGrayForm closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor indexInMap: makeForegroundColor pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth:)
('equality' diff: rgbDistance:)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('other' colorName name)
('printing' hexStringRGB printOn: printString storeArrayOn: storeArrayValuesOn: storeOn:)
('queries' isBlack isOpaque isTransparent isWhite)
('selection' isBlue isCyan isGreen isMagenta isRed isYellow)
('testing' is: isCollection mightBeTranslucent)
('transformations' * + - / adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter slightlyDarker slightlyLighter slightlyWhiter twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('private' attemptToMutateError basicSetRed:green:blue: setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setRed:green:blue: setRed:green:blue:range:)
('as yet unclassified' color)
('object serialization' convertToCurrentVersion:refStream:)
!


!Bitmap reorganize!
('printing' printOn:)
('accessing' atAllPut: bitPatternForDepth: byteAt:bigEndian: byteAt:put:bigEndian: byteSize bytesAt: bytesAt:put: defaultElement integerAt: integerAt:put: pixelValueForDepth: primFill: replaceFrom:to:with:startingAt:)
('testing' isColormap)
('converting' asByteArray)
!


!DummyStream reorganize!
('accessing' contents nextPut: nextPutAll: originalContents)
('as yet unclassified' padToEndIfCantTruncate)
('character writing' newLine space tab)
('error handling' subclassResponsibility)
('file modes' useBytes useCharacters)
('positioning' position position: skip:)
!


!AttemptToWriteReadOnlyGlobal reorganize!
('exceptionDescription' description)
('private' isResumable)
!


!UnhandledError reorganize!
('priv handling' defaultAction devDefaultAction isResumable standaloneAppDefaultAction)
('accessing' exception exception:)
!


!BraceNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools: elements maxElementsForConsArray)
('testing' blockAssociationCheck: isComplex numElements)
('enumerating' casesForwardDo: casesReverseDo:)
('code generation (new scheme)' emitCodeForValue:encoder: sizeCodeForValue:)
('code generation' selectorForShortForm:)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' elements: elements:sourceLocations: matchBraceStreamReceiver:messages: matchBraceWithReceiver:selector:arguments:)
!


!TextEmphasis reorganize!
('as yet unclassified' dominates: emphasisCode emphasisCode:)
('iterating' forTextEmphasisDo:)
('testing' dominatedByNormalEmphasis isSet)
('comparing' = hash)
('printing' printOn:)
!


!DiskProxy reorganize!
('as yet unclassified' constructorArgs constructorSelector global:preSelector:selector:args: global:selector:args: globalObjectName preSelector preSelector: simpleGlobalOrNil)
('object serialization' comeFullyUpOnReload: storeDataOn:)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumn buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCodePane buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' systemCatListKey:from: systemCatSingletonKey:from:)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('refactorings' addInstVar addParameter classRefactoringMenu messageRefactoringMenu openClassRefactoringMenu openMessageRefactoringMenu pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('commands' findClass)
!


!TileResizeMorph reorganize!
('as yet unclassified' action: resizeAtPoint: selectResize: selectionColor: selectionRectangle:)
('drawing' drawOn:)
('event handling testing' handlesMouseDown: handlesMouseHover)
('initialization' initialize)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseHover:localPosition:)
!

