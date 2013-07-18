'From Cuis 4.1 of 12 December 2012 [latest update: #1739] on 25 June 2013 at 11:35:43.773512 pm'!

!WeakMessageSend class reorganize!
('instance creation' new receiver:selector: receiver:selector:argument: receiver:selector:arguments:)
!


!FileStream reorganize!
('accessing' contents contentsOfEntireFile directoryEntry mimeTypes next next: nextPut: nextPutAll: size)
('testing' atEnd)
('positioning' position position: reset setToEnd skip: truncate:)
('printing' longPrintOn: longPrintOn:limitedTo:indent: printOn:)
('file open/close' close closed flush reopen)
('file modes' ascii binary readOnly readWrite text)
('file accessing' file localName name)
('fileIn/Out' fileIn fileInObjectAndCode)
('converting' asBinaryOrTextStream)
!


!Stream class reorganize!
('instance creation' new)
!


!MorphicEvent class reorganize!
('as yet unclassified')
!


!LayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newRow)
('examples' example1 example11 example13 example2 example3 example6 launcherExample)
!


!InnerTextMorph class reorganize!
('new-morph participation' includeInNewMorphMenu)
!


!HandMorph class reorganize!
('new-morph participation' includeInNewMorphMenu)
!


!HaloMorph class reorganize!
('class initialization' initialize)
('accessing - icons' haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon)
!


!VersionsBrowserWindow reorganize!
('GUI building' buildMorphicWindow optionalModelButtonTuples windowColor)
('menu building' classCommentVersionsMenu listMenu methodVersionsMenu)
('keyboard shortcuts' changeListKey:from:)
!


!SystemWindow class reorganize!
('top window' closeTopWindow noteTopWindowIn:but: sendTopWindowToBack topWindow windowsIn:satisfying:)
('instance creation' editText:label:wrap: open: open:label:)
!


!BrowserCommentTextMorph class reorganize!
('as yet unclassified')
!


!PluggableScrollPane class reorganize!
('as yet unclassified')
!


!PasteUpMorph class reorganize!
('system startup' shutDown startUp)
!


!BorderedRectMorph class reorganize!
('as yet unclassified')
!


!Morph class reorganize!
('class initialization' initialize)
('initialize-release' unload)
('instance creation' initializedInstance)
('new-morph participation' includeInNewMorphMenu)
!


!MessageSend class reorganize!
('instance creation' receiver:selector: receiver:selector:argument: receiver:selector:arguments:)
!


!Cursor class reorganize!
('class initialization' initBottomLeft initBottomRight initCorner initCrossHair initDown initMarker initMenu initMove initNormal initNormalWithMask initOrigin initRead initResizeLeft initResizeTop initResizeTopLeft initResizeTopRight initRightArrow initSquare initTarget initTopLeft initTopRight initUp initWait initWrite initXeq initialize makeCursorsWithMask startUp)
('instance creation' extent:fromArray:offset: new resizeForEdge:)
('current cursor' currentCursor currentCursor:)
('constants' blank bottomLeft bottomRight corner crossHair down execute marker menu move normal origin read resizeBottom resizeBottomLeft resizeBottomRight resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight rightArrow square target topLeft topRight up wait webLink write)
!


!ColorForm class reorganize!
('as yet unclassified' mappingWhiteToTransparentFrom: twoToneFromDisplay:using:backgroundColor:)
!


!Form class reorganize!
('instance creation' dotOfSize: extent: extent:depth: extent:depth:bits: extent:depth:fromArray:offset: extent:fromArray:offset: extent:offset: fakeSphereAt:diameter:color: fromBinaryStream: fromDisplay: fromFileNamed: fromFileNamedOrNil: fromUser fromUserWithExtent: verticalGradient:topColor:bottomColor:)
('mode constants' and blend blendAlpha erase erase1bitShape oldErase1bitShape oldPaint over paint paintAlpha reverse rgbMul under)
('examples' exampleBorder exampleSketch exampleSpaceFill makeStar toothpaste: xorHack:)
('shut down' shutDown)
('class initialization' unload)
('creation - anti aliased' bottomLeftCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom:)
!


!Color class reorganize!
('instance creation' colorFrom: colorFromPixelValue:depth: fromArray: fromRgbTriplet: fromString: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: new r:g:b: r:g:b:alpha: r:g:b:range: random random2)
('class initialization' initialize initializeGrayToIndexMap initializeIndexedColors initializeNames named:put:)
('examples' colorRampForDepth:extent: experimentsTowarsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('named colors' aqua beige black blue brightGreen brown cyan darkBlue darkGray darkGreen darkPink darkPurple darktan forestGreen gray green grey hotPink indigo lavender lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightPink lightPurple lightRed lightYellow lilac lime limeGreen magenta maroon mauve mustard navyBlue olive oliveGreen orange paleBlue paleBuff paleGreen paleMagenta paleOrange palePeach paleRed paleTan paleYellow peach periwinkle pink purple red royalBlue salmon seaGreen skyBlue tan teal transparent turquoise veryDarkGray veryLightGray veryPaleRed veryVeryDarkGray veryVeryLightGray violet white yellow)
('colormaps' cachedColormapFrom:to: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapFrom:to: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor:)
('other' indexedColors maskingMap: shutDown)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper: fromUser oldColorPaletteForDepth:extent:)
('color name lookup' colorNames colorNamesDict defaultColorNamesDictionary doesNotUnderstand: exactColorNamed: setColorNamesDict: traditionalColorNamesDictionary xkcdFirst48ColorNamesDictionary)
('selection' blueColorDict blueColorDict: blueGreenColorDict blueGreenColorDict: brightColorDict brightColorDict: brownColorDict brownColorDict: darkColorDict darkColorDict: grayColorDict: greenColorDict greenColorDict: greyColorDict greyColorDict: lightColorDict lightColorDict: orangeColorDict orangeColorDict: pastelColorDict pastelColorDict: pinkColorDict pinkColorDict: purpleColorDict purpleColorDict: redColorDict redColorDict: saturatedColorDict saturatedColorDict: yellowColorDict yellowColorDict:)
('color space conversions' applySimpleGamma:to: linearTosRGBGamma: sRGBGammaToLinear:)
!


!Text class reorganize!
('instance creation' fromString: fromUser initialFont:string:attribute: initialFont:stringOrText: new: streamContents: string: string:attribute: string:attributes: withForm:)
('private' addAttribute:toArray: setParagraphAttributes:toArray: string:runs:)
!


!CompiledMethod class reorganize!
('class initialization' fullFrameSize initialize smallFrameSize)
('instance creation' basicNew: new new: newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive: newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive:flag: newFrom: newInstanceFrom:variable:size:map: newMethod:header: primitive:numArgs:numTemps:stackSize:literals:bytecodes:trailer: toReturnConstant:trailerBytes: toReturnField:trailerBytes: toReturnSelf toReturnSelfTrailerBytes:)
('constants' abstractMarkers conflictMarker disabledMarker explicitRequirementMarker implicitRequirementMarker subclassResponsibilityMarker)
('services' timeStamp:partsDo: unboundMethods)
('evaluating' receiver:withArguments:executeMethod:)
!


!Array class reorganize!
('brace support' braceStream: braceWith: braceWith:with: braceWith:with:with: braceWith:with:with:with: braceWithNone)
!


!BasicClassOrganizer class reorganize!
('as yet unclassified')
!


!Metaclass reorganize!
('initialize-release' adoptInstance:from: instanceVariableNames:)
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('compiling' acceptsLoggingOfCompilation bindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
!


!ChangeSorter class reorganize!
('class initialization' unload)
!


!CodeProvider class reorganize!
('as yet unclassified')
!


!Workspace class reorganize!
('window color' windowColor)
!


!PluggableTextModel class reorganize!
('instance creation' on:)
!


!Object class reorganize!
('instance creation' initializedInstance newFrom: readFrom: unStream:)
('documentation' howToModifyPrimitives whatIsAPrimitive)
('object serialization' createFrom:size:version:)
('class initialization' initialize)
('windowColor' windowColor)
('compiling' noteCompilationOf:meta:)
!

