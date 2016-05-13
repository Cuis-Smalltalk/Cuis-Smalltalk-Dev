'From Cuis 4.2 of 25 July 2013 [latest update: #2327] on 16 May 2015 at 5:28:40.8284 pm'!
'Description This package includes code required by VMMaker for building the Balloon VM plugin. In any case, Cuis does not use Balloon at all, so this code is only for use by VMMaker'!
!provides: 'Balloon' 1 4!
!classDefinition: #BalloonEngineConstants category: #'Balloon-Engine-Pools'!
SharedPool subclass: #BalloonEngineConstants
	instanceVariableNames: ''
	classVariableNames: 'BEAaLevelIndex BEBalloonEngineSize BEBitBltIndex BEClipRectIndex BEColorTransformIndex BEDeferredIndex BEDestOffsetIndex BEEdgeTransformIndex BEExternalsIndex BEFormsIndex BEPostFlushNeededIndex BESpanIndex BEWorkBufferIndex ETBalloonEdgeDataSize ETIndexIndex ETLinesIndex ETSourceIndex ETXValueIndex ETYValueIndex ETZValueIndex FTBalloonFillDataSize FTDestFormIndex FTIndexIndex FTMaxXIndex FTMinXIndex FTSourceIndex FTYValueIndex GBBaseSize GBBitmapDepth GBBitmapHeight GBBitmapRaster GBBitmapSize GBBitmapWidth GBColormapOffset GBColormapSize GBEndX GBEndY GBFinalX GBMBaseSize GBTileFlag GBUpdateDDX GBUpdateDDY GBUpdateDX GBUpdateDY GBUpdateData GBUpdateX GBUpdateY GBViaX GBViaY GBWideEntry GBWideExit GBWideExtent GBWideFill GBWideSize GBWideUpdateData GBWideWidth GEBaseEdgeSize GEBaseFillSize GEEdgeClipFlag GEEdgeFillsInvalid GEFAlreadyFailed GEFBadPoint GEFBitBltLoadFailed GEFClassMismatch GEFEdgeDataTooSmall GEFEngineIsInteger GEFEngineIsWords GEFEngineStopped GEFEngineTooSmall GEFEntityCheckFailed GEFEntityLoadFailed GEFFillDataTooSmall GEFFormLoadFailed GEFSizeMismatch GEFWorkBufferBadMagic GEFWorkBufferIsInteger GEFWorkBufferIsPointers GEFWorkBufferStartWrong GEFWorkBufferTooSmall GEFWorkBufferWrongSize GEFWorkTooBig GEFWrongEdge GEFWrongFill GEFWrongState GEFillIndexLeft GEFillIndexRight GENumLines GEObjectIndex GEObjectLength GEObjectType GEObjectUnused GEPrimitiveBezier GEPrimitiveClippedBitmapFill GEPrimitiveEdge GEPrimitiveEdgeMask GEPrimitiveFill GEPrimitiveFillMask GEPrimitiveLine GEPrimitiveLinearGradientFill GEPrimitiveRadialGradientFill GEPrimitiveRepeatedBitmapFill GEPrimitiveTypeMask GEPrimitiveUnknown GEPrimitiveWide GEPrimitiveWideBezier GEPrimitiveWideEdge GEPrimitiveWideLine GEPrimitiveWideMask GEStateAddingFromGET GEStateBlitBuffer GEStateCompleted GEStateScanningAET GEStateUnlocked GEStateUpdateEdges GEStateWaitingChange GEStateWaitingForEdge GEStateWaitingForFill GEXValue GEYValue GEZValue GErrorAETEntry GErrorBadState GErrorFillEntry GErrorGETEntry GErrorNeedFlush GErrorNoMoreSpace GFDirectionX GFDirectionY GFNormalX GFNormalY GFOriginX GFOriginY GFRampLength GFRampOffset GGBaseSize GLBaseSize GLEndX GLEndY GLError GLErrorAdjDown GLErrorAdjUp GLWideEntry GLWideExit GLWideExtent GLWideFill GLWideSize GLWideWidth GLXDirection GLXIncrement GLYDirection GWAAColorMask GWAAColorShift GWAAHalfPixel GWAALevel GWAAScanMask GWAAShift GWAETStart GWAETUsed GWBezierHeightSubdivisions GWBezierLineConversions GWBezierMonotonSubdivisions GWBezierOverflowSubdivisions GWBufferTop GWClearSpanBuffer GWClipMaxX GWClipMaxY GWClipMinX GWClipMinY GWColorTransform GWCountAddAETEntry GWCountChangeAETEntry GWCountDisplaySpan GWCountFinishTest GWCountInitializing GWCountMergeFill GWCountNextAETEntry GWCountNextFillEntry GWCountNextGETEntry GWCurrentY GWCurrentZ GWDestOffsetX GWDestOffsetY GWEdgeTransform GWFillMaxX GWFillMaxY GWFillMinX GWFillMinY GWFillOffsetX GWFillOffsetY GWGETStart GWGETUsed GWHasClipShapes GWHasColorTransform GWHasEdgeTransform GWHeaderSize GWLastExportedEdge GWLastExportedFill GWLastExportedLeftX GWLastExportedRightX GWMagicIndex GWMagicNumber GWMinimalSize GWNeedsFlush GWObjStart GWObjUsed GWPoint1 GWPoint2 GWPoint3 GWPoint4 GWPointListFirst GWSize GWSpanEnd GWSpanEndAA GWSpanSize GWSpanStart GWState GWStopReason GWTimeAddAETEntry GWTimeChangeAETEntry GWTimeDisplaySpan GWTimeFinishTest GWTimeInitializing GWTimeMergeFill GWTimeNextAETEntry GWTimeNextFillEntry GWTimeNextGETEntry'
	poolDictionaries: ''
	category: 'Balloon-Engine-Pools'!
!classDefinition: 'BalloonEngineConstants class' category: #'Balloon-Engine-Pools'!
BalloonEngineConstants class
	instanceVariableNames: ''!

!classDefinition: #BalloonBezierSimulation category: #'Balloon-Simulation'!
Object subclass: #BalloonBezierSimulation
	instanceVariableNames: 'start end via lastX lastY fwDx fwDy fwDDx fwDDy maxSteps'
	classVariableNames: 'HeightSubdivisions LineConversions MonotonSubdivisions OverflowSubdivisions'
	poolDictionaries: ''
	category: 'Balloon-Simulation'!
!classDefinition: 'BalloonBezierSimulation class' category: #'Balloon-Simulation'!
BalloonBezierSimulation class
	instanceVariableNames: ''!

!classDefinition: #BalloonBuffer category: #'Balloon-Engine'!
Object variableWordSubclass: #BalloonBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Balloon-Engine'!
!classDefinition: 'BalloonBuffer class' category: #'Balloon-Engine'!
BalloonBuffer class
	instanceVariableNames: ''!

!classDefinition: #BalloonEdgeData category: #'Balloon-Simulation'!
Object subclass: #BalloonEdgeData
	instanceVariableNames: 'index xValue yValue zValue lines source'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Balloon-Simulation'!
!classDefinition: 'BalloonEdgeData class' category: #'Balloon-Simulation'!
BalloonEdgeData class
	instanceVariableNames: ''!

!classDefinition: #BalloonEngine category: #'Balloon-Engine'!
Object subclass: #BalloonEngine
	instanceVariableNames: 'workBuffer span bitBlt forms clipRect destOffset externals aaLevel edgeTransform colorTransform deferred postFlushNeeded'
	classVariableNames: 'BezierStats BufferCache CacheProtect Counts Debug Times'
	poolDictionaries: 'BalloonEngineConstants'
	category: 'Balloon-Engine'!
!classDefinition: 'BalloonEngine class' category: #'Balloon-Engine'!
BalloonEngine class
	instanceVariableNames: ''!

!classDefinition: #BalloonFillData category: #'Balloon-Simulation'!
Object subclass: #BalloonFillData
	instanceVariableNames: 'index minX maxX yValue source destForm'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Balloon-Simulation'!
!classDefinition: 'BalloonFillData class' category: #'Balloon-Simulation'!
BalloonFillData class
	instanceVariableNames: ''!

!classDefinition: #BalloonLineSimulation category: #'Balloon-Simulation'!
Object subclass: #BalloonLineSimulation
	instanceVariableNames: 'start end xIncrement xDirection error errorAdjUp errorAdjDown'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Balloon-Simulation'!
!classDefinition: 'BalloonLineSimulation class' category: #'Balloon-Simulation'!
BalloonLineSimulation class
	instanceVariableNames: ''!

!classDefinition: #BalloonSolidFillSimulation category: #'Balloon-Simulation'!
Object subclass: #BalloonSolidFillSimulation
	instanceVariableNames: 'color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Balloon-Simulation'!
!classDefinition: 'BalloonSolidFillSimulation class' category: #'Balloon-Simulation'!
BalloonSolidFillSimulation class
	instanceVariableNames: ''!

!classDefinition: #BalloonState category: #'Balloon-Engine'!
Object subclass: #BalloonState
	instanceVariableNames: 'transform colorTransform aaLevel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Balloon-Engine'!
!classDefinition: 'BalloonState class' category: #'Balloon-Engine'!
BalloonState class
	instanceVariableNames: ''!


!BalloonBezierSimulation commentStamp: '<historical>' prior: 0!
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.!

!BalloonBuffer commentStamp: '<historical>' prior: 0!
BalloonBuffer is a repository for primitive data used by the BalloonEngine.!

!BalloonEdgeData commentStamp: '<historical>' prior: 0!
BalloonEdgeData defines an entry in the internal edge table of the Balloon engine.

Instance Variables:
	index	<Integer>	The index into the external objects array of the associated graphics engine
	xValue	<Integer>	The computed x-value of the requested operation
	yValue	<Integer>	The y-value for the requested operation
	height	<Integer>	The (remaining) height of the edge
	source	<Object>		The object from the external objects array!

!BalloonEngine commentStamp: '<historical>' prior: 0!
BalloonEngine is the representative for the Balloon engine inside Squeak. For most purposes it should not be used directly but via BalloonCanvas since this ensures proper initialization and is polymorphic with other canvas uses.!

!BalloonFillData commentStamp: '<historical>' prior: 0!
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.!

!BalloonLineSimulation commentStamp: '<historical>' prior: 0!
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.!

!BalloonSolidFillSimulation commentStamp: '<historical>' prior: 0!
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.!

!BalloonState commentStamp: '<historical>' prior: 0!
This class is a repository for data which needs to be preserved during certain operations of BalloonCanvas.!

!BalloonEngineConstants class methodsFor: 'pool definition' stamp: 'ar 5/18/2003 19:55'!
initEdgeConstants
	"Initialize the edge constants"

	"Edge primitive types"
	GEPrimitiveEdge := 2.			"External edge - not handled by the GE"
	GEPrimitiveWideEdge := 3.		"Wide external edge"
	GEPrimitiveLine := 4.			"Straight line"
	GEPrimitiveWideLine := 5.		"Wide line"
	GEPrimitiveBezier := 6.		"Quadratic bezier curve"
	GEPrimitiveWideBezier := 7.	"Wide bezier curve"

	"Special flags"
	GEPrimitiveWide := 16r01.		"Flag determining a wide primitive"
	GEPrimitiveWideMask := 16rFE.	"Mask for clearing the wide flag"
	GEEdgeFillsInvalid := 16r10000. "Flag determining if left/right fills of an edge are invalid"
	GEEdgeClipFlag := 16r20000.	"Flag determining if this is a clip edge"

	"General edge state constants"
	GEXValue := 4.					"Current raster x"
	GEYValue := 5.					"Current raster y"
	GEZValue := 6.					"Current raster z"
	GENumLines := 7.					"Number of scan lines remaining"
	GEFillIndexLeft := 8.				"Left fill index"
	GEFillIndexRight := 9.				"Right fill index"
	GEBaseEdgeSize := 10.				"Basic size of each edge"

	"General fill state constants"
	GEBaseFillSize := 4.				"Basic size of each fill"

	"General Line state constants"
	GLXDirection := 10.				"Direction of edge (1: left-to-right; -1: right-to-left)"
	GLYDirection := 11.				"Direction of edge (1: top-to-bottom; -1: bottom-to-top)"
	GLXIncrement := 12.				"Increment at each scan line"
	GLError := 13.						"Current error"
	GLErrorAdjUp := 14.				"Error to add at each scan line"
	GLErrorAdjDown := 15.				"Error to subtract on roll-over"
			"Note: The following entries are only needed before the incremental
			state is computed. They are therefore aliased to the error values above"
	GLEndX := 14.						"End X of line"
	GLEndY := 15.						"End Y of line"
	GLBaseSize := 16.					"Basic size of each line"

	"Additional stuff for wide lines"
	GLWideFill := 16.					"Current fill of line"
	GLWideWidth := 17.				"Current width of line"
	GLWideEntry := 18.				"Initial steps"
	GLWideExit := 19.					"Final steps"
	GLWideExtent := 20.				"Target width"
	GLWideSize := 21.					"Size of wide lines"

	"General Bezier state constants"
	GBUpdateData := 10.				"Incremental update data for beziers"
	GBUpdateX := 0.				"Last computed X value (24.8)"
	GBUpdateY := 1.				"Last computed Y value (24.8)"
	GBUpdateDX := 2.				"Delta X forward difference step (8.24)"
	GBUpdateDY := 3.				"Delta Y forward difference step (8.24)"
	GBUpdateDDX := 4.				"Delta DX forward difference step (8.24)"
	GBUpdateDDY := 5.				"Delta DY forward difference step (8.24)"
		"Note: The following four entries are only needed before the incremental
			state is computed. They are therefore aliased to the incremental values above"
	GBViaX := 12.						"via x"
	GBViaY := 13.						"via y"
	GBEndX := 14.						"end x"
	GBEndY := 15.						"end y"
	GBBaseSize := 16.					"Basic size of each bezier.
										Note: MUST be greater or equal to the size of lines"
	"Additional stuff for wide beziers"
	GBWideFill := 16.					"Current fill of line"
	GBWideWidth := 17.				"Current width of line"
	GBWideEntry := 18.				"Initial steps"
	GBWideExit := 19.					"Final steps"
	GBWideExtent := 20.				"Target extent"
	GBFinalX := 21.					"Final X value"
	GBWideUpdateData := 22.	"Update data for second curve"
	GBWideSize := 28.					"Size of wide beziers"

! !

!BalloonEngineConstants class methodsFor: 'pool definition' stamp: 'ar 5/18/2003 20:08'!
initFillConstants
	"Initialize the fill constants"

	"Fill primitive types"
	GEPrimitiveFill := 16r100.
	GEPrimitiveLinearGradientFill := 16r200.
	GEPrimitiveRadialGradientFill := 16r300.
	GEPrimitiveClippedBitmapFill := 16r400.
	GEPrimitiveRepeatedBitmapFill := 16r500.

	"General fill state constants"
	GEBaseFillSize := 4.				"Basic size of each fill"

	"Oriented fill constants"
	GFOriginX := 4.				"X origin of fill"
	GFOriginY := 5.				"Y origin of fill"
	GFDirectionX := 6.				"X direction of fill"
	GFDirectionY := 7.				"Y direction of fill"
	GFNormalX := 8.				"X normal of fill"
	GFNormalY := 9.				"Y normal of fill"

	"Gradient fill constants"
	GFRampLength := 10.			"Length of following color ramp"
	GFRampOffset := 12.			"Offset of first ramp entry"
	GGBaseSize := 12.

	"Bitmap fill constants"
	GBBitmapWidth := 10.			"Width of bitmap"
	GBBitmapHeight := 11.			"Height of bitmap"
	GBBitmapDepth := 12.			"Depth of bitmap"
	GBBitmapSize := 13.			"Size of bitmap words"
	GBBitmapRaster := 14.			"Size of raster line"
	GBColormapSize := 15.			"Size of colormap, if any"
	GBTileFlag := 16.				"True if the bitmap is tiled"
	GBColormapOffset := 18.		"Offset of colormap, if any"
	GBMBaseSize := 18.			"Basic size of bitmap fill"
! !

!BalloonEngineConstants class methodsFor: 'pool definition' stamp: 'eem 6/4/2009 18:07'!
initPrimitiveConstants
	"Initialize the primitive constants"

	"Primitive type constants"
	GEPrimitiveUnknown := 0.
	GEPrimitiveEdgeMask := 16rFF.
	GEPrimitiveFillMask := 16rFF00.
	GEPrimitiveTypeMask := 16rFFFF.

	"General state constants (Note: could be compressed later)"
	GEObjectType := 0.				"Type of object"
	GEObjectLength := 1.			"Length of object"
	GEObjectIndex := 2.			"Index into external objects"
	GEObjectUnused := 3.			"Currently unused"

	"Primitive failure codes"
	GEFAlreadyFailed := 100.

	GEFEngineIsInteger := 101.
	GEFEngineIsWords := 102.
	GEFEngineTooSmall := 103.
	GEFEngineStopped := 104.

	GEFWorkBufferIsInteger := 105.
	GEFWorkBufferIsPointers := 106.
	GEFWorkBufferTooSmall := 107.
	GEFWorkBufferBadMagic := 108.
	GEFWorkBufferWrongSize := 109.
	GEFWorkBufferStartWrong := 110.
	GEFWorkTooBig := 111.

	GEFEdgeDataTooSmall := 112.
	GEFFillDataTooSmall := 113.
	GEFClassMismatch := 114.
	GEFSizeMismatch := 115.
	GEFWrongState := 116.
	GEFWrongFill := 117.
	GEFWrongEdge := 118.

	GEFEntityLoadFailed := 119.
	GEFEntityCheckFailed := 120.
	GEFBadPoint := 121.
	GEFBitBltLoadFailed := 122.
	GEFFormLoadFailed := 123! !

!BalloonEngineConstants class methodsFor: 'pool definition' stamp: 'ar 5/18/2003 20:00'!
initStateConstants
	"Initialize the state Constants"

	GEStateUnlocked := 0.			"Buffer is unlocked and can be modified as wanted"
	GEStateAddingFromGET := 1.	"Adding edges from the GET"
	GEStateWaitingForEdge := 2.	"Waiting for edges added to GET"
	GEStateScanningAET := 3.		"Scanning the active edge table"
	GEStateWaitingForFill := 4.		"Waiting for a fill to mix in during AET scan"
	GEStateBlitBuffer := 5.			"Blt the current scan line"
	GEStateUpdateEdges := 6.		"Update edges to next scan line"
	GEStateWaitingChange := 7.	"Waiting for a changed edge"
	GEStateCompleted := 8.			"Rendering completed"

	"Error constants"
	GErrorNoMoreSpace := 1.		"No more space in collection"
	GErrorBadState := 2.			"Tried to call a primitive while engine in bad state"
	GErrorNeedFlush := 3.			"Tried to call a primitive that requires flushing before"

	"Incremental error constants"
	GErrorGETEntry := 4.			"Unknown entry in GET"
	GErrorFillEntry := 5.			"Unknown FILL encountered"
	GErrorAETEntry := 6.			"Unknown entry in AET"
! !

!BalloonEngineConstants class methodsFor: 'pool definition' stamp: 'ar 5/18/2003 20:04'!
initWorkBufferConstants
	"Initialize the work buffer constants"

	"General work buffer constants"
	GWMagicNumber := 16r416E6469.	"Magic number"
	GWHeaderSize := 128.				"Size of header"
	GWMinimalSize := 256.				"Minimal size of work buffer"

	"Header entries"
	GWMagicIndex := 0.				"Index of magic number"
	GWSize := 1.						"Size of full buffer"
	GWState := 2.						"Current state (e.g., locked or not."
	"Buffer entries"
	GWObjStart := 8.					"objStart"
	GWObjUsed := 9.					"objUsed"
	GWBufferTop := 10.				"wbTop"
	GWGETStart := 11.					"getStart"
	GWGETUsed := 12.					"getUsed"
	GWAETStart := 13.					"aetStart"
	GWAETUsed := 14.					"aetUsed"

	"Transform entries"
	GWHasEdgeTransform := 16.		"True if we have an edge transformation"
	GWHasColorTransform := 17.		"True if we have a color transformation"
	GWEdgeTransform := 18.			"2x3 edge transformation"
	GWColorTransform := 24.			"8 word RGBA color transformation"

	"Span entries"
	GWSpanStart := 32.				"spStart"
	GWSpanSize := 33.					"spSize"
	GWSpanEnd := 34.					"spEnd"
	GWSpanEndAA := 35.				"spEndAA"

	"Bounds entries"
	GWFillMinX := 36.					"fillMinX"
	GWFillMaxX := 37.					"fillMaxX"
	GWFillMinY := 38.					"fillMinY"
	GWFillMaxY := 39.					"fillMaxY"
	GWFillOffsetX := 40.				"fillOffsetX"
	GWFillOffsetY := 41.				"fillOffsetY"
	GWClipMinX := 42.
	GWClipMaxX := 43.
	GWClipMinY := 44.
	GWClipMaxY := 45.
	GWDestOffsetX := 46.
	GWDestOffsetY := 47.

	"AA entries"
	GWAALevel := 48.					"aaLevel"
	GWAAShift := 49.					"aaShift"
	GWAAColorShift := 50.				"aaColorShift"
	GWAAColorMask := 51.				"aaColorMask"
	GWAAScanMask := 52.				"aaScanMask"
	GWAAHalfPixel := 53.				"aaHalfPixel"

	"Misc entries"
	GWNeedsFlush := 63.				"True if the engine may need a flush"
	GWStopReason := 64.				"stopReason"
	GWLastExportedEdge := 65.			"last exported edge"
	GWLastExportedFill := 66.			"last exported fill"
	GWLastExportedLeftX := 67.			"last exported leftX"
	GWLastExportedRightX := 68.		"last exported rightX"
	GWClearSpanBuffer := 69.			"Do we have to clear the span buffer?"
	GWPointListFirst := 70.				"First point list in buffer"

	GWPoint1 := 80.
	GWPoint2 := 82.
	GWPoint3 := 84.
	GWPoint4 := 86.

	GWCurrentY := 88.

	"Profile stats"
	GWTimeInitializing := 90.
	GWCountInitializing := 91.
	GWTimeFinishTest := 92.
	GWCountFinishTest := 93.
	GWTimeNextGETEntry := 94.
	GWCountNextGETEntry := 95.
	GWTimeAddAETEntry := 96.
	GWCountAddAETEntry := 97.
	GWTimeNextFillEntry := 98.
	GWCountNextFillEntry := 99.
	GWTimeMergeFill := 100.
	GWCountMergeFill := 101.
	GWTimeDisplaySpan := 102.
	GWCountDisplaySpan := 103.
	GWTimeNextAETEntry := 104.
	GWCountNextAETEntry := 105.
	GWTimeChangeAETEntry := 106.
	GWCountChangeAETEntry := 107.

	"Bezier stats"
	GWBezierMonotonSubdivisions := 108. 	"# of subdivision due to non-monoton beziers"
	GWBezierHeightSubdivisions := 109.		"# of subdivisions due to excessive height"
	GWBezierOverflowSubdivisions := 110.	"# of subdivisions due to possible int overflow"
	GWBezierLineConversions := 111.		"# of beziers converted to lines"

	GWHasClipShapes := 112.		"True if the engine contains clip shapes"
	GWCurrentZ := 113.			"Current z value of primitives"
! !

!BalloonEngineConstants class methodsFor: 'pool definition' stamp: 'ar 5/18/2003 20:08'!
initialize
	"BalloonEngineConstants initialize"
	self initStateConstants.
	self initWorkBufferConstants.
	self initPrimitiveConstants.
	self initEdgeConstants.
	self initFillConstants.
	self initializeInstVarNames: BalloonEngine prefixedBy: 'BE'.
	self initializeInstVarNames: BalloonEdgeData prefixedBy: 'ET'.
	self initializeInstVarNames: BalloonFillData prefixedBy: 'FT'.! !

!BalloonEngineConstants class methodsFor: 'pool definition' stamp: 'nice 12/27/2009 03:11'!
initializeInstVarNames: aClass prefixedBy: aString

	| token |
	aClass instVarNames withIndexDo: [ :instVarName :index| | value |
		token := (aString, instVarName first asUppercase asString, (instVarName copyFrom: 2 to: instVarName size),'Index') asSymbol.
		value := index - 1.
		(self bindingOf: token) ifNil:[self addClassVarName: token].
		(self bindingOf: token) value: value.
	].
	token := (aString, aClass name,'Size') asSymbol.
	(self bindingOf: token) ifNil:[self addClassVarName: token].
	(self bindingOf: token) value: aClass instSize.! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 16:37'!
absoluteSquared8Dot24: value
	"Compute the squared value of a 8.24 number with 0.0 <= value < 1.0,
	e.g., compute (value * value) bitShift: -24"
	| halfWord1 halfWord2 result |
	(value >= 0 and:[value < 16r1000000]) ifFalse:[^self error:'Value out of range'].
	halfWord1 := value bitAnd: 16rFFFF.
	halfWord2 := (value bitShift: -16) bitAnd: 255.

	result := (halfWord1 * halfWord1) bitShift: -16. "We don't need the lower 16bits at all"
	result := result + ((halfWord1 * halfWord2) * 2).
	result := result + ((halfWord2 * halfWord2) bitShift: 16).
	"word1 := halfWord1 * halfWord1.
	word2 := (halfWord2 * halfWord1) + (word1 bitShift: -16).
	word1 := word1 bitAnd: 16rFFFF.
	word2 := word2 + (halfWord1 * halfWord2).
	word2 := word2 + ((halfWord2 * halfWord2) bitShift: 16)."

	^result bitShift: -8! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/27/1998 20:46'!
computeInitialStateFrom: source with: transformation
	"Compute the initial state in the receiver."
	start := (transformation localPointToGlobal: source start) asIntegerPoint.
	end := (transformation localPointToGlobal: source end) asIntegerPoint.
	via := (transformation localPointToGlobal: source via) asIntegerPoint.! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'jmv 5/14/2015 09:51'!
computeSplitAt: t
	"Split the receiver at the parametric value t"
	| left right newVia1 newVia2 newPoint |
	left := self copy.
	right := self copy.
	"Compute new intermediate points"
	newVia1 := (via - start) * t + start.
	newVia2 := (end - via) * t + via.
	"Compute new point on curve"
	newPoint := ((newVia1 - newVia2) * t + newVia2) asIntegerPoint.
	left via: newVia1 asIntegerPoint.
	left end: newPoint.
	right start: newPoint.
	right via: newVia2 asIntegerPoint.
	^Array with: left with: right! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 5/25/2000 17:57'!
debugDraw
	| entry minY maxY lX lY canvas |
	entry := BalloonEdgeData new.
	canvas := Display getCanvas.
	minY := (start y min: end y) min: via y.
	maxY := (start y max: end y) max: via y.
	entry yValue: minY.
	self stepToFirstScanLineAt: minY in: entry.
	lX := entry xValue.
	lY := entry yValue.
	minY+1 to: maxY do:[:y|
		self stepToNextScanLineAt: y in: entry.
		canvas line: lX@lY to: entry xValue @ y width: 2 color: Color black.
		lX := entry xValue.
		lY := y.
	].
! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 5/25/2000 17:57'!
debugDraw2
	| canvas last max t next |
	canvas := Display getCanvas.
	max := 100.
	last := nil.
	0 to: max do:[:i|
		t := i asFloat / max asFloat.
		next := self valueAt: t.
		last ifNotNil:[
			canvas line: last to: next rounded width: 2 color: Color blue.
		].
		last := next rounded.
	].! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 5/25/2000 17:57'!
debugDrawWide: n
	| entry minY maxY canvas curve p1 p2 entry2 y |
	curve := self class new.
	curve start: start + (0@n).
	curve via: via + (0@n).
	curve end: end + (0@n).
	entry := BalloonEdgeData new.
	entry2 := BalloonEdgeData new.
	canvas := Display getCanvas.
	minY := (start y min: end y) min: via y.
	maxY := (start y max: end y) max: via y.
	entry yValue: minY.
	entry2 yValue: minY + n.
	self stepToFirstScanLineAt: minY in: entry.
	curve stepToFirstScanLineAt: minY+n in: entry2.
	y := minY.
	1 to: n do:[:i|
		y := y + 1.
		self stepToNextScanLineAt: y in: entry.
		p1 := entry xValue @ y.
		canvas line: p1 to: p1 + (n@0) width: 1 color: Color black.
	].
	[y < maxY] whileTrue:[
		y := y + 1.
		self stepToNextScanLineAt: y in: entry.
		p2 := (entry xValue + n) @ y.
		curve stepToNextScanLineAt: y in: entry2.
		p1 := entry2 xValue @ y.
		canvas line: p1 to: p2 width: 1 color: Color black.
	].
! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
end
	^end! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
end: aPoint
	end := aPoint! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/30/1998 01:34'!
floatStepToFirstScanLineAt: yValue in: edgeTableEntry
	"Float version of forward differencing"
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	steps scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[
		^edgeTableEntry lines: 0].

	fwX1 := (startX + endX - (2 * via x)) asFloat.
	fwX2 := (via x - startX * 2) asFloat.
	fwY1 := (startY + endY - (2 * via y)) asFloat.
	fwY2 := ((via y - startY) * 2) asFloat.
	steps := deltaY asInteger * 2.
	scaledStepSize := 1.0 / steps asFloat.
	squaredStepSize := scaledStepSize * scaledStepSize.
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2.0 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2.0 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx * 0.5).
	fwDy := fwDy + (fwDDy * 0.5).

	lastX := startX asFloat.
	lastY := startY asFloat.

	"self xDirection: xDir.
	self yDirection: yDir."
	edgeTableEntry xValue: startX.
	edgeTableEntry yValue: startY.
	edgeTableEntry zValue: 0.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		self stepToNextScanLineAt: yValue in: edgeTableEntry.
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/30/1998 02:45'!
floatStepToNextScanLineAt: yValue in: edgeTableEntry
	"Float version of forward differencing"
	[yValue asFloat > lastY] whileTrue:[
		(fwDx < -50.0 or:[fwDx > 50.0]) ifTrue:[self halt].
		(fwDy < -50.0 or:[fwDy > 50.0]) ifTrue:[self halt].
		(fwDDx < -50.0 or:[fwDDx > 50.0]) ifTrue:[self halt].
		(fwDDy < -50.0 or:[fwDDy > 50.0]) ifTrue:[self halt].
		lastX := lastX + fwDx.
		lastY := lastY + fwDy.
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
	].
	edgeTableEntry xValue: lastX asInteger.
	edgeTableEntry zValue: 0.! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/30/1998 01:57'!
inTangent
	"Return the tangent at the start point"
	^via - start! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
initialX
	^start y <= end y
		ifTrue:[start x]
		ifFalse:[end x]! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
initialY
	^start y <= end y
		ifTrue:[start y]
		ifFalse:[end y]! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
initialZ
	^0 "Assume no depth given"! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/30/1998 16:23'!
intStepToFirstScanLineAt: yValue in: edgeTableEntry
	"Scaled integer version of forward differencing"
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	 scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[
		^edgeTableEntry lines: 0].

	fwX1 := (startX + endX - (2 * via x)).
	fwX2 := (via x - startX * 2).
	fwY1 := (startY + endY - (2 * via y)).
	fwY2 := ((via y - startY) * 2).
	maxSteps := deltaY asInteger * 2.
	scaledStepSize := 16r1000000 // maxSteps.
	"@@: Okay, we need some fancy 64bit multiplication here"
	squaredStepSize := self absoluteSquared8Dot24: scaledStepSize.
	squaredStepSize = ((scaledStepSize * scaledStepSize) bitShift: -24)
		ifFalse:[self error:'Bad computation'].
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx // 2).
	fwDy := fwDy + (fwDDy // 2).

	self validateIntegerRange.

	lastX := startX * 256.
	lastY := startY * 256.

	edgeTableEntry xValue: startX.
	edgeTableEntry yValue: startY.
	edgeTableEntry zValue: 0.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		self stepToNextScanLineAt: yValue in: edgeTableEntry.
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/30/1998 04:02'!
intStepToNextScanLineAt: yValue in: edgeTableEntry
	"Scaled integer version of forward differencing"
	[maxSteps >= 0 and:[yValue * 256 > lastY]] whileTrue:[
		self validateIntegerRange.
		lastX := lastX + ((fwDx + 16r8000) // 16r10000).
		lastY := lastY + ((fwDy + 16r8000) // 16r10000).
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
		maxSteps := maxSteps - 1.
	].
	edgeTableEntry xValue: lastX // 256.
	edgeTableEntry zValue: 0.! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/29/1998 22:14'!
isMonoton
	"Return true if the receiver is monoton along the y-axis,
	e.g., check if the tangents have the same sign"
	^(via y - start y) * (end y - via y) >= 0! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/30/1998 01:57'!
outTangent
	"Return the tangent at the end point"
	^end - via! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 00:35'!
printOn: aStream
	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: start;
		nextPutAll:' - ';
		print: via;
		nextPutAll:' - ';
		print: end;
		nextPut:$)! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 21:56'!
quickPrint: curve
	Transcript nextPut:$(;
		print: curve start;
		space;
		print: curve via;
		space;
		print: curve end;
		nextPut:$).! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 22:13'!
quickPrint: curve first: aBool
	aBool ifTrue:[Transcript cr].
	Transcript nextPut:$(;
		print: curve start;
		space;
		print: curve via;
		space;
		print: curve end;
		nextPut:$).
	Transcript endEntry.! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
start
	^start! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
start: aPoint
	start := aPoint! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 03:53'!
stepToFirst
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	steps scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(deltaY = 0) ifTrue:[^self].

	fwX1 := (startX + endX - (2 * via x)) asFloat.
	fwX2 := (via x - startX * 2) asFloat.
	fwY1 := (startY + endY - (2 * via y)) asFloat.
	fwY2 := ((via y - startY) * 2) asFloat.
	steps := deltaY asInteger * 2.
	scaledStepSize := 1.0 / steps asFloat.
	squaredStepSize := scaledStepSize * scaledStepSize.
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2.0 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2.0 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx * 0.5).
	fwDy := fwDy + (fwDDy * 0.5).

	lastX := startX asFloat.
	lastY := startY asFloat.
! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 03:50'!
stepToFirstInt
	"Scaled integer version of forward differencing"
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	 scaledStepSize squaredStepSize |
	self halt.
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(deltaY = 0) ifTrue:[^nil].

	fwX1 := (startX + endX - (2 * via x)).
	fwX2 := (via x - startX * 2).
	fwY1 := (startY + endY - (2 * via y)).
	fwY2 := ((via y - startY) * 2).
	maxSteps := deltaY asInteger * 2.
	scaledStepSize := 16r1000000 // maxSteps.
	"@@: Okay, we need some fancy 64bit multiplication here"
	squaredStepSize := (scaledStepSize * scaledStepSize) bitShift: -24.
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx // 2).
	fwDy := fwDy + (fwDDy // 2).

	self validateIntegerRange.

	lastX := startX * 256.
	lastY := startY * 256.
! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/31/1998 16:36'!
stepToFirstScanLineAt: yValue in: edgeTableEntry
	"Compute the initial x value for the scan line at yValue"
	^self intStepToFirstScanLineAt: yValue in: edgeTableEntry! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 00:26'!
stepToNext
		lastX := lastX + fwDx.
		lastY := lastY + fwDy.
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 04:01'!
stepToNextInt
	"Scaled integer version of forward differencing"
	self halt.
	(maxSteps >= 0) ifTrue:[
		self validateIntegerRange.
		lastX := lastX + ((fwDx + 16r8000) // 16r10000).
		lastY := lastY + ((fwDy + 16r8000) // 16r10000).
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
		maxSteps := maxSteps - 1.
	].! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/30/1998 03:40'!
stepToNextScanLineAt: yValue in: edgeTableEntry
	"Compute the next x value for the scan line at yValue.
	This message is sent during incremental updates. 
	The yValue parameter is passed in here for edges
	that have more complicated computations,"
	^self intStepToNextScanLineAt: yValue in: edgeTableEntry! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 11/1/1998 00:31'!
subdivide
	"Subdivide the receiver"
	| dy dx |
	"Test 1: If the bezier curve is not monoton in Y, we need a subdivision"
	self isMonoton ifFalse:[
		MonotonSubdivisions := MonotonSubdivisions + 1.
		^self subdivideToBeMonoton].

	"Test 2: If the receiver is horizontal, don't do anything"
	(end y = start y) ifTrue:[^nil].

	"Test 3: If the receiver can be represented as a straight line,
			make a line from the receiver and declare it invalid"
	((end - start) crossProduct: (via - start)) = 0 ifTrue:[
		LineConversions := LineConversions + 1.
		^self subdivideToBeLine].

	"Test 4: If the height of the curve exceeds 256 pixels, subdivide 
			(forward differencing is numerically not very stable)"
	dy := end y - start y.
	dy < 0 ifTrue:[dy := dy negated].
	(dy > 255) ifTrue:[
		HeightSubdivisions := HeightSubdivisions + 1.
		^self subdivideAt: 0.5].

	"Test 5: Check if the incremental values could possibly overflow the scaled integer range"
	dx := end x - start x.
	dx < 0 ifTrue:[dx := dx negated].
	dy * 32 < dx ifTrue:[
		OverflowSubdivisions := OverflowSubdivisions + 1.
		^self subdivideAt: 0.5].

	^nil! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/30/1998 22:13'!
subdivideAt: parameter
	"Subdivide the receiver at the given parameter"
	| both |
	(parameter <= 0.0 or:[parameter >= 1.0]) ifTrue:[self halt].
	both := self computeSplitAt: parameter.
	"Transcript cr.
	self quickPrint: self.
	Transcript space.
	self quickPrint: both first.
	Transcript space.
	self quickPrint: both last.
	Transcript endEntry."
	self via: both first via.
	self end: both first end.
	^both last! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 11/11/1998 22:15'!
subdivideToBeLine
	"Not a true subdivision.
	Just return a line representing the receiver and fake me to be of zero height"
	| line |
	line := BalloonLineSimulation new.
	line start: start.
	line end: end.
	"Make me invalid"
	end := start.
	via := start.
	 ^line! !

!BalloonBezierSimulation methodsFor: 'computing' stamp: 'ar 10/30/1998 02:24'!
subdivideToBeMonoton
	"Subdivide the receiver at it's extreme point"
	| v1 v2 t other |
	v1 := (via - start).
	v2 := (end - via).
	t := (v1 y / (v2 y - v1 y)) negated asFloat.
	other := self subdivideAt: t.
	self isMonoton ifFalse:[self halt].
	other isMonoton ifFalse:[self halt].
	^other! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/30/1998 03:27'!
validateIntegerRange
	fwDx class == SmallInteger ifFalse:[self halt].
	fwDy class == SmallInteger ifFalse:[self halt].
	fwDDx class == SmallInteger ifFalse:[self halt].
	fwDDy class == SmallInteger ifFalse:[self halt].
! !

!BalloonBezierSimulation methodsFor: 'private' stamp: 'ar 10/29/1998 21:26'!
valueAt: parameter
	"Return the point at the value parameter:
		p(t) =	(1-t)^2 * p1 + 
				2*t*(1-t) * p2 + 
				t^2 * p3.
	"
	| t1 t2 t3 |
	t1 := (1.0 - parameter) squared.
	t2 := 2 * parameter * (1.0 - parameter).
	t3 := parameter squared.
	^(start * t1) + (via * t2) + (end * t3)! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
via
	^via! !

!BalloonBezierSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:45'!
via: aPoint
	via := aPoint! !

!BalloonBezierSimulation class methodsFor: 'initialization' stamp: 'MarcusDenker 9/30/2009 11:56'!
initialize
	HeightSubdivisions := 0.
	LineConversions := 0.
	MonotonSubdivisions := 0.
	OverflowSubdivisions := 0.! !

!BalloonBuffer methodsFor: 'accessing' stamp: 'ar 10/26/1998 21:12'!
at: index
	"For simulation only"
	| word |
	word := self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]! !

!BalloonBuffer methodsFor: 'accessing' stamp: 'ar 10/26/1998 21:12'!
at: index put: anInteger
	"For simulation only"
	| word |
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger! !

!BalloonBuffer methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
floatAt: index
	"For simulation only"
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!BalloonBuffer methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
floatAt: index put: value
	"For simulation only"
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !

!BalloonBuffer class methodsFor: 'instance creation' stamp: 'ar 10/26/1998 21:11'!
new
	^self new: 256.! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:34'!
index
	^index! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:35'!
index: anInteger
	index := anInteger! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:13'!
lines
	^lines! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:13'!
lines: anInteger
	^lines := anInteger! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:34'!
source
	^source! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 21:39'!
source: anObject
	source := anObject! !

!BalloonEdgeData methodsFor: 'computing' stamp: 'ar 10/27/1998 15:53'!
stepToFirstScanLine
	source stepToFirstScanLineAt: yValue in: self! !

!BalloonEdgeData methodsFor: 'computing' stamp: 'ar 10/27/1998 15:53'!
stepToNextScanLine
	source stepToNextScanLineAt: yValue in: self! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:34'!
xValue
	^xValue! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:35'!
xValue: anInteger
	xValue := anInteger! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:34'!
yValue
	^yValue! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:35'!
yValue: anInteger
	yValue := anInteger! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 19:56'!
zValue
	^zValue! !

!BalloonEdgeData methodsFor: 'accessing' stamp: 'ar 10/27/1998 19:56'!
zValue: anInteger
	zValue := anInteger! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/11/1998 23:04'!
aaLevel
	^aaLevel ifNil:[1]! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/11/1998 23:04'!
aaLevel: anInteger
	aaLevel := (anInteger min: 4) max: 1.! !

!BalloonEngine methodsFor: 'accessing' stamp: 'jmv 5/8/2015 10:43'!
aaTransform
	"Return a transformation for the current anti-aliasing level"
	
	"
	| matrix |
	matrix := MatrixTransform2x3 withScale: (self aaLevel) asFloat asPoint.
	matrix offset: (self aaLevel // 2) asFloat asPoint.
	^matrix composedWith:(MatrixTransform2x3 withOffset: destOffset asFloatPoint)
	"! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 10/13/1998 03:04'!
bitBlt
	^bitBlt! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 7/7/2007 11:12'!
bitBlt: aBitBlt
	| destWidth |
	bitBlt := aBitBlt.
	bitBlt isNil ifTrue:[^self].
	destWidth := bitBlt destForm width.
	destWidth > span size ifTrue:[span := Bitmap new: destWidth].
	self class primitiveSetBitBltPlugin: bitBlt getPluginName.
	self clipRect: bitBlt clipRect.
	bitBlt 
		sourceForm: (Form extent: span size @ 1 depth: 32 bits: span);
		sourceRect: (0@0 extent: 1@span size);
		colorMap: (Color colorMapIfNeededFrom: 32 to: bitBlt destForm depth);
		combinationRule: (bitBlt destForm depth >= 8 ifTrue:[34] ifFalse:[Form paint]).! !

!BalloonEngine methodsFor: 'copying' stamp: 'ar 11/25/1998 00:45'!
canProceedAfter: failureReason
	"Check if we can proceed after the failureReason indicated."
	| newBuffer |
	failureReason = GErrorNeedFlush ifTrue:[
		"Need to flush engine before proceeding"
		self copyBits.
		self reset.
		^true].
	failureReason = GErrorNoMoreSpace ifTrue:[
		"Work buffer is too small"
		newBuffer := workBuffer species new: workBuffer size * 2.
		self primCopyBufferFrom: workBuffer to: newBuffer.
		workBuffer := newBuffer.
		^true].
	"Not handled"
	^false! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/1/1998 02:57'!
clipRect
	^clipRect! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 10/13/1998 02:44'!
clipRect: aRect
	clipRect := aRect truncated! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/24/1998 15:04'!
colorTransform
	^colorTransform! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/24/1998 15:04'!
colorTransform: aColorTransform
	colorTransform := aColorTransform! !

!BalloonEngine methodsFor: 'copying' stamp: 'ar 3/6/2001 12:06'!
copyBits
	(bitBlt notNil and:[bitBlt destForm notNil]) ifTrue:[bitBlt destForm unhibernate].
	self copyLoopFaster.! !

!BalloonEngine methodsFor: 'copying' stamp: 'ar 11/14/1998 19:32'!
copyLoop
	"This is the basic rendering loop using as little primitive support as possible."
	| finished edge fill |
	edge := BalloonEdgeData new.
	fill := BalloonFillData new.
	self primInitializeProcessing. "Initialize the GE for processing"
	[self primFinishedProcessing] whileFalse:[
		"Step 1: Process the edges in the global edge table that will be added in this step"
		[finished := self primNextGlobalEdgeEntryInto: edge.
		finished] whileFalse:[
			edge source: (externals at: edge index).
			edge stepToFirstScanLine.
			self primAddActiveEdgeTableEntryFrom: edge].

		"Step 2: Scan the active edge table"
		[finished := self primNextFillEntryInto: fill.
		finished] whileFalse:[
			fill source: (externals at: fill index).
			"Compute the new fill"
			fill computeFill.
			"And mix it in the out buffer"
			self primMergeFill: fill destForm bits from: fill].

		"Step 3: Display the current span buffer if necessary"
		self primDisplaySpanBuffer.

		"Step 4: Advance and resort the active edge table"
		[finished := self primNextActiveEdgeEntryInto: edge.
		finished] whileFalse:[
			"If the index is zero then the edge has been handled by the GE"
			edge source: (externals at: edge index).
			edge stepToNextScanLine.
			self primChangeActiveEdgeTableEntryFrom: edge].
	].
	self primGetTimes: Times.
	self primGetCounts: Counts.
	self primGetBezierStats: BezierStats.! !

!BalloonEngine methodsFor: 'copying' stamp: 'ar 11/14/1998 19:32'!
copyLoopFaster
	"This is a copy loop drawing one scan line at a time"
	| edge fill reason |
	edge := BalloonEdgeData new.
	fill := BalloonFillData new.
	[self primFinishedProcessing] whileFalse:[
		reason := self primRenderScanline: edge with: fill.
		"reason ~= 0 means there has been a problem"
		reason = 0 ifFalse:[
			self processStopReason: reason edge: edge fill: fill.
		].
	].
	self primGetTimes: Times.
	self primGetCounts: Counts.
	self primGetBezierStats: BezierStats.! !

!BalloonEngine methodsFor: 'copying' stamp: 'ar 11/14/1998 19:33'!
copyLoopFastest
	"This is a copy loop drawing the entire image"
	| edge fill reason |
	edge := BalloonEdgeData new.
	fill := BalloonFillData new.
	[self primFinishedProcessing] whileFalse:[
		reason := self primRenderImage: edge with: fill.
		"reason ~= 0 means there has been a problem"
		reason = 0 ifFalse:[
			self processStopReason: reason edge: edge fill: fill.
		].
	].
	self primGetTimes: Times.
	self primGetCounts: Counts.
	self primGetBezierStats: BezierStats.! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 12/30/1998 11:24'!
deferred
	^deferred! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 12/30/1998 11:24'!
deferred: aBoolean
	deferred := aBoolean.! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/1/1998 02:56'!
destOffset
	^destOffset! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/12/1998 00:22'!
destOffset: aPoint
	destOffset := aPoint asIntegerPoint.
	bitBlt destX: aPoint x; destY: aPoint y.! !

!BalloonEngine methodsFor: 'profiling' stamp: 'ar 11/11/1998 21:16'!
doAddCompressedShape: points segments: nSegments leftFills: leftFills rightFills: rightFills lineWidths: lineWidths lineFills: lineFills fillIndexList: fillIndexList matrix: aMatrix
	"Note: This method is for profiling the overhead of loading a compressed shape into the engine."
	^self primAddCompressedShape: points segments: nSegments leftFills: leftFills rightFills: rightFills lineWidths: lineWidths lineFills: lineFills fillIndexList: fillIndexList matrix: aMatrix! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 10/11/1999 16:49'!
drawBezierShape: points fill: fillStyle borderWidth: borderWidth borderColor: borderFill transform: aTransform
	| fills |
	self edgeTransform: aTransform.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderFill.
	self primAddBezierShape: points
		segments: (points size) // 3
		fill: (fills at: 1)
		lineWidth: borderWidth
		lineFill: (fills at: 2).
	self postFlushIfNeeded.! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 11/26/1998 19:44'!
drawCompressedShape: shape transform: aTransform
	| fillIndexList |
	self edgeTransform: aTransform.
	self resetIfNeeded.

	fillIndexList := self registerFills: shape fillStyles.

	self primAddCompressedShape: shape points
		segments: shape numSegments
		leftFills: shape leftFills
		rightFills: shape rightFills
		lineWidths: shape lineWidths
		lineFills: shape lineFills
		fillIndexList: fillIndexList.
	self postFlushIfNeeded.! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 1/15/1999 03:02'!
drawGeneralBezierShape: contours fill: fillStyle borderWidth: borderWidth borderColor: borderFill transform: aTransform

	| fills |
	self edgeTransform: aTransform.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderFill.
	contours do:[:points|
		self primAddBezierShape: points
			segments: (points size // 3)
			fill: (fills at: 1)
			lineWidth: borderWidth
			lineFill: (fills at: 2).
		"Note: To avoid premature flushing of the pipeline we need to
		reset the flush bit within the engine."
		self primFlushNeeded: false.
	].
	"And set the flush bit afterwards"
	self primFlushNeeded: true.
	self postFlushIfNeeded.! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 1/15/1999 03:02'!
drawGeneralPolygon: contours fill: fillStyle borderWidth: borderWidth borderColor: borderFill transform: aTransform

	| fills |
	self edgeTransform: aTransform.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderFill.
	contours do:[:points|
		self primAddPolygon: points
			segments: points size
			fill: (fills at: 1)
			lineWidth: borderWidth
			lineFill: (fills at: 2).
		"Note: To avoid premature flushing of the pipeline we need to
		reset the flush bit within the engine."
		self primFlushNeeded: false.
	].
	"And set the flush bit afterwards"
	self primFlushNeeded: true.
	self postFlushIfNeeded.! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 11/26/1998 19:45'!
drawOval: rect fill: fillStyle borderWidth: borderWidth borderColor: borderColor transform: aMatrix

	| fills |
	self edgeTransform: aMatrix.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderColor.
	self primAddOvalFrom: rect origin 
			to: rect corner
			fillIndex: (fills at: 1)
			borderWidth: borderWidth
			borderColor: (fills at: 2).
	self postFlushIfNeeded.! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 11/26/1998 19:45'!
drawPolygon: points fill: fillStyle borderWidth: borderWidth borderColor: borderFill transform: aTransform

	| fills |
	self edgeTransform: aTransform.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderFill.
	self primAddPolygon: points
		segments: points size
		fill: (fills at: 1)
		lineWidth: borderWidth
		lineFill: (fills at: 2).
	self postFlushIfNeeded.! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 11/26/1998 19:45'!
drawRectangle: rect fill: fillStyle borderWidth: borderWidth borderColor: borderColor transform: aMatrix

	| fills |
	self edgeTransform: aMatrix.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderColor.
	self primAddRectFrom: rect origin 
			to: rect corner
			fillIndex: (fills at: 1)
			borderWidth: borderWidth
			borderColor: (fills at: 2).
	self postFlushIfNeeded.! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/25/1998 22:34'!
edgeTransform
	^edgeTransform! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 11/25/1998 22:34'!
edgeTransform: aTransform
	edgeTransform := aTransform.! !

!BalloonEngine methodsFor: 'initialize' stamp: 'ar 11/25/1998 22:29'!
flush
	"Force all pending primitives onscreen"
	workBuffer ifNil:[^self].
	self copyBits.
	self release.! !

!BalloonEngine methodsFor: 'accessing' stamp: 'ar 10/29/1998 01:51'!
fullTransformFrom: aMatrix
	| m |
	m := self aaTransform composedWith: aMatrix.
	"m offset: m offset + destOffset."
	^m! !

!BalloonEngine methodsFor: 'initialize' stamp: 'ar 12/30/1998 11:24'!
initialize
	externals := OrderedCollection new: 100.
	span := Bitmap new: 2048.
	bitBlt := nil.
	self bitBlt: ((BitBlt toForm: Display) destRect: Display boundingBox; yourself).
	forms := #().
	deferred := false.! !

!BalloonEngine methodsFor: 'initialize' stamp: 'ar 11/25/1998 22:42'!
postFlushIfNeeded
	"Force all pending primitives onscreen"
	workBuffer ifNil:[^self].
	(deferred not or:[postFlushNeeded]) ifTrue:[
		self copyBits.
		self release].! !

!BalloonEngine methodsFor: 'initialize' stamp: 'ar 11/25/1998 22:43'!
preFlushIfNeeded
	"Force all pending primitives onscreen"
	workBuffer ifNil:[^self].
	self primFlushNeeded ifTrue:[
		self copyBits.
		self reset].! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:48'!
primAddActiveEdgeTableEntryFrom: edgeEntry
	"Add edge entry to the AET."
	<primitive: 'primitiveAddActiveEdgeEntry' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddActiveEdgeTableEntryFrom: edgeEntry
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddBezierFrom: start to: end via: via leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	<primitive: 'primitiveAddBezier' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddBezierFrom: start to: end via: via leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddBezierShape: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	<primitive: 'primitiveAddBezierShape' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddBezierShape: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddBitmapFill: form colormap: cmap tile: tileFlag from: origin along: direction normal: normal xIndex: xIndex
	<primitive: 'primitiveAddBitmapFill' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddBitmapFill: form colormap: cmap tile: tileFlag from: origin along: direction normal: normal xIndex: xIndex
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddCompressedShape: points segments: nSegments leftFills: leftFills rightFills: rightFills lineWidths: lineWidths lineFills: lineFills fillIndexList: fillIndexList
	<primitive: 'primitiveAddCompressedShape' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddCompressedShape: points segments: nSegments leftFills: leftFills rightFills: rightFills lineWidths: lineWidths lineFills: lineFills fillIndexList: fillIndexList
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddExternalEdge: index initialX: initialX initialY: initialY initialZ: initialZ leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	<primitive: 'primitiveRegisterExternalEdge' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddExternalEdge: index initialX: initialX initialY: initialY initialZ: initialZ leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddExternalFill: index
	<primitive: 'primitiveRegisterExternalFill' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddExternalFill: index
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddGradientFill: colorRamp from: origin along: direction normal: normal radial: isRadial
	<primitive: 'primitiveAddGradientFill' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddGradientFill: colorRamp 
				from: origin 
				along: direction 
				normal: normal 
				radial: isRadial
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddLineFrom: start to: end leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	<primitive: 'primitiveAddLine' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddLineFrom: start to: end leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddOvalFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	<primitive: 'primitiveAddOval' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddOvalFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddPolygon: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	<primitive: 'primitiveAddPolygon' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddPolygon: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-adding' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primAddRectFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	<primitive: 'primitiveAddRect' module: 'B2DPlugin' error: errorCode>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddRectFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	].
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:48'!
primChangeActiveEdgeTableEntryFrom: edgeEntry
	"Change the entry in the active edge table from edgeEntry"
	<primitive: 'primitiveChangedActiveEdgeEntry' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:48'!
primClipRectInto: rect
	<primitive: 'primitiveGetClipRect' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-misc' stamp: 'ar (auto errcode) 2/2/2001 15:48'!
primCopyBufferFrom: oldBuffer to: newBuffer
	"Copy the contents of oldBuffer into the (larger) newBuffer"
	<primitive: 'primitiveCopyBuffer' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:48'!
primDisplaySpanBuffer
	"Display the current scan line if necessary"
	<primitive: 'primitiveDisplaySpanBuffer' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primFinishedProcessing
	"Return true if there are no more entries in AET and GET and the last scan line has been displayed"
	<primitive: 'primitiveFinishedProcessing' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primFlushNeeded
	<primitive: 'primitiveNeedsFlush' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primFlushNeeded: aBoolean
	<primitive: 'primitiveNeedsFlushPut' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primGetAALevel
	"Set the AA level"
	<primitive: 'primitiveGetAALevel' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primGetBezierStats: statsArray
	<primitive: 'primitiveGetBezierStats' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primGetClipRect: rect
	<primitive: 'primitiveGetClipRect' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primGetCounts: statsArray
	<primitive: 'primitiveGetCounts' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primGetDepth
	<primitive: 'primitiveGetDepth' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primGetFailureReason
	<primitive: 'primitiveGetFailureReason' module: 'B2DPlugin' error: errorCode>
	^0! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primGetOffset
	<primitive: 'primitiveGetOffset' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primGetTimes: statsArray
	<primitive: 'primitiveGetTimes' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-misc' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primInitializeBuffer: buffer
	<primitive: 'primitiveInitializeBuffer' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primInitializeProcessing
	"Initialize processing in the GE.
	Create the active edge table and sort it."
	<primitive: 'primitiveInitializeProcessing' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primMergeFill: fillBitmap from: fill
	"Merge the filled bitmap into the current output buffer."
	<primitive: 'primitiveMergeFillFrom' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primNextActiveEdgeEntryInto: edgeEntry
	"Store the next entry of the AET at the current y-value in edgeEntry.
	Return false if there is no entry, true otherwise."
	<primitive: 'primitiveNextActiveEdgeEntry' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primNextFillEntryInto: fillEntry
	"Store the next fill entry of the active edge table in fillEntry.
	Return false if there is no such entry, true otherwise"
	<primitive: 'primitiveNextFillEntry' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primNextGlobalEdgeEntryInto: edgeEntry
	"Store the next entry of the GET at the current y-value in edgeEntry.
	Return false if there is no entry, true otherwise."
	<primitive: 'primitiveNextGlobalEdgeEntry' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primRenderImage: edge with: fill
	"Start/Proceed rendering the current scan line"
	<primitive: 'primitiveRenderImage' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-incremental' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primRenderScanline: edge with: fill
	"Start/Proceed rendering the current scan line"
	<primitive: 'primitiveRenderScanline' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primSetAALevel: level
	"Set the AA level"
	<primitive: 'primitiveSetAALevel' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primSetClipRect: rect
	<primitive: 'primitiveSetClipRect' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primSetColorTransform: transform
	<primitive: 'primitiveSetColorTransform' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primSetDepth: depth
	<primitive: 'primitiveSetDepth' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
primSetEdgeTransform: transform
	<primitive: 'primitiveSetEdgeTransform' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'primitives-access' stamp: 'ar (auto errcode) 2/2/2001 15:49'!
primSetOffset: point
	<primitive: 'primitiveSetOffset' module: 'B2DPlugin' error: errorCode>
	^self primitiveFailed! !

!BalloonEngine methodsFor: 'copying' stamp: 'ar 11/11/1998 21:19'!
processStopReason: reason edge: edge fill: fill
	"The engine has stopped because of some reason.
	Try to figure out how to respond and do the necessary actions."
	"Note: The order of operations below can affect the speed"

	"Process unknown fills first"
	reason = GErrorFillEntry ifTrue:[
		fill source: (externals at: fill index).
		"Compute the new fill"
		fill computeFill.
		"And mix it in the out buffer"
		^self primMergeFill: fill destForm bits from: fill].

	"Process unknown steppings in the AET second"
	reason = GErrorAETEntry ifTrue:[
		edge source: (externals at: edge index).
		edge stepToNextScanLine.
		^self primChangeActiveEdgeTableEntryFrom: edge].

	"Process unknown entries in the GET third"
	reason = GErrorGETEntry ifTrue:[
		edge source: (externals at: edge index).
		edge stepToFirstScanLine.
		^self primAddActiveEdgeTableEntryFrom: edge].

	"Process generic problems last"
	(self canProceedAfter: reason) ifTrue:[^self]. "Okay."

	^self error:'Unkown stop reason in graphics engine'
! !

!BalloonEngine methodsFor: 'experimental' stamp: 'ar 11/12/1998 19:53'!
registerBezier: aCurve transformation: aMatrix
	self primAddBezierFrom: aCurve start
		to: aCurve end
		via: aCurve via
		leftFillIndex: (self registerFill: aCurve leftFill transform: aMatrix)
		rightFillIndex: (self registerFill: aCurve rightFill transform: aMatrix)
		matrix: aMatrix! !

!BalloonEngine methodsFor: 'experimental' stamp: 'ar 11/11/1998 21:15'!
registerBoundary: boundaryObject transformation: aMatrix
	| external |
	external := boundaryObject asEdgeRepresentation: (self fullTransformFrom: aMatrix).
	self subdivideExternalEdge: external from: boundaryObject.
! !

!BalloonEngine methodsFor: 'experimental' stamp: 'ar 11/12/1998 19:54'!
registerExternalEdge: externalEdge from: boundaryObject
	externals addLast: externalEdge.
	self primAddExternalEdge: externals size
		initialX: externalEdge initialX
		initialY: externalEdge initialY
		initialZ: externalEdge initialZ
		leftFillIndex: (self registerFill: boundaryObject leftFill transform: nil)
		rightFillIndex: (self registerFill: boundaryObject rightFill transform: nil)! !

!BalloonEngine methodsFor: 'drawing' stamp: 'bf 4/3/2004 01:36'!
registerFill: aFillStyle
	"Register the given fill style."
	| theForm |
	aFillStyle ifNil:[^0].
	aFillStyle isSolidFill 
		ifTrue:[^aFillStyle scaledPixelValue32].

	aFillStyle isGradientFill ifTrue:[
		^self primAddGradientFill: aFillStyle pixelRamp
			from: aFillStyle origin
			along: aFillStyle direction
			normal: aFillStyle normal
			radial: aFillStyle isRadialFill
		].
	aFillStyle isBitmapFill ifTrue:[
		theForm := aFillStyle form asSourceForm.
		theForm unhibernate.
		forms := forms copyWith: theForm.
		^self primAddBitmapFill: theForm
				colormap: (theForm colormapIfNeededForDepth: 32)
				tile: aFillStyle isTiled
				from: aFillStyle origin
				along: aFillStyle direction
				normal: aFillStyle normal
				xIndex: forms size].
	^0! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 11/26/1998 19:45'!
registerFill: fill1 and: fill2
	^self registerFills: (Array with: fill1 with: fill2)! !

!BalloonEngine methodsFor: 'drawing' stamp: 'ar 1/14/1999 15:24'!
registerFill: aFillStyle transform: aTransform

	aFillStyle ifNil:[^0].
	aFillStyle isSolidFill 
		ifTrue:[^aFillStyle scaledPixelValue32].

	aFillStyle isGradientFill ifTrue:[
		^self primAddGradientFill: aFillStyle pixelRamp
			from: aFillStyle origin
			along: aFillStyle direction
			normal: aFillStyle normal
			radial: aFillStyle isRadialFill
			matrix: aTransform.
		].
	^0! !

!BalloonEngine methodsFor: 'drawing' stamp: 'di 11/21/1999 20:15'!
registerFills: fills

	| fillIndexList index fillIndex |
	((colorTransform notNil and:[colorTransform isAlphaTransform]) or:[
		fills anySatisfy: [:any| any notNil and:[any isTranslucent]]])
			ifTrue:[	self flush.
					self reset.
					postFlushNeeded := true].
	fillIndexList := WordArray new: fills size.
	index := 1.
	[index <= fills size] whileTrue:[
		fillIndex := self registerFill: (fills at: index).
		fillIndex == nil 
			ifTrue:[index := 1] "Need to start over"
			ifFalse:[fillIndexList at: index put: fillIndex.
					index := index+1]
	].
	^fillIndexList! !

!BalloonEngine methodsFor: 'experimental' stamp: 'ar 11/12/1998 19:54'!
registerLine: aLine transformation: aMatrix
	self primAddLineFrom: aLine start to: aLine end
		leftFillIndex: (self registerFill: aLine leftFill transform: aMatrix)
		rightFillIndex: (self registerFill: aLine rightFill transform: aMatrix)
		matrix: aMatrix! !

!BalloonEngine methodsFor: 'initialize' stamp: 'ar 11/11/1998 22:52'!
release
	self class recycleBuffer: workBuffer.
	workBuffer := nil.! !

!BalloonEngine methodsFor: 'initialize' stamp: 'ar 11/25/1998 22:34'!
reset
	workBuffer ifNil:[workBuffer := self class allocateOrRecycleBuffer: 10000].
	self primInitializeBuffer: workBuffer.
	self primSetAALevel: self aaLevel.
	self primSetOffset: destOffset.
	self primSetClipRect: clipRect.
	self primSetEdgeTransform: edgeTransform.
	self primSetColorTransform: colorTransform.
	forms := #().! !

!BalloonEngine methodsFor: 'initialize' stamp: 'ar 11/25/1998 22:39'!
resetIfNeeded
	workBuffer ifNil:[self reset].
	self primSetEdgeTransform: edgeTransform.
	self primSetColorTransform: colorTransform.
	self primSetDepth: self primGetDepth + 1.
	postFlushNeeded := false.! !

!BalloonEngine methodsFor: 'experimental' stamp: 'ar 11/11/1998 21:15'!
subdivideExternalEdge: external from: boundaryObject
	| external2 |
	external2 := external subdivide.
	external2 notNil ifTrue:[
		self subdivideExternalEdge: external from: boundaryObject.
		self subdivideExternalEdge: external2 from: boundaryObject.
	] ifFalse:[
		self registerExternalEdge: external from: boundaryObject.
	].! !

!BalloonEngine class methodsFor: 'private' stamp: 'ar 11/11/1998 22:50'!
allocateOrRecycleBuffer: initialSize
	"Try to recycly a buffer. If this is not possibly, create a new one."
	| buffer |
	CacheProtect critical:[
		buffer := BufferCache at: 1.
		BufferCache at: 1 put: nil.
	].
	^buffer ifNil:[BalloonBuffer new: initialSize]! !

!BalloonEngine class methodsFor: 'accessing' stamp: 'ar 10/25/1998 17:37'!
debug: aBoolean
	"BalloonEngine debug: true"
	"BalloonEngine debug: false"
	Debug := aBoolean! !

!BalloonEngine class methodsFor: 'accessing' stamp: 'ar (auto errcode) 2/2/2001 15:47'!
doProfileStats: aBool
	"Note: On Macintosh systems turning on profiling can significantly
	degrade the performance of Balloon since we're using the high
	accuracy timer for measuring."
	"BalloonEngine doProfileStats: true"
	"BalloonEngine doProfileStats: false"
	<primitive: 'primitiveDoProfileStats' module: 'B2DPlugin' error: errorCode>
	^false! !

!BalloonEngine class methodsFor: 'class initialization' stamp: 'ar 11/11/1998 22:49'!
initialize
	"BalloonEngine initialize"
	BufferCache := WeakArray new: 1.
	Smalltalk garbageCollect. "Make the cache old"
	CacheProtect := Semaphore forMutualExclusion.
	Times := WordArray new: 10.
	Counts := WordArray new: 10.
	BezierStats := WordArray new: 4.
	Debug ifNil:[Debug := false].! !

!BalloonEngine class methodsFor: 'private' stamp: 'ar (auto errcode) 5/28/2000 22:17'!
primitiveSetBitBltPlugin: pluginName
	<primitive: 'primitiveSetBitBltPlugin' module: 'B2DPlugin' error: errorCode>
	^nil! !

!BalloonEngine class methodsFor: 'accessing' stamp: 'ar 10/30/1998 23:57'!
printBezierStats
	"BalloonEngine printBezierStats"
	"BalloonEngine resetBezierStats"
	Transcript 
		cr; nextPutAll:'Bezier statistics:';
		crtab; print: (BezierStats at: 1); tab; nextPutAll:' non-monoton curves splitted';
		crtab; print: (BezierStats at: 2); tab; nextPutAll:' curves splitted for numerical accuracy';
		crtab; print: (BezierStats at: 3); tab; nextPutAll:' curves splitted to avoid integer overflow';
		crtab; print: (BezierStats at: 4); tab; nextPutAll:' curves internally converted to lines';
	endEntry.! !

!BalloonEngine class methodsFor: 'accessing' stamp: 'nice 12/25/2013 22:13'!
printStat: time count: n string: aString
	Transcript
		cr;
		print: time; tab;
		nextPutAll:' mSecs -- ';
		print: n; tab;
		nextPutAll:' ops -- ';
		nextPutAll: ((time asFloat / (n max: 1) asFloat) printShowingMaxDecimalPlaces: 2); tab;
		nextPutAll: ' avg. mSecs/op -- ';
		nextPutAll: aString.! !

!BalloonEngine class methodsFor: 'accessing' stamp: 'ar 1/12/1999 10:52'!
printStats
	"BalloonEngine doProfileStats: true"
	"BalloonEngine printStats"
	"BalloonEngine resetStats"
	Transcript cr; nextPutAll:'/************** BalloonEngine statistics ****************/'.
	self printStat: (Times at: 1) count: (Counts at: 1) string: 'Initialization'.
	self printStat: (Times at: 2) count: (Counts at: 2) string: 'Finish test'.
	self printStat: (Times at: 3) count: (Counts at: 3) string: 'Fetching/Adding GET entries'.
	self printStat: (Times at: 4) count: (Counts at: 4) string: 'Adding AET entries'.
	self printStat: (Times at: 5) count: (Counts at: 5) string: 'Fetching/Computing fills'.
	self printStat: (Times at: 6) count: (Counts at: 6) string: 'Merging fills'.
	self printStat: (Times at: 7) count: (Counts at: 7) string: 'Displaying span buffer'.
	self printStat: (Times at: 8) count: (Counts at: 8) string: 'Fetching/Updating AET entries'.
	self printStat: (Times at: 9) count: (Counts at: 9) string: 'Changing AET entries'.
	Transcript cr; print: Times sum; nextPutAll:' mSecs for all operations'.
	Transcript cr; print: Counts sum; nextPutAll: ' overall operations'.
	Transcript endEntry.! !

!BalloonEngine class methodsFor: 'private' stamp: 'eem 6/11/2008 13:00'!
recycleBuffer: balloonBuffer
	"Try to keep the buffer for later drawing operations."

	CacheProtect critical:[ | buffer |
		buffer := BufferCache at: 1.
		(buffer isNil or:[buffer size < balloonBuffer size] )
			ifTrue:[BufferCache at: 1 put: balloonBuffer].
	].! !

!BalloonEngine class methodsFor: 'accessing' stamp: 'ar 10/30/1998 23:57'!
resetBezierStats
	BezierStats := WordArray new: 4.! !

!BalloonEngine class methodsFor: 'accessing' stamp: 'ar 10/28/1998 23:38'!
resetStats
	Times := WordArray new: 10.
	Counts := WordArray new: 10.! !

!BalloonFillData methodsFor: 'computing' stamp: 'ar 11/14/1998 19:32'!
computeFill
	(destForm isNil or:[destForm width < self width]) ifTrue:[
		destForm := Form extent: (self width + 10) @ 1 depth: 32.
	].
	source computeFillFrom: minX to: maxX at: yValue in: destForm! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:56'!
destForm
	^destForm! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:56'!
destForm: aForm
	destForm := aForm! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
index
	^index! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
index: anInteger
	index := anInteger! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
maxX
	^maxX! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
maxX: anInteger
	maxX := anInteger! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
minX
	^minX! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
minX: anInteger
	minX := anInteger! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:56'!
source
	^source! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:56'!
source: anObject
	source := anObject! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/28/1998 16:35'!
width
	^maxX - minX! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
yValue
	^yValue! !

!BalloonFillData methodsFor: 'accessing' stamp: 'ar 10/27/1998 15:55'!
yValue: anInteger
	yValue := anInteger! !

!BalloonLineSimulation methodsFor: 'computing' stamp: 'ar 10/27/1998 20:52'!
computeInitialStateFrom: source with: aTransformation
	"Compute the initial state in the receiver."
	start := (aTransformation localPointToGlobal: source start) asIntegerPoint.
	end := (aTransformation localPointToGlobal: source end) asIntegerPoint.! !

!BalloonLineSimulation methodsFor: 'accessing' stamp: 'ar 10/30/1998 03:02'!
end
	^end! !

!BalloonLineSimulation methodsFor: 'accessing' stamp: 'ar 10/30/1998 03:02'!
end: aPoint
	end := aPoint! !

!BalloonLineSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:31'!
initialX
	^start y <= end y
		ifTrue:[start x]
		ifFalse:[end x]! !

!BalloonLineSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:31'!
initialY
	^start y <= end y
		ifTrue:[start y]
		ifFalse:[end y]! !

!BalloonLineSimulation methodsFor: 'accessing' stamp: 'ar 10/27/1998 20:31'!
initialZ
	^0 "Assume no depth given"! !

!BalloonLineSimulation methodsFor: 'printing' stamp: 'ar 10/27/1998 23:20'!
printOn: aStream
	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: start;
		nextPutAll:' - ';
		print: end;
		nextPut:$)! !

!BalloonLineSimulation methodsFor: 'accessing' stamp: 'ar 10/30/1998 03:02'!
start
	^start! !

!BalloonLineSimulation methodsFor: 'accessing' stamp: 'ar 10/30/1998 03:02'!
start: aPoint
	start := aPoint! !

!BalloonLineSimulation methodsFor: 'computing' stamp: 'ar 10/27/1998 23:22'!
stepToFirstScanLineAt: yValue in: edgeTableEntry
	"Compute the initial x value for the scan line at yValue"
	|  startX endX startY endY yDir deltaY deltaX widthX |
	(start y) <= (end y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
		yDir := 1.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
		yDir := -1.
	].

	deltaY := endY - startY.
	deltaX := endX - startX.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[^edgeTableEntry lines: 0].

	"Check if edge goes left to right"
	deltaX >= 0 ifTrue:[
		xDirection := 1.
		widthX := deltaX.
		error := 0.
	] ifFalse:[
		xDirection := -1.
		widthX := 0 - deltaX.
		error := 1 - deltaY.
	].

	"Check if edge is horizontal"
	deltaY = 0 
		ifTrue:[	xIncrement := 0.
				errorAdjUp := 0]
		ifFalse:["Check if edge is y-major"
			deltaY > widthX 
				ifTrue:[	xIncrement := 0.
						errorAdjUp := widthX]
				ifFalse:[	xIncrement := (widthX // deltaY) * xDirection.
						errorAdjUp := widthX \\ deltaY]].

	errorAdjDown := deltaY.

	edgeTableEntry xValue: startX.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		startY to: yValue do:[:y| self stepToNextScanLineAt: y in: edgeTableEntry].
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].! !

!BalloonLineSimulation methodsFor: 'computing' stamp: 'ar 10/27/1998 20:39'!
stepToNextScanLineAt: yValue in: edgeTableEntry
	"Compute the next x value for the scan line at yValue.
	This message is sent during incremental updates. 
	The yValue parameter is passed in here for edges
	that have more complicated computations,"
	| x |
	x := edgeTableEntry xValue + xIncrement.
	error := error + errorAdjUp.
	error > 0 ifTrue:[
		x := x + xDirection.
		error := error - errorAdjDown.
	].
	edgeTableEntry xValue: x.! !

!BalloonLineSimulation methodsFor: 'computing' stamp: 'ar 10/29/1998 23:42'!
subdivide
	^nil! !

!BalloonSolidFillSimulation methodsFor: 'computing' stamp: 'ar 10/27/1998 23:07'!
computeFillFrom: minX to: maxX at: yValue in: form
	| bb |
	color isTransparent ifFalse:[
		bb := BitBlt toForm: form.
		bb fillColor: color.
		bb destX: 0 destY: 0 width: (maxX - minX) height: 1.
		bb combinationRule: Form over.
		bb copyBits].! !

!BalloonSolidFillSimulation methodsFor: 'computing' stamp: 'ar 10/27/1998 23:08'!
computeInitialStateFrom: source with: aColorTransform
	color := source asColor.! !

!BalloonState methodsFor: 'accessing' stamp: 'ar 12/30/1998 10:47'!
aaLevel
	^aaLevel! !

!BalloonState methodsFor: 'accessing' stamp: 'ar 12/30/1998 10:47'!
aaLevel: aNumber
	aaLevel := aNumber! !

!BalloonState methodsFor: 'accessing' stamp: 'ar 12/30/1998 10:42'!
colorTransform
	^colorTransform! !

!BalloonState methodsFor: 'accessing' stamp: 'ar 12/30/1998 10:42'!
colorTransform: aColorTransform
	colorTransform := aColorTransform! !

!BalloonState methodsFor: 'accessing' stamp: 'ar 12/30/1998 10:41'!
transform
	^transform! !

!BalloonState methodsFor: 'accessing' stamp: 'ar 12/30/1998 10:42'!
transform: aMatrixTransform
	transform := aMatrixTransform! !
BalloonEngineConstants initialize!
BalloonBezierSimulation initialize!
BalloonEngine initialize!
