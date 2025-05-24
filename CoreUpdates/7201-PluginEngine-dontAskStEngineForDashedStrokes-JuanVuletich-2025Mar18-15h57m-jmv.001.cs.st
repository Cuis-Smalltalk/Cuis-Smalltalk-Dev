'From Cuis7.3 [latest update: #7086] on 18 March 2025 at 4:21:44 pm'!

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 3/18/2025 16:14:34'!
doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Evaluate a sequence of path commands. Path parameters already set.
	VectorEnginePlugin is not reentrant.
	- Any other process must wait.
	- Nested calls in the same process are not supported, and ignored."

	self class protected: [
		super doPath: pathCommandsBlock ensureClosePath: ensureClosePath ].! !

!methodRemoval: VectorEngineWithPluginWholePixel #dashedStroke_doPath:ensureClosePath: stamp: 'jmv 3/18/2025 16:15:39'!
VectorEngineWithPluginWholePixel removeSelector: #dashedStroke_doPath:ensureClosePath:!
!methodRemoval: VectorEngineWithPluginSubPixel #dashedStroke_doPath:ensureClosePath: stamp: 'jmv 3/18/2025 16:15:40'!
VectorEngineWithPluginSubPixel removeSelector: #dashedStroke_doPath:ensureClosePath:!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_clipBottom stamp: 'jmv 3/18/2025 16:17:52'!
VectorEngineWithPlugin removeSelector: #dashedStroke_clipBottom!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_contour stamp: 'jmv 3/18/2025 16:17:54'!
VectorEngineWithPlugin removeSelector: #dashedStroke_contour!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_edgeCounts stamp: 'jmv 3/18/2025 16:17:54'!
VectorEngineWithPlugin removeSelector: #dashedStroke_edgeCounts!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_clipLeft stamp: 'jmv 3/18/2025 16:17:53'!
VectorEngineWithPlugin removeSelector: #dashedStroke_clipLeft!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_finishPath:smalltalkVGE: stamp: 'jmv 3/18/2025 16:16:37'!
VectorEngineWithPlugin removeSelector: #dashedStroke_finishPath:smalltalkVGE:!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_geometryTransformation stamp: 'jmv 3/18/2025 16:17:55'!
VectorEngineWithPlugin removeSelector: #dashedStroke_geometryTransformation!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_strokeDashArrayLast stamp: 'jmv 3/18/2025 16:17:57'!
VectorEngineWithPlugin removeSelector: #dashedStroke_strokeDashArrayLast!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_strokeDashArray stamp: 'jmv 3/18/2025 16:17:56'!
VectorEngineWithPlugin removeSelector: #dashedStroke_strokeDashArray!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_strokeDashLength stamp: 'jmv 3/18/2025 16:17:58'!
VectorEngineWithPlugin removeSelector: #dashedStroke_strokeDashLength!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_targetForm stamp: 'jmv 3/18/2025 16:17:59'!
VectorEngineWithPlugin removeSelector: #dashedStroke_targetForm!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_clipRight stamp: 'jmv 3/18/2025 16:17:53'!
VectorEngineWithPlugin removeSelector: #dashedStroke_clipRight!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_morphIds stamp: 'jmv 3/18/2025 16:17:55'!
VectorEngineWithPlugin removeSelector: #dashedStroke_morphIds!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_strokeDashArrayOffset stamp: 'jmv 3/18/2025 16:17:57'!
VectorEngineWithPlugin removeSelector: #dashedStroke_strokeDashArrayOffset!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_strokeWidth stamp: 'jmv 3/18/2025 16:17:58'!
VectorEngineWithPlugin removeSelector: #dashedStroke_strokeWidth!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_alphaMask stamp: 'jmv 3/18/2025 16:17:51'!
VectorEngineWithPlugin removeSelector: #dashedStroke_alphaMask!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_clipTop stamp: 'jmv 3/18/2025 16:17:54'!
VectorEngineWithPlugin removeSelector: #dashedStroke_clipTop!
!methodRemoval: VectorEngineWithPlugin #dashedStroke_strokeColor stamp: 'jmv 3/18/2025 16:17:56'!
VectorEngineWithPlugin removeSelector: #dashedStroke_strokeColor!
!methodRemoval: VectorEngineWholePixel #dashedStroke_ToFollow: stamp: 'jmv 3/18/2025 16:17:05'!
VectorEngineWholePixel removeSelector: #dashedStroke_ToFollow:!
!methodRemoval: VectorEngineSubPixel #dashedStroke_ToFollow: stamp: 'jmv 3/18/2025 16:17:04'!
VectorEngineSubPixel removeSelector: #dashedStroke_ToFollow:!
!methodRemoval: VectorEngineSmalltalk #dashedStroke_spanBottom stamp: 'jmv 3/18/2025 16:17:23'!
VectorEngineSmalltalk removeSelector: #dashedStroke_spanBottom!
!methodRemoval: VectorEngineSmalltalk #dashedStroke_spanTop stamp: 'jmv 3/18/2025 16:17:25'!
VectorEngineSmalltalk removeSelector: #dashedStroke_spanTop!
!methodRemoval: VectorEngineSmalltalk #dashedStroke_finishPath: stamp: 'jmv 3/18/2025 16:17:21'!
VectorEngineSmalltalk removeSelector: #dashedStroke_finishPath:!
!methodRemoval: VectorEngineSmalltalk #dashedStroke_spanLeft stamp: 'jmv 3/18/2025 16:17:24'!
VectorEngineSmalltalk removeSelector: #dashedStroke_spanLeft!
!methodRemoval: VectorEngineSmalltalk #dashedStroke_spanRight stamp: 'jmv 3/18/2025 16:17:25'!
VectorEngineSmalltalk removeSelector: #dashedStroke_spanRight!

!VectorEngineSmalltalk reorganize!
('accessing' antiAliasingWidth:)
('primitive algorithms' arcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin: cubicBezierFromX:y:toX:y:control1X:y:control2X:y: lineFromX:y:toX:y: quadraticBezierFromX:y:toX:y:controlX:y:)
('primitive algorithms - aux' updateContourForX:y:)
('private' finishPath: initializePath newTrajectoryFragment pvtFillColor: pvtStrokeColor: pvtStrokeWidth: resetMorphBounds updateContourLastLine)
('private - blend with mask' blendFillOnly blendStrokeAndFill blendStrokeOnly)
!


!VectorEngineWithPlugin reorganize!
('path algorithms' arcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin: cubicBezierFromX:y:toX:y:control1X:y:control2X:y: lineFromX:y:toX:y: quadraticBezierFromX:y:toX:y:controlX:y:)
('text and multi paths' displayByteString:from:to:at:trueTypeFont:color:scale:preventMirroring: displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring: displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring: pathSequence:)
('private' doPath:ensureClosePath: finishPath: initializePath newTrajectoryFragment pvtFillColor: pvtStrokeColor: pvtStrokeWidth: resetMorphBounds)
('plugin calls' primAntiAliasingWidth:subPixelDelta:hopLength: primClipCurrentMorph:morphIds: primClipLeft:clipTop:clipRight:clipBottom: primCurrentMorphId:currentClipsSubmorphs: primFillColor: primFillR:g:b:a: primGeometryTxA11:a12:a13:a21:a22:a23: primInitializePath primNewTrajectoryFragment primResetContourTop:bottom:contour: primSpanBottom primSpanLeft primSpanRight primSpanTop primStrokeColor: primStrokeR:g:b:a: primStrokeWidth: primUpdateContourLastLine:)
('testing' usesVectorEnginePlugin)
!

