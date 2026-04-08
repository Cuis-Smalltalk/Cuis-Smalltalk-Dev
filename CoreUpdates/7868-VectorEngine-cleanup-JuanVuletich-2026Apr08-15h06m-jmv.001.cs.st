'From Cuis7.7 [latest update: #7864] on 8 April 2026 at 3:12:07 pm'!

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 8/Apr/2026 15:11:27'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	self subclassResponsibility! !

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 8/Apr/2026 15:11:19'!
strokeWidth: clientStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: clientLengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock
	"Prepare parameters for drawing dashed stroke."

	self subclassResponsibility! !

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 8/Apr/2026 15:10:45'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self subclassResponsibility! !

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 8/Apr/2026 15:10:36'!
strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	self subclassResponsibility! !


!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 8/Apr/2026 15:11:14'!
strokeWidth: clientStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: clientLengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock
	"Prepare parameters for drawing dashed stroke."

	^self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock! !


!VectorEngine reorganize!
('accessing' antiAliasingWidth antiAliasingWidth: geometryTransformation: morphBoundsAfterDraw)
('stroke & fill' fillColor:do: strokeWidth:color:dashedStrokeBits:dashBitLength:dashBitOffset:do: strokeWidth:color:do: strokeWidth:color:fillColor:do:)
('paths' arcCenter:radius:rotationAngle:start:sweep: arcTo:radius:angleOfXAxis:largeFlag:sweepFlag: circleCenter:radius: cubicBezierTo:control1:control2: ellipseCenter:radius: ellipseCenter:radius:rotationAngle: ellipseCenter:radius:rotationAngle:start:sweep: lineTo: lineToFirstPoint moveTo: quadraticBezierTo:control:)
('text and multi paths' drawByteString:from:to:at:trueTypeFont:color:scale:preventMirroring: drawUtf32String:from:to:at:trueTypeFont:color:scale:preventMirroring: drawUtf8String:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring: drawUtf8String:fromByte:toByte:at:trueTypeFont:color:strokeWidth:strokeColor:scale:preventMirroring: pathSequence:)
('morphic' canvas:)
('private' contourDataIndexForUtf32:contourDataIndexes: displayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes: displayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes: displayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes: doPath:ensureClosePath: glyphContours:atx:y:scalex:y:contourData:i: initializePath newTrajectoryFragment pvtStrokeWidth:hop: updateMorphBoundsLeft:top:right:bottom:)
!

