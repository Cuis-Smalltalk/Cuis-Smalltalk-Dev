'From Cuis7.7 [latest update: #7864] on 8 April 2026 at 3:06:38 pm'!

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 8/Apr/2026 15:01:34'!
pvtFillColor: aFillColorOrNil
	self subclassResponsibility! !

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 8/Apr/2026 15:01:22'!
pvtStrokeColor: aStrokeColorOrNil
	self subclassResponsibility! !


!VectorEngineSmalltalk methodsFor: 'primitive algorithms - aux' stamp: 'jmv 8/Apr/2026 15:02:15'!
updateAlphasForX: x y: y
	self subclassResponsibility! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms - aux' stamp: 'jmv 8/Apr/2026 15:02:36'!
updateEdgeCountAtX: x y: y
	self subclassResponsibility! !


!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:05:17'!
primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:03:12'!
primBlendFillOnlyLeft: l top: t right: r bottom: b
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:04:19'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:04:31'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:04:45'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:03:23'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:04:56'!
primPathSequence: aFloat32Array size: s
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:03:41'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl
	self subclassResponsibility! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/Apr/2026 15:03:55'!
primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray affectedBits: aByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	self subclassResponsibility! !

