'From Cuis7.7 [latest update: #7868] on 8 April 2026 at 3:31:55 pm'!

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 8/Apr/2026 15:22:58'!
currentMorph: aMorph
	self subclassResponsibility! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 8/Apr/2026 15:19:24'!
resetMorphBounds
	self subclassResponsibility! !

!VectorEngine methodsFor: 'primitive algorithms' stamp: 'jmv 8/Apr/2026 15:21:27'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin
	self subclassResponsibility ! !

!VectorEngine methodsFor: 'primitive algorithms' stamp: 'jmv 8/Apr/2026 15:21:14'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2
	self subclassResponsibility ! !

!VectorEngine methodsFor: 'primitive algorithms' stamp: 'jmv 8/Apr/2026 15:22:07'!
finishPath: ensureClosePath
	self subclassResponsibility ! !

!VectorEngine methodsFor: 'primitive algorithms' stamp: 'jmv 8/Apr/2026 15:21:44'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo
	self subclassResponsibility ! !

!VectorEngine methodsFor: 'primitive algorithms' stamp: 'jmv 8/Apr/2026 15:20:48'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl
	self subclassResponsibility ! !

