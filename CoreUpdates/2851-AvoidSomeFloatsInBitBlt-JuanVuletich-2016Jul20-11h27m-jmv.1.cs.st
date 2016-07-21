'From Cuis 4.2 of 25 July 2013 [latest update: #2850] on 20 July 2016 at 11:30:39 am'!

!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 7/20/2016 11:29:59'!
morphContainsPoint: aLocalPoint

	| iconOrigin |
	((0@0 extent: extent) containsPoint: aLocalPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ extent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: (aLocalPoint - iconOrigin) rounded) not ]]! !

