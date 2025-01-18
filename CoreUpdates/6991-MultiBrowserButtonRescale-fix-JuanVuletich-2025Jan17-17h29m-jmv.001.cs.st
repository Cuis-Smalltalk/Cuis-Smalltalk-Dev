'From Cuis7.3 [latest update: #6989] on 17 January 2025 at 5:29:31 pm'!

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 1/17/2025 17:25:55'!
windowSquareButtonPathData: aSize
	| scale |
	(IconPathData at: #windowSquareButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowSquareButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowSquareButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowSquareButton put: (
		self scalePathData: `#[
			0.0 0 0
			1    0 20
			1    20 20
			1    20 0
			1    0 0
		] asFloat32Array` scale: scale)! !

