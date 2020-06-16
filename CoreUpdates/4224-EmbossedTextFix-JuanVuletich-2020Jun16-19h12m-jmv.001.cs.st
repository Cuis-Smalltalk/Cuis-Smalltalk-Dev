'From Cuis 5.0 [latest update: #4223] on 16 June 2020 at 7:25:31 pm'!

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/16/2020 19:24:29'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"
	| p1 font |
	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ fontOrNil ifNil: [ FontFamily defaultFamilyAndPointSize ].

	aColor = `Color black` ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: `Color black`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: `Color white`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1 + `0@2`
			color: bottomColor ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1 + `0@1`
		color: aColor.! !

