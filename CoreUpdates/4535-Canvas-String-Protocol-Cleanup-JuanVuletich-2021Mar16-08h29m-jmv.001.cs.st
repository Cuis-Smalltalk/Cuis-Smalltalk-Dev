'From Cuis 5.0 [latest update: #4534] on 16 March 2021 at 9:05:21 am'!

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:47:32'!
drawString: s at: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:47:48'!
drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^aBoolean
		ifTrue: [ self drawStringEmbossed: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor ]
		ifFalse: [ self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor ]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:40:05'!
drawString: s atBaseline: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atBaseline: pt font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:49:16'!
drawString: s atCenterX: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		at: pt - ((font widthOfString: s) / 2 @ 0)
		font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:42:31'!
drawString: s atWaist: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atWaist: pt font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:43:49'!
drawString: s atWaistRight: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		atWaist: pt - ((font widthOfString: s) @ 0)
		font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:57:07'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
	"Eventually, convert all senders to the 'Baseline' protocol"

	^self drawString: aString from: firstIndex to: lastIndex
		atBaseline: aPoint + (0@(font ascent + font lineGap-1))
		font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:34:31'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:59:54'!
drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: font color: aColor

	^self drawString: aString from: firstIndex to: lastIndex
		atBaseline: aPoint + (0@(font ascent / 3))
		font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:50:14'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	aColor = `Color black` ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: `Color black`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: font
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: `Color white`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + `0@2`
			font: font
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + `0@1`
		font: font
		color: aColor! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:56:16'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 |
	engine ifNil: [ ^nil ].

	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ currentTransformation transform: aPoint rounded.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 rounded.
	engine colorMap: nil.
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:50:44'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"
	| p1 |

	engine ifNil: [ ^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.

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

!methodRemoval: BitBltCanvas #drawString:from:to:atWaist:font:color: stamp: 'jmv 3/16/2021 09:04:41'!
BitBltCanvas removeSelector: #drawString:from:to:atWaist:font:color:!
!methodRemoval: BitBltCanvas #drawString:from:to:at:font:color: stamp: 'jmv 3/16/2021 08:38:20'!
BitBltCanvas removeSelector: #drawString:from:to:at:font:color:!
!methodRemoval: BitBltCanvas #drawString:from:to:atTop:font:color: stamp: 'jmv 3/16/2021 08:56:26'!
BitBltCanvas removeSelector: #drawString:from:to:atTop:font:color:!
!methodRemoval: MorphicCanvas #drawString:from:to:atTop:font:color: stamp: 'jmv 3/16/2021 08:57:09'!
MorphicCanvas removeSelector: #drawString:from:to:atTop:font:color:!
