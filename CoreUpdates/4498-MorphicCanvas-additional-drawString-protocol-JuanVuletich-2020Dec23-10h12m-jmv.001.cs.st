'From Cuis 5.0 [latest update: #4494] on 23 December 2020 at 10:14:31 am'!

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:47:59'!
drawString: s atBaseline: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size atBaseline: pt font: fontOrNil color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:58:56'!
drawString: s atCenterX: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^font
		ifNil: [ pt ] 
		ifNotNil: [
			self
				drawString: s
				from: 1 to: s size
				at: pt - ((font widthOfString: s) / 2 @ 0)
				font: fontOrNil color: aColor ]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:47:24'!
drawString: s atWaist: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size atWaist: pt font: fontOrNil color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:52:31'!
drawString: s atWaistRight: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^font
		ifNil: [ pt ] 
		ifNotNil: [
			self
				drawString: s
				from: 1 to: s size
				atWaist: pt - ((font widthOfString: s) @ 0)
				font: fontOrNil color: aColor ]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:26:18'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: fontOrNil color: aColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:32:21'!
drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: fontOrNil color: aColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 12/21/2020 17:36:19'!
fontToUse: fontOrNil
	"Answer a suitable font, aFont if possible."

	self subclassResponsibility ! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:38:00'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	engine ifNil: [ ^nil ].

	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ self fontToUse: fontOrNil.
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1 + (0@(0 - (font ascent + font lineGap-1)))
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:46:18'!
drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	engine ifNil: [ ^nil ].

	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ self fontToUse: fontOrNil.
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1 + (0@(0 - (font ascent * 2 // 3 + font lineGap-1)))
		color: aColor! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 12/21/2020 17:36:15'!
fontToUse: fontOrNil
	"Answer a suitable font, aFont if possible."

	^fontOrNil ifNil: [ FontFamily defaultFamilyAndPointSize ].! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:37:33'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 |
	engine ifNil: [ ^nil ].

	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	^(self fontToUse: fontOrNil)
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:38:13'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"
	| p1 font |

	engine ifNil: [ ^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ self fontToUse: fontOrNil.

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

