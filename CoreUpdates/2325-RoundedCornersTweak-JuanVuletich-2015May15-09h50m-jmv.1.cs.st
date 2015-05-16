'From Cuis 4.2 of 25 July 2013 [latest update: #2324] on 15 May 2015 at 9:50:26.729365 am'!

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:47'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy l1 l2 |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (width - 1 - x @ dy) r.
							l1 _ ((r - d-1+aaw max: 0.0) min: aaw) / aaw.
							l2 _ ((d - r+bw+aaw max: 0.0) min: aaw) / aaw.
							l _ l1 min: l2.
							].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:41'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy l1 l2 |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (x @ dy) r.
							l1 _ ((r - d-1+aaw max: 0.0) min: aaw) / aaw.
							l2 _ ((d - r+bw+aaw max: 0.0) min: aaw) / aaw.
							l _ l1 min: l2.
							].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !


!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 5/15/2015 09:42'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	^self cachedForms
		at: { #bottomLeft. r. height. gradientTopFactor . gradientBottomFactor . bw}
		ifAbsentPut: [
			Form
				bottomLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor
				borderWidth: bw ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 5/15/2015 09:42'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	^self cachedForms
		at: { #bottomRight. r. height. gradientTopFactor . gradientBottomFactor . bw}
		ifAbsentPut: [
			Form
				bottomRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor
				borderWidth: bw ]! !


!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:37'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (width - 1 - x @ dy) r.
							l _ ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:38'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	"Create and answer a Form for the bottomRight corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (x @ dy) r.
							l _ ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !


!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 5/15/2015 09:43'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	BitBltCanvas releaseClassCachedState. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor |
	"top stripe"
	self
		image: (self class topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) color: bottomColor! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 5/15/2015 09:43'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	"
	Display restore.
	BitBltCanvas releaseClassCachedState. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35
	"
	| h2 |
	"top stripe"
	self
		image: (self class topLeftCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: h1) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor.
	
	"bottom stripe"
	h2 _ aRectangle height - h1.
	self
		image: (self class bottomLeftCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft + (0@h1).
	self
		image: (self class bottomRightCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight + (r negated@h1).
	self
		fillRectangle: ((aRectangle topLeft + (r@h1)) extent: (aRectangle width-r-r@h2))
		tilingWith: (self class verticalGrayGradient: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 5/15/2015 09:47'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	BitBltCanvas releaseClassCachedState
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green
	"
	"top stripe"
	| bottomColor he tl tr |
	self
		image: (self class topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ aRectangle topLeft + (0@lh).
	tr _ aRectangle topRight + (bw negated@lh).
	he _ bw@(aRectangle height - lh - r).
	self fillRectangle: (tl extent: he) color: bottomColor.
	self fillRectangle: (tr extent: he) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	BitBltCanvas releaseClassCachedState!

