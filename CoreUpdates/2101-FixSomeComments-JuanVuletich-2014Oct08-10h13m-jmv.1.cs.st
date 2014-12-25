'From Cuis 4.2 of 25 July 2013 [latest update: #2100] on 8 October 2014 at 10:45:43.08115 am'!

!BitBlt methodsFor: 'copying' stamp: 'jmv 10/6/2014 10:43'!
pixelAt: aPoint put: pixelValue
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPokerToForm:.  Overwrites the pixel at aPoint."
	destX _ aPoint x.
	destY _ aPoint y.
	sourceForm unhibernate. "before poking"
	sourceForm bits at: 1 put: pixelValue.
	self copyBits
"
| bb |
bb _ (BitBlt bitPokerToForm: Display).
[Sensor isAnyButtonPressed] whileFalse:
	[bb pixelAt: Sensor mousePoint put: 55]
"! !


!Form methodsFor: 'analyzing' stamp: 'jmv 10/8/2014 10:24'!
cgForPixelValue: pv orNot: not
	"Return the center of gravity for all pixels of value pv.
	Note:  If orNot is true, then produce the center of gravity for all pixels
	that are DIFFERENT from the supplied (background) value"
	| pixCount weighted xAndY |
	xAndY _ (Array with: (self xTallyPixelValue: pv orNot: not)
					with: (self yTallyPixelValue: pv orNot: not)) collect: [ :profile |	"For both x and y profiles..."
		pixCount _ 0.  weighted _ 0.
		profile doWithIndex: [ :t :i |
			pixCount _ pixCount + t.
			weighted _ weighted + (t*i)].
		pixCount = 0  "Produce average of nPixels weighted by coordinate"
			ifTrue: [0.0]
			ifFalse: [weighted asFloat / pixCount asFloat - 1.0]].

	^ xAndY first @ xAndY last
"
| r f cg |
[Sensor isAnyButtonPressed] whileFalse: [
	r _ Sensor mousePoint extent: 50@50.
	f _ Form extent: r extent depth: 16.
	f fromDisplay: r.
	cg _ f cgForPixelValue: (Color black pixelValueForDepth: f depth) orNot: false.
	f displayAt: 0@0.
	Display fill: (cg extent: 4) fillColor: Color red ]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 10/8/2014 10:34'!
pixelCompare: aRect with: otherForm at: otherLoc
	"Compare the selected bits of this form (those within aRect) against
	those in a similar rectangle of otherFrom.  Return the sum of the
	absolute value of the differences of the color values of every pixel.
	Obviously, this is most useful for rgb (16- or 32-bit) pixels but,
	in the case of 8-bits or less, this will return the sum of the differing
	bits of the corresponding pixel values (somewhat less useful)"
	| pixPerWord temp |
	pixPerWord _ 32//self depth.
	(aRect left\\pixPerWord = 0 and: [aRect right\\pixPerWord = 0]) ifTrue: [
		"If word-aligned, use on-the-fly difference"
		^ (BitBlt toForm: self) copy: aRect from: otherLoc in: otherForm
				fillColor: nil rule: 32].
	"Otherwise, combine in a word-sized form and then compute difference"
	temp _ self copy: aRect.
	temp copy: aRect from: otherLoc in: otherForm rule: 21.
	^ (BitBlt toForm: temp) copy: aRect from: otherLoc in: nil
				fillColor: (Bitmap with: 0) rule: 32
"  Dumb example prints zero only when you move over the original rectangle...
(Note: jmv, 8-Oct-2014. Seems not to work. Always zero in 32 bit, can not make zero in 16 bit...)
 | f diff |
f _ Form fromUser.
[ Sensor isAnyButtonPressed ] whileFalse: [
	diff _ f
		pixelCompare: f boundingBox
		with: Display
		at: Sensor mousePoint.
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	diff printString , '        ' displayAt: 0@0 ]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 10/8/2014 10:35'!
tallyPixelValuesInRect: destRect into: valueTable
	"Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable."

	(BitBlt toForm: self)
		sourceForm: self;  "src must be given for color map ops"
		sourceOrigin: 0@0;
		colorMap: valueTable;
		combinationRule: 33;
		destRect: destRect;
		copyBits.
	^ valueTable

"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r _ Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies _ (Display copy: r) tallyPixelValues.
	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"! !

!Form methodsFor: 'image manipulation' stamp: 'jmv 10/8/2014 10:36'!
tallyPixelValues
	"Return a Bitmap with tallies in it of the number of pixels in this Form that have each pixel value. Note that several Forms may be tallied into the same table by tallyPixelValuesPrimitive:into: with the same table. Also Forms of depth 16 or 32 can be tallied into a tables of size 512, 4096, or 32768 entries by making a direct call with a Bitmap of the given size."

	^ self tallyPixelValuesInRect: self boundingBox
		into: (Bitmap new: (1 bitShift: (self depth min: 15)))
"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r _ Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies _ (Display copy: r) tallyPixelValues.
	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"! !

!Form methodsFor: 'pixel access' stamp: 'jmv 10/8/2014 10:37'!
colorAt: aPoint put: aColor
	"Store a Color into the pixel at coordinate aPoint.  "

	self pixelValueAt: aPoint put: (self pixelValueFor: aColor).

"
[Sensor isAnyButtonPressed] whileFalse:
	[Display colorAt: Sensor mousePoint put: Color red]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/8/2014 10:40'!
flippedBy: direction centerAt: aPoint
	"Return a copy of the receiver flipped either #vertical or #horizontal."
	| newForm quad |
	newForm _ self class extent: self extent depth: depth.
	quad _ self boundingBox innerCorners.
	quad _ (direction = #vertical ifTrue: [#(2 1 4 3)] ifFalse: [#(4 3 2 1)])
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
"	newForm offset: (self offset flippedBy: direction centerAt: aPoint)."
	^ newForm
"
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p |
[ Sensor isAnyButtonPressed ] whileFalse: [
	f _ Form fromDisplay: ((p _ Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 _ f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p ]
"
! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/6/2014 10:44'!
magnify: aRectangle by: scale 
	"Answer a Form created as a scaling of the receiver.
	Scale may be a Float, and may be greater or less than 1.0."
	^ self magnify: aRectangle by: scale smoothing: 1

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
	[(Display magnify: (Sensor mousePoint extent: 31@41) by: 5@3) display]
"
"Scaling test...
| f cp | f _ Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
	[cp _ Sensor mousePoint.
	(f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent) display]]
"
"Consistency test...
 | f f2 p | [Sensor isAnyButtonPressed] whileFalse:
	[f _ Form fromDisplay: ((p _ Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 _ f magnify: f boundingBox by: 5@3.
	(f2 shrink: f2 boundingBox by: 5@3) displayAt: p]
"
! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/6/2014 10:44'!
magnify: aRectangle by: scale smoothing: cellSize
        "Answer a Form created as a scaling of the receiver.
        Scale may be a Float, and may be greater or less than 1.0."
        | newForm |
        newForm _ self blankCopyOf: aRectangle scaledBy: scale.
        (WarpBlt toForm: newForm)
                sourceForm: self;
                colorMap: (self colormapIfNeededFor: newForm);
                cellSize: cellSize;  "installs a new colormap if cellSize > 1"
                combinationRule: 3;
                copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
        ^ newForm

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) by: 0.5 smoothing: 2) display]
"
"Scaling test...
| f cp | f _ Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
        [cp _ Sensor mousePoint.
        (f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent smoothing: 2) display]]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/6/2014 10:44'!
magnify: aRectangle to: extent smoothing: cellSize
        "Answer a Form created as a scaling of the receiver.
        Scale may be a Float, and may be greater or less than 1.0."
        | newForm |
        newForm _ Form extent: extent depth: depth.
        (WarpBlt toForm: newForm)
                sourceForm: self;
                colorMap: (self colormapIfNeededFor: newForm);
                cellSize: cellSize;  "installs a new colormap if cellSize > 1"
                combinationRule: 3;
                copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
        ^ newForm

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) to: 300@200 smoothing: 2) display]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/8/2014 10:43'!
rotateBy: deg
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."

	^ self rotateBy: deg smoothing: 1
"
 | a f |  f _ Form fromDisplay: (0@0 extent: 200@200).  a _ 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotateBy: (a _ a+0.1)) display].
f display
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/8/2014 10:44'!
rotateBy: deg smoothing: cellSize
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."
	| side rot warp r1 pts p center |
	side _ 1 + ((width*width) + (height*height)) asFloat sqrt asInteger.
	rot _ Form extent: side@side depth: self depth.
	center _ rot extent // 2.

	"Now compute the sin and cos constants for the rotation angle." 
	warp _ (WarpBlt toForm: rot)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: rot);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form over.
	r1 _ rot boundingBox aligned: center with: self boundingBox center.

	pts _ r1 innerCorners collect: [ :pt |
		p _ pt - r1 center.
		(r1 center x asFloat + (p x asFloat*deg degreeCos) + (p y asFloat*deg degreeSin)) @
		(r1 center y asFloat - (p x asFloat*deg degreeSin) + (p y asFloat*deg degreeCos))].
	warp copyQuad: pts toRect: rot boundingBox.
	^ rot
"
 | a f |  f _ Form fromDisplay: (0@0 extent: 200@200).  a _ 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotateBy: (a _ a+0.1) smoothing: 2) display].
f display
"! !


!Point methodsFor: 'point functions' stamp: 'jmv 10/8/2014 10:45'!
quadrantOf: otherPoint
	"Return 1..4 indicating relative direction to otherPoint.
	1 is downRight, 2=downLeft, 3=upLeft, 4=upRight"
	^ x <= otherPoint x
		ifTrue: [y < otherPoint y ifTrue: [1] ifFalse: [4]]
		ifFalse: [y <= otherPoint y ifTrue: [2] ifFalse: [3]]
"
[Sensor isAnyButtonPressed] whileFalse: [
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	(Display boundingBox center quadrantOf: Sensor mousePoint) printString displayAt: 0@0]
"! !

