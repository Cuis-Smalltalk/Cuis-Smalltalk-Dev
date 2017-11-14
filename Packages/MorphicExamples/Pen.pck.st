'From Cuis 5.0 of 7 November 2016 [latest update: #3190] on 8 November 2017 at 3:43:04 pm'!
'Description Draw with a pen and messages as go: turn: etc.'!
!provides: 'Pen' 1 2!
!classDefinition: #Pen category: #Pen!
BitBlt subclass: #Pen
	instanceVariableNames: 'location direction penDown'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Pen'!
!classDefinition: 'Pen class' category: #Pen!
Pen class
	instanceVariableNames: ''!

!classDefinition: #PenPointRecorder category: #Pen!
Pen subclass: #PenPointRecorder
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Pen'!
!classDefinition: 'PenPointRecorder class' category: #Pen!
PenPointRecorder class
	instanceVariableNames: ''!


!Pen commentStamp: '<historical>' prior: 0!
My instances can scribble on the screen or some other Form, drawing and printing at any angle. Since I am a BitBlt, the drawing can be done with an arbitary source Form.
!

!PenPointRecorder commentStamp: 'nice 3/24/2010 07:38' prior: 0!
This class is a special kind of Pen that instead of actually drawing lines records the destination points for those lines. These points can later be accessed through my accessing method #points.

This can be useful when determining the boundaries of a drawing session.

Example:

| pen |
pen := PenPointRecorder new.
pen up; goto: 100@100; down; goto: 120@120.
Transcript cr;
	show: 'Bounding box for drawing: ';
	show: (Rectangle encompassing: pen points)

Implementation note: Shouldn't we override #drawFrom:to:withFirstPoint: instead, and what about #drawLoopX:Y:? Aren't we missing those calls?!

!Form methodsFor: '*Pen-filling' stamp: 'HilaireFernandes 11/8/2017 11:55:59'!
findShapeAroundSeedBlock: seedBlock
	"Build a shape that is black in any region marked by seedBlock. 
	SeedBlock will be supplied a form, in which to blacken various
	pixels as 'seeds'.  Then the seeds are smeared until 
	there is no change in the smear when it fills the region, ie,
	when smearing hits a black border and thus goes no further."
	| smearForm previousSmear all count smearPort |
	self depth > 1 ifTrue: [self halt]. "Only meaningful for B/W forms."
	all := self boundingBox.
	smearForm := Form extent: self extent.
	smearPort := BitBlt toForm: smearForm.
	seedBlock value: smearForm.		"Blacken seeds to be smeared"
	smearPort copyForm: self to: 0 @ 0 rule: Form erase.  "Clear any in black"
	previousSmear := smearForm copy.
	count := 1.
	[count = 10 and:   "check for no change every 10 smears"
		[count := 1.
		previousSmear copy: all from: 0 @ 0 in: smearForm rule: Form reverse.
		previousSmear isAllWhite]]
		whileFalse: 
			[smearPort copyForm: smearForm to: 1 @ 0 rule: Form under.
			smearPort copyForm: smearForm to: -1 @ 0 rule: Form under.
			"After horiz smear, trim around the region border"
			smearPort copyForm: self to: 0 @ 0 rule: Form erase.
			smearPort copyForm: smearForm to: 0 @ 1 rule: Form under.
			smearPort copyForm: smearForm to: 0 @ -1 rule: Form under.
			"After vert smear, trim around the region border"
			smearPort copyForm: self to: 0 @ 0 rule: Form erase.
			count := count + 1.
			count = 9 ifTrue: "Save penultimate smear for comparison"
				[previousSmear copy: all from: 0 @ 0 in: smearForm rule: Form over]].
	"Now paint the filled region in me with aHalftone"
	^ smearForm! !

!Pen methodsFor: 'operations' stamp: 'jm 4/28/1998 03:40'!
color: aColorOrInteger
	"Set the pen to the given color or to a color chosen from a fixed set of colors."

	| count c |
	aColorOrInteger isInteger
		ifTrue: [
			destForm depth = 1 ifTrue: [^ self fillColor: Color black].
			count := 19.  "number of colors in color wheel"
			c := (Color red wheel: count) at: ((aColorOrInteger * 7) \\ count) + 1]
		ifFalse: [c := aColorOrInteger].  "assume aColorOrInteger is a Color"
	self fillColor: c.
! !

!Pen methodsFor: 'initialize-release' stamp: 'jm 4/28/1998 04:02'!
defaultNib: widthInteger 
	"Nib is the tip of a pen. This sets up the pen, with a nib of width widthInteger. You can also set the shape of the pen nib using:
		roundNib: widthInteger, or
		squareNib: widthInteger, or
		sourceForm: aForm"
"Example:
	| bic |
	bic := Pen new sourceForm: Cursor normal.
	bic combinationRule: Form paint; turn: 90.
	10 timesRepeat: [bic down; go: 3; up; go: 10]."

	self color: Color black.
	self squareNib: widthInteger.
! !

!Pen methodsFor: 'accessing'!
direction
	"Answer the receiver's current direction. 0 is towards the top of the
	screen."

	^direction! !

!Pen methodsFor: 'operations'!
down
	"Set the state of the receiver's pen to down (drawing)."

	penDown := true! !

!Pen methodsFor: 'geometric designs' stamp: 'di 6/11/1998 22:01'!
dragon: n  "Display restoreAfter: [Display fillWhite. Pen new dragon: 10]."
	"Display restoreAfter: [Display fillWhite. 1 to: 4 do:
				[:i | Pen new color: i; turn: 90*i; dragon: 10]]"
	"Draw a dragon curve of order n in the center of the screen."
	n = 0
		ifTrue: [self go: 5]
		ifFalse: [n > 0
				ifTrue: [self dragon: n - 1; turn: 90; dragon: 1 - n]
				ifFalse: [self dragon: -1 - n; turn: -90; dragon: 1 + n]]
! !

!Pen methodsFor: 'geometric designs' stamp: 'di 6/14/1998 13:42'!
filberts: n side: s   "Display restoreAfter: [Pen new filberts: 4 side: 5]"
	"Two Hilbert curve fragments form a Hilbert tile. Draw four interlocking 
	tiles of order n and sides length s."
	| n2 |
	Display fillWhite.
	n2 := 1 bitShift: n - 1.
	self up; go: 0 - n2 * s; down.
	1 to: 4 do: 
		[:i | 
		self fill: [:p |
				p hilbert: n side: s.
				p go: s.
				p hilbert: n side: s.
				p go: s.
				p up.
				p go: n2 - 1 * s.
				p turn: -90.
				p go: n2 * s.
				p turn: 180.
				p down]
			color: (Color perform: (#(yellow red green blue) at: i))]! !

!Pen methodsFor: 'operations' stamp: 'HilaireFernandes 11/8/2017 12:11:52'!
fill: drawBlock color: color
	| region tileForm tilePen shape saveColor recorder |
	drawBlock value: (recorder := self as: PenPointRecorder).
	region := Rectangle encompassing: recorder points.
	tileForm := Form extent: region extent+6.
	tilePen := Pen newOnForm: tileForm.
	tilePen location: location-(region origin-3)
		direction: direction
		penDown: penDown.
	drawBlock value: tilePen.  "Draw the shape in B/W"
	saveColor := halftoneForm.
	drawBlock value: self.
	halftoneForm := saveColor.
	shape := (tileForm findShapeAroundSeedBlock: [:f | 
		f border: (0@0 corner: f extent) width: 1]) reverse.
	shape copy: shape boundingBox from: tileForm to: 0@0 rule: Form erase.
	destForm fillShape: shape fillColor: color at: region origin-3! !

!Pen methodsFor: 'operations'!
go: distance 
	"Move the pen in its current direction a number of bits equal to the 
	argument, distance. If the pen is down, a line will be drawn using the 
	receiver's form source as the shape of the drawing brush."

	self goto: (direction degreeCos @ direction degreeSin) * distance + location! !

!Pen methodsFor: 'operations' stamp: 'di 11/4/97 20:11'!
goto: aPoint 
	"Move the receiver to position aPoint. If the pen is down, a line will be 
	drawn from the current position to the new one using the receiver's 
	form source as the shape of the drawing brush. The receiver's set 
	direction does not change."
	| old |
	old := location.
	location := aPoint.
	penDown ifTrue: [self drawFrom: old rounded
								to: location rounded]

	"NOTE:  This should be changed so it does NOT draw the first point, so as
	not to overstrike at line junctions.  At the same time, place should draw
	a single dot if the pen is down, as should down (put-pen-down) if it
	was not down before."! !

!Pen methodsFor: 'geometric designs'!
hilbert: n side: s 
	"Draw an nth level Hilbert curve with side length s in the center of the 
	screen. Write directly into the display's bitmap only. A Hilbert curve is 
	a space-filling curve."

	| a m |
	n = 0 ifTrue: [^self turn: 180].
	n > 0
		ifTrue: 
			[a := 90.
			m := n - 1]
		ifFalse: 
			[a := -90.
			m := n + 1].
	self turn: a.
	self hilbert: 0 - m side: s.
	self turn: a; go: s.
	self hilbert: m side: s.
	self turn: 0 - a; go: s; turn: 0 - a.
	self hilbert: m side: s.
	self go: s; turn: a.
	self hilbert: 0 - m side: s.
	self turn: a
	" 
	(Pen new) hilbert: 3 side: 8. 
	(Pen new sourceForm: Cursor wait) combinationRule: Form under; 
	hilbert: 3 side: 25.
	"! !

!Pen methodsFor: 'geometric designs'!
hilberts: n   "Display restoreAfter: [Display fillWhite.  Pen new hilberts: 5]"
	"Draws n levels of nested Hilbert curves"
	| s |
	self up; turn: 90; go: 128; down.
	1 to: n do: 
		[:i | 
		s := 256 bitShift: 0 - i.
		self defaultNib: n - i * 2 + 1.
		self color: i+1.
		self up; go: 0 - s / 2; turn: -90; go: s / 2; turn: 90; down.
		self hilbert: i side: s.
		self go: s.
		self hilbert: i side: s.
		self go: s]! !

!Pen methodsFor: 'operations'!
home
	"Place the receiver at the center of its frame."
	location := destForm boundingBox center! !

!Pen methodsFor: 'accessing'!
location
	"Answer where the receiver is currently located."

	^location! !

!Pen methodsFor: 'private' stamp: 'di 6/11/1998 16:09'!
location: aPoint direction: aFloat penDown: aBoolean
	location := aPoint.
	direction := aFloat.
	penDown := aBoolean! !

!Pen methodsFor: 'geometric designs' stamp: 'HilaireFernandes 11/8/2017 15:39:15'!
mandala: npoints
	"Display restoreAfter: [Pen new mandala: 30]"
	"On a circle of diameter d, place npoints number of points. Draw all 	possible connecting lines between the circumferential points."
	| l points d |
	Display fillWhite.
	d := Display height-50.
	l := 3.14 * d / npoints.
	self home; up; turn: -90; go: d // 2; turn: 90; go: 0 - l / 2; down.
	points := Array new: npoints.
	1 to: npoints do: [:i | 
		points at: i put: location rounded.
		self go: l; turn: 360.0 / npoints].
	npoints // 2 to: 1 by: -1 do: [:i | 
		self color: i.
		1 to: npoints do: [:j | 
			self place: (points at: j).
			self goto: (points at: j + i - 1 \\ npoints + 1)]].
	ImageMorph new image: Display copy; openInWorld! !

!Pen methodsFor: 'operations'!
north
	"Set the receiver's direction to facing toward the top of the display screen."

	direction := 270! !

!Pen methodsFor: 'operations'!
place: aPoint 
	"Set the receiver at position aPoint. No lines are drawn."

	location := aPoint! !

!Pen methodsFor: 'operations' stamp: 'tpr 3/26/2013 11:12'!
print: str withFont: font
	"Print the given string in the given font at the current heading"
	| lineStart scale wasDown |
	scale := sourceForm width.
	wasDown := penDown.
	lineStart := location.
	str do:
		[:char |
		char = Character cr ifTrue:
			[self place: lineStart; up; turn: 90; go: font height*scale; turn: -90; down]
		ifFalse:
			[ | charStart pix rowStart form backgroundCode |
			form := font characterFormAt: char.
			backgroundCode := 1<< (form depth // 3 * 3) - 1.
			charStart := location.
wasDown ifTrue: [
			self up; turn: -90; go: font descent*scale; turn: 90; down.
			0 to: form height-1 do:
				[:y |
				rowStart := location.
				pix := RunArray newFrom:
					((0 to: form width-1) collect: [:x | form pixelValueAt: x@y]).
				pix runs with: pix values do:
					[:run :value |
					value = backgroundCode
						ifTrue: [self up; go: run*scale; down]
						ifFalse: [self go: run*scale]].
				self place: rowStart; up; turn: 90; go: scale; turn: -90; down].
].
			self place: charStart; up; go: form width*scale; down].
			].
	wasDown ifFalse: [self up]
"
Display restoreAfter:
[Pen new squareNib: 2; color: Color red; turn: 45;
	print: 'The owl and the pussycat went to sea
in a beautiful pea green boat.' withFont: TextStyle defaultFont]
"! !

!Pen methodsFor: 'initialize-release' stamp: 'jm 4/28/1998 04:03'!
roundNib: diameter
	"Makes this pen draw with a round dot of the given diameter."

	self sourceForm: (Form dotOfSize: diameter).
	combinationRule := Form paint.
! !

!Pen methodsFor: 'private'!
sourceForm: aForm
	(aForm depth = 1 and: [destForm depth > 1])
		ifTrue: ["Map 1-bit source to all ones for color mask"
				colorMap := Bitmap with: 0 with: 16rFFFFFFFF]
		ifFalse: [colorMap := nil].
	^ super sourceForm: aForm! !

!Pen methodsFor: 'geometric designs' stamp: 'jm 5/6/1998 22:26'!
spiral: n angle: a 
	"Draw a double squiral (see Papert, MindStorms), where each design is made
	by moving the receiver a distance of n after turning the amount + or -a."

	1 to: n do: 
		[:i | 
		self color: i * 2.
		self go: i; turn: a]
"
	Display restoreAfter: [
		Display fillWhite. Pen new spiral: 200 angle: 89; home; spiral: 200 angle: -89].
"! !

!Pen methodsFor: 'initialize-release' stamp: 'jm 4/28/1998 04:03'!
squareNib: widthInteger 
	"Makes this pen draw with a square nib of the given width."

	self sourceForm: (Form extent: widthInteger @widthInteger) fillBlack.
	self combinationRule: Form over.  "a bit faster than paint mode"
! !

!Pen methodsFor: 'operations'!
turn: degrees 
	"Change the direction that the receiver faces by an amount equal to the 
	argument, degrees."

	direction := direction + degrees! !

!Pen methodsFor: 'operations'!
up
	"Set the state of the receiver's pen to up (no drawing)."

	penDown := false! !

!Pen methodsFor: 'geometric designs' stamp: 'HilaireFernandes 11/8/2017 11:37:38'!
web   "Display restoreAfter: [Pen new web]"
	"Draw pretty web-like patterns from the mouse movement on the screen.
	Press the mouse button to draw, option-click to exit.
	By Dan Ingalls and Mark Lentczner. "
	| history newPoint ancientPoint lastPoint filter color |
	"self erase."
	color := 1.
	[ history := OrderedCollection new.
	Sensor waitButton.
	Sensor isMouseButton2Pressed ifTrue: [^ self].
	filter := lastPoint := Sensor mousePoint.
	20 timesRepeat: [ history addLast: lastPoint ].
	self color: (color := color + 1).
	[ Sensor isMouseButton1Pressed ] whileTrue: 
		[ newPoint := Sensor mousePoint.
		(newPoint = lastPoint) ifFalse:
			[ ancientPoint := history removeFirst.
			filter := filter * 4 + newPoint // 5.
			self place: filter.
			self goto: ancientPoint.
			lastPoint := newPoint.
			history addLast: filter ] ] ] repeat! !

!Pen class methodsFor: 'examples' stamp: 'jm 5/6/1998 22:28'!
example
	"Draw a spiral with a pen that is 2 pixels wide."
	"Display restoreAfter: [Pen example]"

	| bic |
	bic := self new.
	bic defaultNib: 2.
	bic color: Color blue.
	bic combinationRule: Form over.
	1 to: 100 do: [:i | bic go: i*4. bic turn: 89].
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'tpr 3/15/2013 12:42'!
feltTip: width cellSize: cellSize
	"Warning: This example potentially uses a large amount of memory--it creates a Form with cellSize squared bits for every Display pixel."
	"In this example, all drawing is done into a large, monochrome Form and then scaled down onto the Display using smoothing. The larger the cell size, the more possible shades of gray can be generated, and the smoother the resulting line appears. A cell size of 8 yields 64 possible grays, while a cell size of 16 gives 256 levels, which is about the maximum number of grays that the human visual system can distinguish. The width parameter determines the maximum line thickness. Requires the optional tablet support primitives which may not be supported on all platforms. Works best in full screen mode. Shift-mouse to exit." 
	"Pen feltTip: 2.7 cellSize: 8"

	| tabletScale bitForm pen warp |
	tabletScale := self tabletScaleFactor.
	bitForm := Form extent: Display extent * cellSize depth: 1.
	pen := Pen newOnForm: bitForm.
	pen color: Color black.
	warp := (WarpBlt toForm: Display)
		sourceForm: bitForm;
		colorMap: (bitForm colormapIfNeededFor: Display);
		cellSize: cellSize;
		combinationRule: Form over.
	Display fillColor: Color white.
	Display restoreAfter: [ | p r nibSize srcR startP dstR |
		[Sensor shiftPressed and: [Sensor anyButtonPressed]] whileFalse: [
			p := (Sensor tabletPoint * cellSize * tabletScale) rounded.
			nibSize := (Sensor tabletPressure * (cellSize * width)) rounded.
		     nibSize > 0
				ifTrue: [
					pen squareNib: nibSize.
					startP := pen location.
					pen goto: p.
					r := startP rect: pen location.
					dstR := (r origin // cellSize) corner: ((r corner + nibSize + (cellSize - 1)) // cellSize).
					srcR := (dstR origin * cellSize) corner: (dstR corner * cellSize).
					warp copyQuad: srcR innerCorners toRect: dstR]
				ifFalse: [
					pen place: p]]].
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'nice 12/27/2009 03:11'!
inkBrush
	"Similar to simplePressurePen, but this example uses the average of the recent pen pressure values. The effect is that of a Japanese ink brush that comes up gradually off the paper as the brush is lifted, causing end (and beginning) of each stroke to taper. Requires the optional tablet support primitives which may not be supported on all platforms. Works best in full screen mode. Shift-mouse to exit." 
	"Pen inkBrush"

	| tabletScale historyMSecs pressureHistory pen |
	tabletScale := self tabletScaleFactor.
	historyMSecs := 120.
	pressureHistory := OrderedCollection new.
	pen := Pen newOnForm: Display.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [ | sum p averagePressure now currentPressure |
		[Sensor shiftPressed and: [Sensor anyButtonPressed]] whileFalse: [
			"compute the average pressure over last historyMSecs milliseconds"
			now := Time millisecondClockValue.
			currentPressure := (20.0 * Sensor tabletPressure) rounded.
			pressureHistory addLast: (Array with: now with: currentPressure).
			[pressureHistory size > 0 and:
			 [(pressureHistory first first + historyMSecs) < now]]
				whileTrue: [pressureHistory removeFirst].  "prune old entries"
			sum := pressureHistory inject: 0 into: [:t :e | t + e last].
			averagePressure := sum // pressureHistory size.

			p := (Sensor tabletPoint * tabletScale) rounded.
		     averagePressure > 0
				ifTrue: [
					pen roundNib: averagePressure.
					pen goto: p]
				ifFalse: [
					pen place: p]]].
! !

!Pen class methodsFor: 'instance creation'!
new
	^ self newOnForm: Display! !

!Pen class methodsFor: 'instance creation'!
newOnForm: aForm
	| pen |
	pen := super new.
	pen setDestForm: aForm.
	pen sourceOrigin: 0@0.
	pen home.
	pen defaultNib: 1.
	pen north.
	pen down.
	^ pen! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'nice 12/27/2009 03:11'!
simplePressurePen
	"An example of using a pressure sensitive pen to control the thickness of the pen. This requires the optional tablet support primitives which may not be supported on all platforms. Works best in full screen mode. Shift-mouse to exit." 
	"Pen simplePressurePen"

	| tabletScale pen |
	tabletScale := self tabletScaleFactor.
	pen := Pen newOnForm: Display.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [ | p pressure |
		[Sensor shiftPressed and: [Sensor anyButtonPressed]] whileFalse: [
			p := (Sensor tabletPoint * tabletScale) rounded.
			pressure := (15.0 * Sensor tabletPressure) rounded.
		     pressure > 0
				ifTrue: [
					pen roundNib: pressure.
					pen goto: p]
				ifFalse: [
					pen place: p]]].
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'jm 4/13/1999 11:12'!
tabletScaleFactor
	"Answer a Point that scales tablet coordinates to Display coordinates, where the full extent of the tablet maps to the extent of the entire Display."

	| tabletExtent |
	tabletExtent := Sensor tabletExtent.
	^ (Display width asFloat / tabletExtent x) @ (Display height asFloat / tabletExtent y)
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'nice 12/27/2009 03:11'!
testMouseTracking
	"A very simple example of drawing using the mouse. Compare the tracking speed of this example with that of testTabletTracking. Mouse down to draw a stroke, shift-mouse to exit." 
	"Pen testMouseTracking"

	| pen |
	pen := Pen newOnForm: Display.
	pen roundNib: 8.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [ | p |
		[Sensor shiftPressed and: [Sensor anyButtonPressed]] whileFalse: [
			p := Sensor cursorPoint.
		     Sensor anyButtonPressed
				ifTrue: [pen goto: p]
				ifFalse: [
					pen color: Color random.
					pen place: p]]].
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'nice 12/27/2009 03:11'!
testTabletTracking
	"A very simple example of drawing using the pen of a digitizing tablet such as a Wacom ArtZ tablet. This requires the optional tablet support primitives which may not be supported on all platforms. Compare the tracking speed of this example with that of testMouseTracking. On a Macintosh, the tablet primitives provide roughly 120 samples/second versus only 60 mouse samples/second, and the difference is noticable. Works best in full screen mode. Mouse down to draw a stroke, shift-mouse to exit." 
	"Pen testTabletTracking"

	| tabletScale pen |
	tabletScale := self tabletScaleFactor.
	pen := Pen newOnForm: Display.
	pen roundNib: 8.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [ | p |
		[Sensor shiftPressed and: [Sensor anyButtonPressed]] whileFalse: [
			p := (Sensor tabletPoint * tabletScale) rounded.
		     Sensor tabletPressure > 0
				ifTrue: [pen goto: p]
				ifFalse: [
					pen color: Color random.
					pen place: p]]].
! !

!PenPointRecorder methodsFor: 'line drawing' stamp: 'md 11/14/2003 16:56'!
drawFrom: p1 to: p2
	"Overridden to skip drawing but track bounds of the region traversed."

	points ifNil: [points := OrderedCollection with: p1].
	points addLast: p2! !

!PenPointRecorder methodsFor: 'accessing' stamp: 'di 6/21/1998 09:35'!
points
	^ points! !
