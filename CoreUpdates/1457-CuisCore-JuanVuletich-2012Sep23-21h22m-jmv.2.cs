'From Cuis 4.0 of 21 April 2012 [latest update: #1456] on 23 September 2012 at 9:59:07 pm'!

!RectangleLikeMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Subclasses can add things like 'roundedCornerRadious' or such.!


!DisplayObject methodsFor: 'displaying-Display' stamp: 'jmv 9/23/2012 21:38'!
follow: locationBlock while: durationBlock bitsBehind: initialBitsBehind startingLoc: loc
   "Move an image around on the Display. Restore the background continuously without causing flashing. The argument, locationBlock, supplies each new location, and the argument, durationBlock, supplies true to continue or false to stop. This variant takes the bitsBehind as an input argument, and returns the final saved saved bits as method value."

   | location rect1 save1 save1Blt buffer bufferBlt newLoc rect2 bothRects |
   location _ loc.
   rect1 _ location extent: self extent.
   save1 _ initialBitsBehind.
   save1Blt _ BitBlt toForm: save1.
   buffer _ Form extent: self extent*2 depth: Display depth.  "Holds overlapping region"
   bufferBlt _ BitBlt toForm: buffer.
   Display deferUpdates: true.
   self displayOn: Display at: location rule: Form paint.
   Display deferUpdates: false; forceToScreen: (location extent: self extent).
   [durationBlock value] whileTrue: [
		newLoc _ locationBlock value.
		newLoc ~= location ifTrue: [
			rect2 _ newLoc extent: self extent.
			bothRects _ rect1 merge: rect2.
			(rect1 intersects: rect2)
				ifTrue: [  "when overlap, buffer background for both rectangles"
					bufferBlt copyFrom: bothRects in: Display to: 0@0.
					bufferBlt copyFrom: save1 boundingBox in: save1 to: rect1 origin - bothRects origin.
					"now buffer is clean background; get new bits for save1"
					save1Blt copy: (0@0 extent: self extent) from: rect2 origin - bothRects origin in: buffer.
					self displayOnPort: bufferBlt at: rect2 origin - bothRects origin rule: Form paint.
					Display deferUpdates: true.
					Display copy: bothRects from: 0@0 in: buffer rule: Form over.
					Display deferUpdates: false; forceToScreen: bothRects]
				ifFalse: [  "when no overlap, do the simple thing (both rects might be too big)"
					Display deferUpdates: true.
					Display copy: (location extent: save1 extent) from: 0@0 in: save1 rule: Form over.
					save1Blt copyFrom: rect2 in: Display to: 0@0.
					self displayOn: Display at: newLoc rule: Form paint.
					Display deferUpdates: false; 
						forceToScreen: (location extent: save1 extent); 
						forceToScreen: (newLoc extent: self extent)].
			location _ newLoc.
			rect1 _ rect2]].

	^ save1 displayOn: Display at: location
! !


!DisplayMedium methodsFor: 'coloring' stamp: 'jmv 9/23/2012 21:38'!
fillShape: aShapeForm fillColor: aColor at: location
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	((BitBlt destForm: self sourceForm: aShapeForm fillColor: aColor
		combinationRule: Form paint
		destOrigin: location + aShapeForm offset sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits! !


!Form methodsFor: 'accessing' stamp: 'jmv 9/23/2012 21:30'!
getCanvas
	"Return a Canvas that can be used to draw onto the receiver"
	^FormCanvas onForm: self! !

!Form methodsFor: 'analyzing' stamp: 'jmv 9/23/2012 21:43'!
pixelCompare: aRect with: otherForm at: otherLoc
	"Compare the selected bits of this form (those within aRect) against
	those in a similar rectangle of otherFrom.  Return the sum of the
	absolute value of the differences of the color values of every pixel.
	Obviously, this is most useful for rgb (16- or 32-bit) pixels but,
	in the case of 8-bits or less, this will return the sum of the differing
	bits of the corresponding pixel values (somewhat less useful)"
	| pixPerWord temp |
	pixPerWord _ 32//self depth.
	(aRect left\\pixPerWord = 0 and: [aRect right\\pixPerWord = 0]) ifTrue:
		["If word-aligned, use on-the-fly difference"
		^ (BitBlt toForm: self) copy: aRect from: otherLoc in: otherForm
				fillColor: nil rule: 32].
	"Otherwise, combine in a word-sized form and then compute difference"
	temp _ self copy: aRect.
	temp copy: aRect from: otherLoc in: otherForm rule: 21.
	^ (BitBlt toForm: temp) copy: aRect from: otherLoc in: nil
				fillColor: (Bitmap with: 0) rule: 32
"  Dumb example prints zero only when you move over the original rectangle...
 | f diff | f _ Form fromUser.
[Sensor anyButtonPressed] whileFalse:
	[diff _ f pixelCompare: f boundingBox
		with: Display at: Sensor mousePoint.
	diff printString , '        ' displayAt: 0@0]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 9/23/2012 21:43'!
primCountBits
	"Count the non-zero pixels of this form."
	self depth > 8 ifTrue:
		[^(self asFormOfDepth: 8) primCountBits].
	^ (BitBlt toForm: self)
		fillColor: (Bitmap with: 0);
		destRect: (0@0 extent: width@height);
		combinationRule: 32;
		copyBits! !

!Form methodsFor: 'analyzing' stamp: 'jmv 9/23/2012 21:46'!
rectangleEnclosingPixelsNotOfColor: aColor
	"Answer the smallest rectangle enclosing all the pixels of me that are different from the given color. Useful for extracting a foreground graphic from its background."

	| cm slice copyBlt countBlt top bottom newH left right |
	"map the specified color to 1 and all others to 0"
	cm _ Bitmap new: (1 bitShift: (self depth min: 15)).
	cm primFill: 1.
	cm at: (aColor indexInMap: cm) put: 0.

	"build a 1-pixel high horizontal slice and BitBlts for counting pixels of interest"
	slice _ Form extent: width@1 depth: 1.
	copyBlt _ (BitBlt toForm: slice)
		sourceForm: self;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: 1;
		colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
		fillColor: (Bitmap with: 0);
		destRect: (0@0 extent: slice extent);
		combinationRule: 32.

	"scan in from top and bottom"
	top _ (0 to: height)
		detect: [:y |
			copyBlt sourceOrigin: 0@y; copyBits.
			countBlt copyBits > 0]
		ifNone: [^ 0@0 extent: 0@0].
	bottom _ (height - 1 to: top by: -1)
		detect: [:y |
			copyBlt sourceOrigin: 0@y; copyBits.
			countBlt copyBits > 0].

	"build a 1-pixel wide vertical slice and BitBlts for counting pixels of interest"
	newH _ bottom - top + 1.
	slice _ Form extent: 1@newH depth: 1.
	copyBlt _ (BitBlt toForm: slice)
		sourceForm: self;
		combinationRule: Form over;
		destX: 0 destY: 0 width: 1 height: newH;
		colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
		fillColor: (Bitmap with: 0);
		destRect: (0@0 extent: slice extent);
		combinationRule: 32.

	"scan in from left and right"
	left _ (0 to: width)
		detect: [:x |
			copyBlt sourceOrigin: x@top; copyBits.
			countBlt copyBits > 0].
	right _ (width - 1 to: left by: -1)
		detect: [:x |
			copyBlt sourceOrigin: x@top; copyBits.
			countBlt copyBits > 0].

	^ left@top corner: (right + 1)@(bottom + 1)
! !

!Form methodsFor: 'analyzing' stamp: 'jmv 9/23/2012 21:43'!
tallyPixelValuesInRect: destRect into: valueTable
	"Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable."

	(BitBlt toForm: self)
		sourceForm: self;  "src must be given for color map ops"
		sourceOrigin: 0@0;
		tallyMap: valueTable;
		combinationRule: 33;
		destRect: destRect;
		copyBits.
	^ valueTable

"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor anyButtonPressed] whileFalse:
	[r _ Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies _ (Display copy: r) tallyPixelValues.
	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 9/23/2012 21:43'!
xTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by x-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice countBlt copyBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: 1@height.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				halftoneForm: nil combinationRule: Form over
				destOrigin: 0@0 sourceOrigin: 0@0 extent: 1 @ slice height
				clipRect: slice boundingBox) colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (0@0 extent: slice extent);
				combinationRule: 32.
	^ (0 to: width-1) collect:
		[:x |
		copyBlt sourceOrigin: x@0; copyBits.
		countBlt copyBits]! !

!Form methodsFor: 'analyzing' stamp: 'jmv 9/23/2012 21:44'!
yTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice copyBlt countBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: width@1.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				halftoneForm: nil combinationRule: Form over
				destOrigin: 0@0 sourceOrigin: 0@0 extent: slice width @ 1
				clipRect: slice boundingBox) colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (0@0 extent: slice extent);
				combinationRule: 32.
	^ (0 to: height-1) collect:
		[:y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits]! !

!Form methodsFor: 'bordering' stamp: 'jmv 9/23/2012 21:39'!
border: rect width: borderWidth rule: rule fillColor: fillColor
        "Paint a border whose rectangular area is defined by rect. The
width of the border of each side is borderWidth. Uses fillColor for drawing
the border."
        | blt |
        blt _ (BitBlt toForm: self) combinationRule: rule; fillColor: fillColor.
        blt sourceOrigin: 0@0.
        blt destOrigin: rect origin.
        blt width: rect width; height: borderWidth; copyBits.
        blt destY: rect corner y - borderWidth; copyBits.
        blt destY: rect origin y + borderWidth.
        blt height: rect height - borderWidth - borderWidth; width:
borderWidth; copyBits.
        blt destX: rect corner x - borderWidth; copyBits! !

!Form methodsFor: 'bordering' stamp: 'jmv 9/23/2012 21:39'!
borderFormOfWidth: borderWidth sharpCorners: sharpen
	"Smear this form around and then subtract the original to produce
	an outline.  If sharpen is true, then cause right angles to be outlined
	by right angles (takes an additional diagonal smears ANDed with both
	horizontal and vertical smears)."
	| smearForm bigForm smearPort all cornerForm cornerPort nbrs |
	self depth > 1 ifTrue: [self halt]. "Only meaningful for B/W forms."
	bigForm _ self copy.
	all _ bigForm boundingBox.
	smearForm _ Form extent: self extent.
	smearPort _ BitBlt toForm: smearForm.
	sharpen ifTrue:
		[cornerForm _ Form extent: self extent.
		cornerPort _ BitBlt toForm: cornerForm].
	nbrs _ (0@0) fourNeighbors.
	1 to: borderWidth do:
		[:i |  "Iterate to get several layers of 'skin'"
		nbrs do:
			[:d |  "Smear the self in 4 directions to grow each layer of skin"
			smearPort copyForm: bigForm to: d rule: Form under].
		sharpen ifTrue:
			["Special treatment to smear sharp corners"
			nbrs with: ((2 to: 5) collect: [:i2 | nbrs atWrap: i2]) do:
				[:d1 :d2 |
				"Copy corner points diagonally"
				cornerPort copyForm: bigForm to: d1+d2 rule: Form over.
				"But only preserve if there were dots on either side"
				cornerPort copyForm: bigForm to: d1+d1+d2 rule: Form and.
				cornerPort copyForm: bigForm to: d1+d2+d2 rule: Form and.
				smearPort copyForm: cornerForm to: 0@0 rule: Form under].
			].
		bigForm copy: all from: 0@0 in: smearForm rule: Form over.
		].
	"Now erase the original shape to obtain the outline"
	bigForm copy: all from: 0@0 in: self rule: Form erase.
	^ bigForm! !

!Form methodsFor: 'color mapping' stamp: 'jmv 9/23/2012 21:42'!
mapColor: oldColor to: newColor
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."

	| map |
	map _ (Color cachedColormapFrom: self depth to: self depth) copy.
	map at: (oldColor indexInMap: map) put: (newColor pixelWordForDepth: self depth).
	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits.
! !

!Form methodsFor: 'color mapping' stamp: 'jmv 9/23/2012 21:42'!
mapColors: oldColorBitsCollection to: newColorBits
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map |
	self depth < 16
		ifTrue: [map _ (Color cachedColormapFrom: self depth to: self depth) copy]
		ifFalse: [
			"use maximum resolution color map"
			"source is 16-bit or 32-bit RGB; use colormap with 5 bits per color component"
			map _ Color computeRGBColormapFor: self depth bitsPerColor: 5].
	oldColorBitsCollection do:[ :oldColor | map at: oldColor put: newColorBits].

	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits.
! !

!Form methodsFor: 'converting' stamp: 'jmv 9/23/2012 21:38'!
asFormOfDepth: d
	| newForm |
	d = self depth ifTrue: [ ^self ].
	newForm _ Form extent: self extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (self colormapIfNeededFor: newForm);
		copy: (self boundingBox)
		from: 0@0 in: self
		fillColor: nil rule: Form over.
	"If we build a 32bpp from one of smaller depth,
	it will have zero in the alpha channel (until BitBlt is fixed!!)"
	d = 32 ifTrue: [
		newForm fixAlpha ].
	^newForm! !

!Form methodsFor: 'converting' stamp: 'jmv 9/23/2012 21:38'!
asFormOfNativeDepth: d
	| newForm |
	d = self nativeDepth ifTrue:[^self].
	newForm _ Form extent: self extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (self colormapIfNeededFor: newForm);
		copy: (self boundingBox)
		from: 0@0 in: self
		fillColor: nil rule: Form over.
	^newForm! !

!Form methodsFor: 'converting' stamp: 'jmv 9/23/2012 21:39'!
asGrayScale
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)"
	| f32 srcForm result map bb grays |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayScale].
	self unhibernate.
	srcForm _ Form extent: (width * 4)@height depth: 8.
	srcForm bits: bits.
	result _ ColorForm extent: width@height depth: 8.
	map _ Bitmap new: 256.
	2 to: 256 do: [:i | map at: i put: i - 1].
	map at: 1 put: 1.  "map zero pixel values to near-black"
	bb _ (BitBlt toForm: result)
		sourceForm: srcForm;
		combinationRule: Form over;
		colorMap: map.
	0 to: width - 1 do: [:dstX |
		bb  sourceRect: (((dstX * 4) + 2)@0 extent: 1@height);
			destOrigin: dstX@0;
			copyBits].

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map _ Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: 0@0;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	
	grays _ (0 to: 255) collect: [:brightness | Color gray: brightness asFloat / 255.0].
	grays at: 1 put: Color transparent.
	result colors: grays.
	^ result
! !

!Form methodsFor: 'copying' stamp: 'jmv 9/23/2012 21:39'!
copy: destRectangle from: sourcePt in: sourceForm rule: rule 
	"Make up a BitBlt table and copy the bits."
	(BitBlt toForm: self)
		copy: destRectangle
		from: sourcePt in: sourceForm
		fillColor: nil rule: rule! !

!Form methodsFor: 'copying' stamp: 'jmv 9/23/2012 21:39'!
copyBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		halftoneForm: nil
		combinationRule: 30
		destOrigin: destOrigin
		sourceOrigin: 0@0
		extent: sourceForm extent
		clipRect: self boundingBox)
		copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f _ Form fromUser. f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 copyBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

!Form methodsFor: 'copying' stamp: 'jmv 9/23/2012 21:39'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule fillColor: aForm 
	"Make up a BitBlt table and copy the bits."

	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		fillColor: aForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) copyBits! !

!Form methodsFor: 'copying' stamp: 'jmv 9/23/2012 21:39'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule fillColor: aForm map: map
	"Make up a BitBlt table and copy the bits.  Use a colorMap."

	((BitBlt 
		destForm: self
		sourceForm: sourceForm
		fillColor: aForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) colorMap: map) copyBits! !

!Form methodsFor: 'copying' stamp: 'jmv 9/23/2012 21:39'!
copyBits: sourceRect from: sourceForm at: destOrigin colorMap: map 
	"Make up a BitBlt table and copy the bits with the given colorMap."
	((BitBlt 
		destForm: self
		sourceForm: sourceForm
		halftoneForm: nil
		combinationRule: Form over
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: self boundingBox) colorMap: map) copyBits! !

!Form methodsFor: 'displaying' stamp: 'jmv 9/23/2012 21:42'!
displayResourceFormOn: aForm
	"a special display method for blowing up resource thumbnails"
	self extent = aForm extent ifTrue:[^self displayOn: aForm].

	"We've got no bilinear interpolation. Use WarpBlt instead"
	(WarpBlt toForm: aForm)
		sourceForm: self destRect: aForm boundingBox;
		combinationRule: 3;
		cellSize: 2;
		warpBits! !

!Form methodsFor: 'displaying' stamp: 'jmv 9/23/2012 21:39'!
drawLine: sourceForm from: beginPoint to: endPoint clippingBox: clipRect rule: anInteger fillColor: aForm 
	"Refer to the comment in 
	DisplayMedium|drawLine:from:to:clippingBox:rule:mask:." 
	
	| dotSetter |
	"set up an instance of BitBlt for display"
	dotSetter _ BitBlt
		destForm: self
		sourceForm: sourceForm
		fillColor: aForm
		combinationRule: anInteger
		destOrigin: beginPoint
		sourceOrigin: 0 @ 0
		extent: sourceForm extent
		clipRect: clipRect.
	dotSetter drawFrom: beginPoint to: endPoint! !

!Form methodsFor: 'displaying' stamp: 'jmv 9/23/2012 21:42'!
paintBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt destForm: self
		sourceForm: sourceForm
		halftoneForm: nil
		combinationRule: 31
		destOrigin: destOrigin
		sourceOrigin: 0@0
		extent: sourceForm extent
		clipRect: self boundingBox)
		copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f _ Form fromUser. f replaceColor: f dominantColor withColor: Color transparent.
f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 paintBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

!Form methodsFor: 'filling' stamp: 'jmv 9/23/2012 21:39'!
eraseShape: bwForm
	"use bwForm as a mask to clear all pixels where bwForm has 1's"
	((BitBlt destForm: self sourceForm: bwForm 
		fillColor: nil
		combinationRule: Form erase1bitShape	"Cut a hole in the picture with my mask"
		destOrigin: bwForm offset 
		sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits.
! !

!Form methodsFor: 'filling' stamp: 'jmv 9/23/2012 21:39'!
fill: aRectangle rule: anInteger fillColor: aForm 
	"Replace a rectangular area of the receiver with the pattern described by aForm 
	according to the rule anInteger."
	(BitBlt toForm: self)
		copy: aRectangle
		from: 0@0 in: nil
		fillColor: aForm rule: anInteger! !

!Form methodsFor: 'filling' stamp: 'jmv 9/23/2012 21:39'!
fillFromXYColorBlock: colorBlock
	"General Gradient Fill.
	Supply relative x and y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| poker yRel xRel |
	poker _ BitBlt bitPokerToForm: self.
	0 to: height-1 do:
		[:y | yRel _ y asFloat / (height-1) asFloat.
		0 to: width-1 do:
			[:x |  xRel _ x asFloat / (width-1) asFloat.
			poker pixelAt: x@y
				put: ((colorBlock value: xRel value: yRel) pixelWordForDepth: self depth)]]
"
 | d |
((Form extent: 100@20 depth: Display depth)
	fillFromXYColorBlock:
	[:x :y | d _ 1.0 - (x - 0.5) abs - (y - 0.5) abs.
	Color r: d g: 0 b: 1.0-d]) display
"! !

!Form methodsFor: 'filling' stamp: 'jmv 9/23/2012 21:39'!
findShapeAroundSeedBlock: seedBlock
	"Build a shape that is black in any region marked by seedBlock. 
	SeedBlock will be supplied a form, in which to blacken various
	pixels as 'seeds'.  Then the seeds are smeared until 
	there is no change in the smear when it fills the region, ie,
	when smearing hits a black border and thus goes no further."
	| smearForm previousSmear all count smearPort |
	self depth > 1 ifTrue: [self halt]. "Only meaningful for B/W forms."
	all _ self boundingBox.
	smearForm _ Form extent: self extent.
	smearPort _ BitBlt toForm: smearForm.
	seedBlock value: smearForm.		"Blacken seeds to be smeared"
	smearPort copyForm: self to: 0@0 rule: Form erase.  "Clear any in black"
	previousSmear _ smearForm copy.
	count _ 1.
	[count = 10 and:   "check for no change every 10 smears"
		[count _ 1.
		previousSmear copy: all from: 0@0 in: smearForm rule: Form reverse.
		previousSmear isAllWhite]]
		whileFalse: 
			[smearPort copyForm: smearForm to: 1@0 rule: Form under.
			smearPort copyForm: smearForm to: -1@0 rule: Form under.
			"After horiz smear, trim around the region border"
			smearPort copyForm: self to: 0@0 rule: Form erase.
			smearPort copyForm: smearForm to: 0@1 rule: Form under.
			smearPort copyForm: smearForm to: 0@-1 rule: Form under.
			"After vert smear, trim around the region border"
			smearPort copyForm: self to: 0@0 rule: Form erase.
			count _ count+1.
			count = 9 ifTrue: "Save penultimate smear for comparison"
				[previousSmear copy: all from: 0@0 in: smearForm rule: Form over]].
	"Now paint the filled region in me with aHalftone"
	^ smearForm! !

!Form methodsFor: 'filling' stamp: 'jmv 9/23/2012 21:42'!
floodFill2: aColor at: interiorPoint
	"Fill the shape (4-connected) at interiorPoint.  The algorithm is based on Paul Heckbert's 'A Seed Fill Algorithm', Graphic Gems I, Academic Press, 1990.
	NOTE: This is a less optimized variant for flood filling which is precisely along the lines of Heckbert's algorithm. For almost all cases #floodFill:at: will be faster (see the comment there) but this method is left in both as reference and as a fallback if such a strange case is encountered in reality."
	| peeker poker stack old new x y top x1 x2 dy left goRight |
	peeker _ BitBlt bitPeekerFromForm: self.
	poker _ BitBlt bitPokerToForm: self.
	stack _ OrderedCollection new: 50.
	"read old pixel value"
	old _ peeker pixelAt: interiorPoint.
	"compute new value"
	new _ self pixelValueFor: aColor.
	old = new ifTrue:[^self]. "no point, is there?!!"

	x _ interiorPoint x.
	y _ interiorPoint y.
	(y >= 0 and:[y < height]) ifTrue:[
		stack addLast: {y. x. x. 1}. "y, left, right, dy"
		stack addLast: {y+1. x. x. -1}].
	[stack isEmpty] whileFalse:[
		top _ stack removeLast.
		y _ top at: 1. x1 _ top at: 2. x2 _ top at: 3. dy _ top at: 4.
		y _ y + dy.
		"Segment of scanline (y-dy) for x1 <= x <= x2 was previously filled.
		Now explore adjacent pixels in scanline y."
		x _ x1.
		[x >= 0 and:[(peeker pixelAt: x@y) = old]] whileTrue:[
			poker pixelAt: x@y put: new.
			x _ x - 1].
		goRight _ x < x1.
		left _ x+1.
		(left < x1 and:[y-dy >= 0 and:[y-dy < height]]) 
			ifTrue:[stack addLast: {y. left. x1-1. 0-dy}].
		goRight ifTrue:[x _ x1 + 1].
		[
			goRight ifTrue:[
				[x < width and:[(peeker pixelAt: x@y) = old]] whileTrue:[
					poker pixelAt: x@y put: new.
					x _ x + 1].
				(y+dy >= 0 and:[y+dy < height]) 
					ifTrue:[stack addLast: {y. left. x-1. dy}].
				(x > (x2+1) and:[y-dy >= 0 and:[y-dy >= 0]]) 
					ifTrue:[stack addLast: {y. x2+1. x-1. 0-dy}]].
			[(x _ x + 1) <= x2 and:[(peeker pixelAt: x@y) ~= old]] whileTrue.
			left _ x.
			goRight _ true.
		x <= x2] whileTrue.
	].
! !

!Form methodsFor: 'image manipulation' stamp: 'jmv 9/23/2012 21:43'!
replaceColor: oldColor withColor: newColor
	"Replace one color with another everywhere is this form"

	| cm newInd target ff |
	self depth = 32
		ifTrue: [cm _ (Color  cachedColormapFrom: 16 to: 32) copy]
		ifFalse: [cm _ Bitmap new: (1 bitShift: (self depth min: 15)).
				1 to: cm size do: [:i | cm at: i put: i - 1]].
	newInd _ newColor pixelValueForDepth: self depth.
	cm at: (oldColor pixelValueForDepth: (self depth min: 16))+1 put: newInd.
	target _ newColor isTransparent 
		ifTrue: [ff _ Form extent: self extent depth: depth.
			ff fillWithColor: newColor.  ff]
		ifFalse: [self].
	(BitBlt toForm: target)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form paint;
		destX: 0 destY: 0 width: width height: height;
		colorMap: cm;
		copyBits.
	newColor = Color transparent 
		ifTrue: [target displayOn: self].! !

!Form methodsFor: 'image manipulation' stamp: 'jmv 9/23/2012 21:43'!
smear: dir distance: dist
	"Smear any black pixels in this form in the direction dir in Log N steps"
	| skew bb |
	bb _ BitBlt destForm: self sourceForm: self fillColor: nil
		combinationRule: Form under destOrigin: 0@0 sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox.
	skew _ 1.
	[skew < dist] whileTrue:
		[bb destOrigin: dir*skew; copyBits.
		skew _ skew+skew]! !

!Form methodsFor: 'pixel access' stamp: 'jmv 9/23/2012 21:43'!
pixelValueAt: aPoint 
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color.  "

	^ (BitBlt bitPeekerFromForm: self) pixelAt: aPoint
! !

!Form methodsFor: 'pixel access' stamp: 'jmv 9/23/2012 21:43'!
pixelValueAt: aPoint put: pixelValue
	"Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color. "

	(BitBlt bitPokerToForm: self) pixelAt: aPoint put: pixelValue.
! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 9/23/2012 21:42'!
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
	newForm offset: (self offset flippedBy: direction centerAt: aPoint).
	^ newForm
"
[Sensor anyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p | [Sensor anyButtonPressed] whileFalse:
	[f _ Form fromDisplay: ((p _ Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 _ f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p]
"
! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 9/23/2012 21:42'!
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
[Sensor anyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) by: 0.5 smoothing: 2) display]
"
"Scaling test...
| f cp | f _ Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor anyButtonPressed] whileFalse:
        [cp _ Sensor mousePoint.
        (f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent smoothing: 2) display]]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 9/23/2012 21:42'!
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
[Sensor anyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) to: 300@200 smoothing: 2) display]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 9/23/2012 21:43'!
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
[Sensor anyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotateBy: (a _ a+5) smoothing: 2) display].
f display
"! !

!Form methodsFor: 'transitions' stamp: 'jmv 9/23/2012 21:42'!
pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d |
	pageRect _ otherImage boundingBox.
	oldPage _ self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: Color black.
	oldBottom _ self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps _ 8.
	buffer _ Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d _ pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect _ pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad _ Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp _ (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceDisplayUpdate].

	buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceDisplayUpdate.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"
! !


!ColorForm methodsFor: 'pixel accessing' stamp: 'jmv 9/23/2012 21:38'!
pixelValueAt: aPoint 
	"Return the raw pixel value at the given point. Typical clients use colorAt: to get a Color."
	"Details: To get the raw pixel value, be sure the peeker's colorMap is nil."

	^ (BitBlt bitPeekerFromForm: self) colorMap: nil; pixelAt: aPoint
! !

!ColorForm methodsFor: 'color manipulation' stamp: 'jmv 9/23/2012 21:38'!
twoToneFromDisplay: aRectangle backgroundColor: bgColor
	"Copy one-bit deep ColorForm from the Display using a color map that maps all colors except the background color to black. Used for caching the contents of inactive MVC windows."

	| map |
	(width = aRectangle width and: [height = aRectangle height])
		ifFalse: [self setExtent: aRectangle extent depth: depth].

	"make a color map mapping the background color
	 to zero and all other colors to one"
	map _ Bitmap new: (1 bitShift: (Display depth min: 9)).
	1 to: map size do: [:i | map at: i put: 16rFFFFFFFF].
	map at: (bgColor indexInMap: map) put: 0.

	(BitBlt toForm: self)
		destOrigin: 0@0;
		sourceForm: Display;
		sourceRect: aRectangle;
		combinationRule: Form over;
		colorMap: map;
		copyBits.
! !

!ColorForm methodsFor: 'copying' stamp: 'jmv 9/23/2012 21:38'!
copy: aRect
 	"Return a new ColorForm containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		fillColor: nil
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	colors ifNotNil: [newForm colors: colors copy].
	^ newForm
! !


!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 9/23/2012 21:38'!
copyBits: rect from: sf at: destOrigin clippingBox: clipRect rule: cr fillColor: hf 
	(BitBlt
		destForm: self
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: rect origin
		extent: rect extent
		clipRect: (clipRect intersect: clippingBox)) copyBits! !

!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 9/23/2012 21:38'!
copyBits: rect from: sf at: destOrigin clippingBox: clipRect rule: cr fillColor: hf map: map
	((BitBlt
		destForm: self
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: rect origin
		extent: rect extent
		clipRect: (clipRect intersect: clippingBox)) colorMap: map) copyBits! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 9/23/2012 21:44'!
dotOfSize: diameter
	"Create a form which contains a round black dot."
	| radius form bb rect centerX centerY centerYBias centerXBias radiusSquared xOverY maxy dx |
	radius _ diameter//2.
	form _ self extent: diameter@diameter offset: (0@0) - (radius@radius).	
	bb _ (BitBlt toForm: form)
		sourceX: 0; sourceY: 0;
		combinationRule: Form over;
		fillColor: Color black.
	rect _ form boundingBox.
	centerX _ rect center x.
	centerY _ rect center y.
	centerYBias _ rect height odd ifTrue: [0] ifFalse: [1].
	centerXBias _ rect width odd ifTrue: [0] ifFalse: [1].
	radiusSquared _ (rect height asFloat / 2.0) squared - 0.01.
	xOverY _ rect width asFloat / rect height asFloat.
	maxy _ rect height - 1 // 2.

	"First do the inner fill, and collect x values"
	0 to: maxy do:
		[:dy |
		dx _ ((radiusSquared - (dy * dy) asFloat) sqrt * xOverY) truncated.
		bb	destX: centerX - centerXBias - dx
			destY: centerY - centerYBias - dy
			width: dx + dx + centerXBias + 1
			height: 1;
			copyBits.
		bb	destY: centerY + dy;
			copyBits].
	^ form
"
Time millisecondsToRun:
	[1 to: 20 do: [:i | (Form dotOfSize: i) displayAt: (i*20)@(i*20)]]
"! !

!Form class methodsFor: 'examples' stamp: 'jmv 9/23/2012 21:44'!
toothpaste: diam		"Display restoreAfter: [Form toothpaste: 30]"
	"Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| facade ball filter point queue port color q colors colr colr2 |
	colors _ Display depth = 1
		ifTrue: [Array with: Color black]
		ifFalse: [Color red wheel: 12].
	facade _ Form extent: diam@diam offset: (diam//-2) asPoint.
	(Form dotOfSize: diam) displayOn: facade
			at: (diam//2) asPoint clippingBox: facade boundingBox
			rule: Form under fillColor: Color white.
	#(1 2 3) do:
		[:x |  "simulate facade by circles of gray"
		(Form dotOfSize: x*diam//5) displayOn: facade
			at: (diam*2//5) asPoint clippingBox: facade boundingBox
			rule: Form under
			fillColor: (Color perform: 
					(#(black gray lightGray) at: x)).
		"facade displayAt: 50*x@50"].
	ball _ Form dotOfSize: diam.
	color _ 8.
	[ true ] whileTrue:
		[port _ BitBlt toForm: Display.
		"Expand 1-bit forms to any pixel depth"
		port colorMap: (Bitmap with: 0 with: 16rFFFFFFFF).
		queue _ OrderedCollection new: 32.
		16 timesRepeat: [queue addLast: -20@-20].
		Sensor waitButton.
		Sensor mouseButton2Pressed ifTrue: [^ self].
		filter _ Sensor mousePoint.
		colr _ colors atWrap: (color _ color + 5).  "choose increment relatively prime to colors size"
		colr2 _ colr alphaMixed: 0.3 with: Color white.
		[Sensor mouseButton1Pressed or: [queue size > 0]] whileTrue:
			[filter _ filter * 4 + Sensor mousePoint // 5.
			point _ Sensor mouseButton1Pressed
				ifTrue: [filter] ifFalse: [-20@-20].
			port copyForm: ball to: point rule: Form paint fillColor: colr.
			(q _ queue removeFirst) ifNil: [^ self].	"exit"
			Display depth = 1
				ifTrue: [port copyForm: facade to: q rule: Form erase]
				ifFalse: [port copyForm: facade to: q rule: Form paint fillColor: colr2].
			Sensor mouseButton1Pressed ifTrue: [queue addLast: point]]].
! !


!FormCanvas methodsFor: 'other' stamp: 'jmv 9/23/2012 21:44'!
showAt: pt invalidRects: updateRects
	| blt |
	blt _ (BitBlt toForm: Display)
		sourceForm: form;
		combinationRule: Form over.
	updateRects do:
		[:rect |
		blt sourceRect: rect;
			destOrigin: rect topLeft + pt;
			copyBits]! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/23/2012 21:48'!
resetGrafPort
	"Private!! Create a new grafPort for a new copy."

	port _ GrafPort toForm: form.
	port clipRect: clipRect.
! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/23/2012 21:48'!
setForm: aForm

	form _ aForm.
	port _ GrafPort toForm: form.

	"this was the contents of the #reset method"

	"origin of the top-left corner of this cavas"
	transformations first setTranslation: 0@0.
	clipRect _ (0@0 corner: form extent).	"default clipping rectangle"
	shadowColor _ nil! !


!InfiniteForm methodsFor: 'displaying' stamp: 'jmv 9/23/2012 21:45'!
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm
	"This is the real display message, but it doesn't get used until the new
	display protocol is installed."
	| targetBox patternBox bb |
	(patternForm is: #Form) ifFalse: [
		^ aDisplayMedium fill: clipRectangle rule: ruleInteger fillColor: patternForm].

	"Do it iteratively"
	targetBox _ aDisplayMedium boundingBox intersect: clipRectangle.
	patternBox _ patternForm boundingBox.
	bb _ BitBlt destForm: aDisplayMedium sourceForm: patternForm fillColor: aForm
		combinationRule: ruleInteger destOrigin: 0@0 sourceOrigin: 0@0
		extent: patternBox extent clipRect: clipRectangle.
	bb colorMap:
		(patternForm colormapIfNeededFor: aDisplayMedium).
	(targetBox left truncateTo: patternBox width)
		to: targetBox right - 1 by: patternBox width do:
		[:x |
		(targetBox top truncateTo: patternBox height)
			to: targetBox bottom - 1 by: patternBox height do:
			[:y |
			bb destOrigin: x@y; copyBits]]! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/23/2012 21:30'!
imageForm: depth forRectangle: rect
	| canvas |
	canvas _ FormCanvas depth: depth over: rect.
	canvas fullDraw: self.
	^ canvas formWithOffset! !

!Morph methodsFor: 'drawing' stamp: 'jmv 9/23/2012 21:30'!
ownShadowForm
	"Return a form representing the 'shadow' of the receiver, without including submorphs 
	regardless of clipping"
	| canvas |
	canvas _ FormCanvas forShadowOver: self morphBoundsInWorld.
	canvas into: self.
	canvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ].
	^ canvas form! !


!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:45'!
copyPixelsGray: y
	"Handle non-interlaced grayscale color mode (colorType = 0)"

	bitsPerChannel = 16 ifTrue: [
		"Warning: This is extremely slow. Besides we are downsampling to 8 bits!!"
		| blitter |
		blitter := BitBlt bitPokerToForm: form.
		0 to: width - 1 do: [ :x |
			blitter pixelAt: x @ y put: 255 - (thisScanline at: x * 2 + 1) ].
			^self ].

	"Just copy the bits"

	"This interesting technique (By Andreas Raab) is a bit obscure, but it is so fast that we leave it active"
	^self copyPixelsGrayWeirdBitBltHack: y.

	"This interesting technique  (By Yoshiki Ohshima) is also instructive"
	"true ifTrue: [ ^form bits copyFromByteArray2: thisScanline to: y * (form width* bitsPerChannel // 32) ]."

	"This Smalltalk version might be easier to understand and is quite fast too."
	"This somewhat weird mixture of (#* and #+) with (#bitShift: and #bitOr:) 
	is to make use of faster arithmetic bytecodes, but not of slow largeintegers."
	"
	base _ y * (form width * bitsPerChannel + 31 // 32) + 1.
	bits _ form bits.
	0 to: thisScanline size // 4 - 1 do: [ :i |
		| ii |
		ii _ i * 4.
		word _
	           ((thisScanline at: ii+1) *256 + 
	           (thisScanline at: ii+2) *256 + 
	           ((thisScanline at: ii+3)) bitShift: 8) bitOr: 
	           (thisScanline at: ii+4).
		bits at: base + i put: word.].
	(bytesLeft := thisScanline size bitAnd: 3) = 0 ifFalse: [
		word := 0.
		thisScanline size - bytesLeft + 1 to: thisScanline size do: [ :ii |
			word := word * 256 + (thisScanline at: ii) ].
		word := word bitShift: 8 * (4 - bytesLeft).
		bits at: base + (thisScanline size // 4) put: word ].
	"! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:46'!
copyPixelsGray: y at: startX by: incX
	"Handle interlaced grayscale color mode (colorType = 0)"

	| offset bits blitter pixPerByte shifts b pixel mask pixelNumber |
	bitsPerChannel = 16
		ifTrue: [
			b := BitBlt bitPokerToForm: form.
			startX to: width-1 by: incX do: [ :x |
				b pixelAt: x@y put: 255 - (thisScanline at: (x//incX<<1)+1).
				].
			^ self
			].
	offset := y*rowSize+1.
	bits := form bits.
	bitsPerChannel = 8 ifTrue: [
		startX to: width-1 by: incX do: [ :x | | w |
			w := offset + (x>>2).
			b := 3- (x \\ 4) * 8.
			pixel := (thisScanline at: x // incX + 1)<<b.
			mask := (255<<b) bitInvert32.
			bits at: w put: (((bits at: w) bitAnd: mask) bitOr: pixel)
		].
		^ self
	].
	bitsPerChannel = 1 ifTrue: [
		pixPerByte := 8.
		mask := 1.
		shifts := #(7 6 5 4 3 2 1 0).
	].
	bitsPerChannel = 2 ifTrue: [
		pixPerByte := 4.
		mask := 3.
		shifts := #(6 4 2 0).
	].
	bitsPerChannel = 4 ifTrue: [
		pixPerByte := 2.
		mask := 15.
		shifts := #(4 0).
	].

	blitter := BitBlt bitPokerToForm: form.
	pixelNumber := 0.
	startX to: width-1 by: incX do: [ :x | | rawByte |
		rawByte := thisScanline at: (pixelNumber // pixPerByte) + 1.
		pixel := (rawByte >> (shifts at: (pixelNumber \\ pixPerByte) + 1)) bitAnd: mask.
		blitter pixelAt: (x@y) put: pixel.
		pixelNumber := pixelNumber + 1.
	].
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:45'!
copyPixelsGrayAlpha: y
	"Handle non-interlaced grayscale with alpha color mode (colorType = 4)"

	| i pixel gray b |
	b _ BitBlt bitPokerToForm: form.
	bitsPerChannel = 8
		ifTrue: [
			0 to: width-1 do: [ :x |
				i _ (x << 1) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+1)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
		ifFalse: [
			0 to: width-1 do: [ :x |
				i _ (x << 2) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+2)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:45'!
copyPixelsGrayAlpha: y at: startX by: incX
	"Handle interlaced grayscale with alpha color mode (colorType = 4)"

	| i pixel gray b |
	b _ BitBlt bitPokerToForm: form.
	bitsPerChannel = 8
		ifTrue: [
			startX to: width-1 by: incX do: [ :x |
				i _ (x // incX << 1) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+1)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
		ifFalse: [
			startX to: width-1 by: incX do: [ :x |
				i _ (x // incX << 2) + 1.
				gray _ thisScanline at: i.
				pixel _ ((thisScanline at: i+2)<<24) + (gray<<16) + (gray<<8) + gray.
				b pixelAt: x@y put: pixel.
				]
			]
! !

!PNGReadWriter methodsFor: 'pixel copies' stamp: 'jmv 9/23/2012 21:45'!
copyPixelsIndexed: y at: startX by: incX
	"Handle interlaced indexed color mode (colorType = 3)"

	| offset bits pixPerByte shifts blitter pixel mask pixelNumber |
	offset := y*rowSize+1.
	bits := form bits.
	bitsPerChannel = 8
		ifTrue: [
			startX to: width-1 by: incX do: [ :x | | b w |
				w := offset + (x>>2).
				b := 3 - (x \\ 4) * 8.
				pixel := (thisScanline at: x // incX + 1)<<b.
				mask := (255<<b) bitInvert32.
				bits at: w put: (((bits at: w) bitAnd: mask) bitOr: pixel)].
			^ self ].
	bitsPerChannel = 1 ifTrue: [
		pixPerByte := 8.
		mask := 1.
		shifts := #(7 6 5 4 3 2 1 0).
	].
	bitsPerChannel = 2 ifTrue: [
		pixPerByte := 4.
		mask := 3.
		shifts := #(6 4 2 0).
	].
	bitsPerChannel = 4 ifTrue: [
		pixPerByte := 2.
		mask := 15.
		shifts := #(4 0).
	].

	blitter := BitBlt bitPokerToForm: form.
	pixelNumber := 0.
	startX to: width-1 by: incX do: [ :x | | rawByte |
		rawByte := thisScanline at: (pixelNumber // pixPerByte) + 1.
		pixel := (rawByte >> (shifts at: (pixelNumber \\ pixPerByte) + 1)) bitAnd: mask.
		blitter pixelAt: (x@y) put: pixel.
		pixelNumber := pixelNumber + 1.
	].
! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 9/23/2012 21:46'!
nextWordsInto: aBitmap 
	"Fill the word based buffer from my collection. 
	Stored on stream as Big Endian. Optimized for speed. 
	Read in BigEndian, then restoreEndianness."
	| blt pos source byteSize |
	collection class isBytes
		ifFalse: [^ self next: aBitmap size into: aBitmap startingAt: 1].

	byteSize := aBitmap byteSize.
	"is the test on collection basicSize \\ 4 necessary?"
	((self position bitAnd: 3) = 0 and: [ (collection basicSize bitAnd: 3) = 0])
		ifTrue: [source := collection.
			pos := self position.
			self skip: byteSize]
		ifFalse: ["forced to copy it into a buffer"
			source := self next: byteSize.
			pos := 0].

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt
				toForm: (Form new hackBits: aBitmap))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.

	"And do whatever the bitmap needs to do to convert from big-endian order."
	aBitmap restoreEndianness.

	^ aBitmap 	"May be WordArray, ColorArray, etc"
! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/23/2012 21:30'!
submorphsShadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ FormCanvas forShadowOver: bnds.
	canvas into: self.
	self drawSubmorphsOn: canvas.
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !


!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 9/23/2012 21:45'!
magnifiedForm
	| srcRect form neededExtent |
	lastPos _ self sourcePoint.
	srcRect _ self sourceRectFrom: lastPos.
	((srcRect intersects: self morphBoundsInWorld) and: [ RecursionLock == nil ])
		ifTrue: [
			RecursionLock _ self.
			"try to reuse form if appropriate"
			auxCanvas _ (auxCanvas notNil and: [ auxCanvas extent = srcExtent ])
				ifTrue: [
					"Just in case we go out of the Display"
					srcRect origin > (0@0) ifFalse: [
						auxCanvas form fillBlack ].
					FormCanvas on: auxCanvas form over: srcRect ]
				ifFalse: [ FormCanvas depth: 32 over: srcRect ].
			World drawOn: auxCanvas.
			World drawSubmorphsOn: auxCanvas.
			form _ auxCanvas form.
			RecursionLock _ nil]
		ifFalse: [
			"cheaper method if the source is not occluded"
			form _ Display copy: srcRect].
	"smooth if non-integer scale"
	neededExtent _ (srcExtent * magnification ) truncated.
	(magnifiedForm isNil or: [ magnifiedForm extent ~=  neededExtent ])
		ifTrue: [ magnifiedForm _ Form extent: neededExtent depth: 32 ].
	(WarpBlt toForm: magnifiedForm)
		sourceForm: form;
		colorMap: (form colormapIfNeededFor: magnifiedForm);
		cellSize: (magnification isInteger ifTrue: [1] ifFalse: [2]);  "installs a new colormap if cellSize > 1"
		combinationRule: 3;
		copyQuad: form boundingBox innerCorners toRect: magnifiedForm boundingBox.
	^magnifiedForm.! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 9/23/2012 21:45'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			"self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage multipliedBy: color at: bounds topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage multipliedBy: color at: bounds topLeft ]"
			self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBoundsInWorld
					during: [ :canvas | canvas image: backgroundImage at: 0@0 ]]
				ifFalse: [ aCanvas image: backgroundImage at: 0@0 ]]

		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [aCanvas drawsOnDisplay] and: [color class == TranslucentColor])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Squeak Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 9/23/2012 21:45'!
flashRects: rectangleList
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."
	"Details: Uses two reverses so that the display is restored to its original state. This is necessary when in deferred update mode."

	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		combinationRule: Form reverse.
	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect; forceDisplayUpdate.
		(Delay forMilliseconds: 250) wait.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect; forceDisplayUpdate].
! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 9/23/2012 21:46'!
bonk: glyphForm with: bonkForm
	"Bonking means to run through the glyphs clearing out black pixels
	between characters to prevent them from straying into an adjacent
	character as a result of, eg, bolding or italicizing"
	"Uses the bonkForm to erase at every character boundary in glyphs."
	| bb offset |
	offset _ bonkForm offset x.
	bb _ BitBlt toForm: glyphForm.
	bb sourceForm: bonkForm; sourceRect: bonkForm boundingBox;
		combinationRule: Form erase; destY: 0.
	1 to: xTable size-1 do: [:i | bb destX: (xTable at: i) + offset; copyBits].
! !


!WarpBlt methodsFor: 'primitives' stamp: 'jmv 9/23/2012 21:46'!
warpBitsSmoothing: n sourceMap: sourceMap
	| deltaP12 deltaP43 pA pB deltaPAB sp fixedPtOne picker poker pix nSteps |
	<primitive: 'primitiveWarpBits' module: 'BitBltPlugin'>

	"Check for compressed source, destination or halftone forms"
	((sourceForm is: #Form) and: [sourceForm unhibernate])
		ifTrue: [^ self warpBitsSmoothing: n sourceMap: sourceMap].
	((destForm is: #Form) and: [destForm unhibernate])
		ifTrue: [^ self warpBitsSmoothing: n sourceMap: sourceMap].
	((halftoneForm is: #Form) and: [halftoneForm unhibernate])
		ifTrue: [^ self warpBitsSmoothing: n sourceMap: sourceMap].

	(width < 1) | (height < 1) ifTrue: [^ self].
	fixedPtOne _ 16384.  "1.0 in fixed-pt representation"
	n > 1 ifTrue:
		[(destForm depth < 16 and: [colorMap == nil])
			ifTrue: ["color map is required to smooth non-RGB dest"
					^ self primitiveFail].
		pix _ Array new: n*n].

	nSteps _ height-1 max: 1.
	deltaP12 _ (self deltaFrom: p1x to: p2x nSteps: nSteps)
			@ (self deltaFrom: p1y to: p2y nSteps: nSteps).
	pA _ (self startFrom: p1x to: p2x offset: nSteps*deltaP12 x)
		@ (self startFrom: p1y to: p2y offset: nSteps*deltaP12 y).
	deltaP43 _ (self deltaFrom: p4x to: p3x nSteps: nSteps)
			@ (self deltaFrom: p4y to: p3y nSteps: nSteps).
	pB _ (self startFrom: p4x to: p3x offset: nSteps*deltaP43 x)
		@ (self startFrom: p4y to: p3y offset: nSteps*deltaP43 y).

	picker _ BitBlt bitPeekerFromForm: sourceForm.
	poker _ BitBlt bitPokerToForm: destForm.
	poker clipRect: self clipRect.
	nSteps _ width-1 max: 1.
	destY to: destY+height-1 do:
		[:y |
		deltaPAB _ (self deltaFrom: pA x to: pB x nSteps: nSteps)
				@ (self deltaFrom: pA y to: pB y nSteps: nSteps).
		sp _ (self startFrom: pA x to: pB x offset: nSteps*deltaPAB x)
			@ (self startFrom: pA y to: pB y offset: nSteps*deltaPAB x).
		destX to: destX+width-1 do:
			[:x | 
			n = 1
			ifTrue:
				[poker pixelAt: x@y
						put: (picker pixelAt: sp // fixedPtOne asPoint)]
			ifFalse:
				[0 to: n-1 do:
					[:dx | 0 to: n-1 do:
						[:dy |
						pix at: dx*n+dy+1 put:
								(picker pixelAt: sp
									+ (deltaPAB*dx//n)
									+ (deltaP12*dy//n)
										// fixedPtOne asPoint)]].
				poker pixelAt: x@y put: (self mixPix: pix
										sourceMap: sourceMap
										destMap: colorMap)].
			sp _ sp + deltaPAB].
		pA _ pA + deltaP12.
		pB _ pB + deltaP43]! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 9/23/2012 21:31'!
assuredNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= viewBox extent) or: [
		canvas form depth ~= Display depth]]])
			ifTrue: [
				"allocate a new offscreen canvas the size of the window"
				self canvas: (FormCanvas withExtent: viewBox extent depth: Display depth)].
	^ self canvas! !

!methodRemoval: WarpBlt class #current!
WarpBlt class removeSelector: #current!
!methodRemoval: ProcessorScheduler class #sweepHandIdleProcess!
ProcessorScheduler class removeSelector: #sweepHandIdleProcess!
!methodRemoval: FormCanvas #portClass!
FormCanvas removeSelector: #portClass!
!methodRemoval: DisplayScreen #defaultBitBltClass!
DisplayScreen removeSelector: #defaultBitBltClass!
!methodRemoval: DisplayScreen #defaultCanvasClass!
DisplayScreen removeSelector: #defaultCanvasClass!
!methodRemoval: DisplayScreen #defaultWarpBltClass!
DisplayScreen removeSelector: #defaultWarpBltClass!
!methodRemoval: Form #bytesPerRow!
Form removeSelector: #bytesPerRow!
!methodRemoval: Form #defaultCanvasClass!
Form removeSelector: #defaultCanvasClass!
!methodRemoval: BitBlt class #asGrafPort!
BitBlt class removeSelector: #asGrafPort!
!methodRemoval: BitBlt class #current!
BitBlt class removeSelector: #current!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk removeKey: #ActiveEvent!

