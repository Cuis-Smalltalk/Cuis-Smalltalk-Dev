'From Cuis 5.0 [latest update: #4047] on 3 March 2020 at 4:04:48 am'!

!Color class methodsFor: 'color from user' stamp: 'pb 3/3/2020 03:54:12'!
colorPaletteForDepth: depth extent: chartExtent
	"Display a palette of colors sorted horizontally by hue and vertically by lightness. Useful for eyeballing the color gamut of the display, or for choosing a color interactively."
	"Note: It is slow to build this palette, so it should be cached for quick access."
	"(Color colorPaletteForDepth: 16 extent: 190@60) display"

	| basicHue x y startHue palette transHt vSteps transCaption grayWidth hSteps |
	palette _ Form extent: chartExtent depth: depth.
	transCaption _ 
		(Form extent: 34@9 depth: 1
			fromArray: #(0 0 256 0 256 0 3808663859 2147483648 2491688266 2147483648 2491688266 0 2491688266 0 2466486578 0 0 0)
			offset: `0@0`).
	transHt _ transCaption height.
	palette fillWhite: (`0@0` extent: palette width@transHt).
	palette fillBlack: (0@transHt extent: palette width@1).
	transCaption displayOn: palette at: palette boundingBox topCenter - ((transCaption width // 2)@0).
	grayWidth _ 10.
	startHue _ 338.0.
	vSteps _ palette height - transHt // 2.
	hSteps _ palette width - grayWidth.
	x _ 0.
	startHue to: startHue + 360.0 by: 360.0/hSteps do: [ :h |
		basicHue _ Color h: h asFloat s: 1.0 v: 1.0.
		y _ transHt+1.
		0 to: vSteps do: [ :n | | c |
 			c _ basicHue mixed: (n asFloat / vSteps asFloat) with: Color white.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y _ y + 1].
		1 to: vSteps do: [ :n | | c |
 			c _ Color black mixed: (n asFloat / vSteps asFloat) with: basicHue.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y _ y + 1].
		x _ x + 1].
	y _ transHt + 1.
	1 to: vSteps * 2 do: [ :n | | c |
 		c _ Color black mixed: (n asFloat / (vSteps*2) asFloat) with: Color white.
		palette fill: (x@y extent: 10@1) fillColor: c.
		y _ y + 1].
	^ palette
! !


!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:00:30'!
bottomLeftCursor
	^ self
		extent: `16 @ 16`
		fromArray: #(49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 65532 65532 0 0 )
		offset: `0 @ -16`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:00:40'!
bottomRightCursor
	^ self
		extent: `16 @ 16`
		fromArray: #(12 12 12 12 12 12 12 12 12 12 12 12 65532 65532 0 0 )
		offset: `-16 @ -16`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:00:50'!
cornerCursor
	"Answer the instance of me that is the shape of the bottom right corner 
	of a rectangle."
	^ self
		extent: `16 @ 16`
		fromArray: #(12 12 12 12 12 12 12 12 12 12 12 12 65532 65532 0 0 )
		offset: `-16 @ -16`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:01:07'!
crossHairCursor
	"Answer the instance of me that is the shape of a cross."
	^ self
		extent: `16 @ 16`
		fromArray: #(0 256 256 256 256 256 256 32764 256 256 256 256 256 256 0 0 )
		offset: `-7 @ -7`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:01:13'!
downCursor
	"Answer the instance of me that is the shape of an arrow facing 
	downward."
	^ self
		extent: `16 @ 16`
		fromArray: #(12288 12288 12288 12288 12288 12288 12288 64512 30720 12288 0 0 0 0 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:01:19'!
executeCursor
	"Answer the instance of me that is the shape of an arrow slanted left 
	with a star next to it."
	^ self
		extent: `16 @ 16`
		fromArray: #(32800 49184 57456 62462 63884 64648 65272 61656 55692 39172 3072 3072 1536 1536 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:01:27'!
extent: extentPoint fromArray: anArray offset: offsetPoint 
	"Answer a new instance of me with width and height specified by
	extentPoint, offset by offsetPoint, and bits from anArray.
	NOTE: This has been kluged to take an array of 16-bit constants,
	and shift them over so they are left-justified in a 32-bit bitmap"

	extentPoint = (`16 @ 16`)
		ifTrue: 
			[^ super
				extent: extentPoint
				fromArray: (anArray collect: [:bits | bits bitShift: 16])
				offset: offsetPoint]
		ifFalse: [self error: 'cursors must be 16@16']! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:01:32'!
markerCursor
	"Answer the instance of me that is the shape of a small ball."
	^ self
		extent: `16 @ 16`
		fromArray: #(28672 63488 63488 28672 0 0 0 0 0 0 0 0 0 0 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:01:37'!
menuCursor
	"Answer the instance of me that is the shape of a menu."
	^ self
		extent: `16 @ 16`
		fromArray: #(65504 32800 42528 32800 54112 65504 32800 45728 32800 44192 32800 42272 32800 65504 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:01:44'!
moveCursor
	"Answer the instance of me that is the shape of a cross inside a square."
	^ self
		extent: `16 @ 16`
		fromArray: #(65532 65532 49932 49932 49932 49932 65532 65532 49932 49932 49932 49932 65532 65532 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:48:43'!
new

	^ self extent: `16 @ 16`
		fromArray: (Array new: 16 withAll: 0)
		offset: `0 @ 0`

	"Cursor new bitEdit show"! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:48:52'!
normalCursor
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ self
		extent: `16 @ 16`
		fromArray: #(32768 49152 57344 61440 63488 64512 65024 63488 63488 38912 3072 3072 1536 1536 768 768 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:49:10'!
normalCursorWithMask
	"Cursor initNormalWithMask.  Cursor normal show"
	"Next two lines work simply for any cursor..."
	"
	self initNormal.
	NormalCursor _ CursorWithMask derivedFrom: NormalCursor.
	"
	"But for a good looking cursor, you have to tweak things..."
	^ (CursorWithMask
		extent: `16 @ 16`
		depth: 1
		fromArray: #(0 1073741824 1610612736 1879048192 2013265920 2080374784 2113929216 2130706432 2139095040 2080374784 1811939328 1174405120 100663296 50331648 50331648 0 )
		offset: -1 @ -1) setMaskForm:
		(Form
			extent: `16 @ 16`
			depth: 1
			fromArray: #(3221225472 3758096384 4026531840 4160749568 4227858432 4261412864 4278190080 4286578688 4290772992 4292870144 4261412864 4009754624 3472883712 2273312768 125829120 58720256 )
			offset: `0 @ 0`).! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:49:18'!
originCursor
	"Answer the instance of me that is the shape of the top left corner of a 
	rectangle."
	^ self
		extent: `16 @ 16`
		fromArray: #(65532 65532 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:49:28'!
readCursor
	"Answer the instance of me that is the shape of eyeglasses."
	^ self
		extent: `16 @ 16`
		fromArray: #(0 0 4104 10260 16416 64480 33824 33824 46496 31680 0 0 0 0 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:02:13'!
resizeLeftCursor
	^ (self
		extent: `16 @ 16`
		fromArray: #(0 1152 1152 1152 5280 13488 29880 64764 29880 13488 5280 1152 1152 1152 0 0 )
		offset: `-7 @ -7`) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:02:22'!
resizeTopCursor
	^ (self
		extent: `16 @ 16`
		fromArray: #(256 896 1984 4064 256 32764 0 0 32764 256 4064 1984 896 256 0 )
		offset: `-7 @ -7`) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:02:30'!
resizeTopLeftCursor
	^ (self
		extent: `16 @ 16`
		fromArray: #(0 31760 30752 28740 26760 17680 544 1088 2176 4420 8748 1052 2108 124 0 0 )
		offset: `-7 @ -7`) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:02:37'!
resizeTopRightCursor
	^ (self
		extent: `16 @ 16`
		fromArray: #(0 4220 2108 17436 8748 4420 2176 1088 544 17680 26760 28736 30752 31744 0 0 )
		offset: `-7 @ -7`) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:49:38'!
rightArrowCursor 
	"Answer the instance of me that is the shape of an arrow pointing to the right."
	^ self
		extent: `16 @ 16`
		fromArray: #(1536 1920 2016 65528 2016 1920 1536 0 0 0 0 0 0 0 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:02:48'!
squareCursor
	"Answer the instance of me that is the shape of a square."
	^ self
		extent: `16 @ 16`
		fromArray: #(0 0 0 0 0 960 960 960 960 0 0 0 0 0 0 0 )
		offset: `-8 @ -8`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:02:56'!
targetCursor
	"Answer the instance of me that is the shape of a gunsight."
	^ self
		extent: `16 @ 16`
		fromArray: #(1984 6448 8456 16644 17284 33026 35106 65278 35106 33026 17284 16644 8456 6448 1984 0 )
		offset: `-7 @ -7`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:49:50'!
topLeftCursor
	^ self
		extent: `16 @ 16`
		fromArray: #(65532 65532 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 04:03:08'!
topRightCursor
	^ self
		extent: `16 @ 16`
		fromArray: #(65532 65532 12 12 12 12 12 12 12 12 12 12 12 12 0 0 )
		offset: `-16 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:49:59'!
upCursor
	"Answer the instance of me that is the shape of an arrow facing upward."
	^ self
		extent: `16 @ 16`
		fromArray: #(12288 30720 64512 12288 12288 12288 12288 12288 12288 12288 0 0 0 0 0 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:50:07'!
waitCursor
	"Answer the instance of me that is the shape of an Hourglass (was in the 
	shape of three small balls)."
	^ self
		extent: `16 @ 16`
		fromArray: #(65532 32772 16392 8208 7392 4032 1920 1920 2368 4384 8592 17352 36852 65532 0 )
		offset: `0 @ 0`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:50:19'!
webLinkCursor
	"Return a cursor that can be used for emphasizing web links"
	^ (CursorWithMask
		extent: `16 @ 16`
		fromArray: #(3072 4608 4608 4608 4608 5046 4681 29257 37449 37449 32769 32769 49155 16386 24582 16380 )
		offset: -5 @ 0) setMaskForm:
		(Form
			extent: `16 @ 16`
			fromArray:
				(#(3072 7680 7680 7680 7680 8118 8191 32767 65535 65535 65535 65535 65535 32766 32766 16380 ) collect: [ :bits |
					bits bitShift: 16 ])
			offset: `0 @ 0`).! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:50:30'!
writeCursor
	"Answer the instance of me that is the shape of a pen writing."
	^ self
		extent: `16 @ 16`
		fromArray: #(24 60 72 144 288 580 1156 2316 4624 9232 30728 20728 57728 32512 0 0 )
		offset: `0 @ 0`.! !


!CursorWithMask class methodsFor: 'as yet unclassified' stamp: 'pb 3/3/2020 03:54:50'!
derivedFrom: aForm      "Cursor initNormalWithMask.  Cursor normal show"
	"aForm is presumably a cursor"
	| cursor mask ext |
	ext _ aForm extent.
	cursor _ self extent: ext.
	cursor copy: (1@1 extent: ext) from: `0@0` in: aForm rule: Form over.
	mask _ Form extent: ext.
	(1@1) eightNeighbors do:
		[:p | mask copy: (p extent: ext) from: `0@0` in: aForm rule: Form under].
	cursor setMaskForm: mask.
	cursor offset: ((aForm offset - (1@1)) max: ext negated).
	^ cursor! !


!Rectangle class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 03:51:23'!
fromUser
	"Answer a Rectangle that is determined by having the user 
	designate the top left and bottom right corners."
	| originRect |
	originRect _ (Cursor cursorAt: #originCursor) showWhile: [
		(Sensor mousePoint extent: `0 @ 0`) newRectFrom: [ :f |
			Sensor mousePoint extent: `0 @ 0` ]].
	^ (Cursor cursorAt: #cornerCursor) showWhile: [
		originRect newRectFrom: [ :f |
			f origin corner: Sensor mousePoint ]].! !


!StrikeFont methodsFor: 'emphasis' stamp: 'pb 3/3/2020 03:57:46'!
makeItalicGlyphs
	"Make an italic set of glyphs with same widths by skewing left and right.
	In the process, characters would overlap, so we widen them all first.
	"
	| extraWidth newGlyphs newXTable x newX w extraOnLeft |  
	extraOnLeft _ (self lineSpacing-1-self ascent+4)//4 max: 0.
	extraWidth _ ((self ascent-5+4)//4 max: 0) + extraOnLeft.
	newGlyphs _ Form extent: (glyphs width + (maxAscii + 1 - minAscii*extraWidth)) @ glyphs height depth: glyphs depth.
	newGlyphs fillWhite.
	newXTable _ xTable copy.

	"Copy glyphs into newGlyphs with room on left and right for overlap."
	minAscii to: maxAscii+1 do:
		[:ascii | x _ xTable at: ascii+1.  w _ (xTable at: ascii+2) - x.
		newX _ newXTable at: ascii+1.
		newGlyphs copy: ((newX + extraOnLeft) @ 0 extent: w @ glyphs height)
			from: x @ 0 in: glyphs rule: Form over.
		newXTable at: ascii+2 put: newX + w + extraWidth].		
	glyphs _ newGlyphs. 
	xTable _ newXTable.
	"Slide the bitmaps left and right for synthetic italic effect."
	4 to: self ascent-1 by: 4 do:
		[:y | 		"Slide ascenders right..."
		glyphs copy: (`1@0` extent: glyphs width @ (self ascent - y))
			from: `0@0` in: glyphs rule: Form over].
	self ascent to: self lineSpacing-1 by: 4 do:
		[:y | 		"Slide descenders left..."
		glyphs copy: (0@y extent: glyphs width @ glyphs height)
			from: 1@y in: glyphs rule: Form over].
	self isSynthetic: true! !

!StrikeFont methodsFor: 'glyphs' stamp: 'pb 3/3/2020 03:57:26'!
glyphAt: character put: characterForm
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii _ character numericValue.
	ascii < minAscii ifTrue: [^ self error: 'Cant store characters below min ascii'].
	ascii > maxAscii ifTrue:
		[(self confirm:
'This font does not accomodate ascii values higher than ' , maxAscii printString , '.
Do you wish to extend it permanently to handle values up to ' , ascii printString)
			ifTrue: [self extendMaxAsciiTo: ascii]
			ifFalse: [^ self error: 'No change made']].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	widthDif _ characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue:
		["Make new glyphs with more or less space for this char"
		newGlyphs _ Form extent: (glyphs width + widthDif) @ glyphs height depth: glyphs depth.
		newGlyphs copy: (`0@0` corner: leftX@glyphs height)
			from: `0@0` in: glyphs rule: Form over.
		newGlyphs copy: ((rightX+widthDif)@0 corner: newGlyphs width@glyphs height)
			from: rightX@0 in: glyphs rule: Form over.
		glyphs _ newGlyphs.
		"adjust further entries on xTable"
		xTable _ xTable copy.
		ascii+2 to: xTable size
			do: [:i | xTable at: i put: (xTable at: i) + widthDif]].
	glyphs copy: (leftX @ 0 extent: characterForm extent)
		from: 0@0 in: characterForm rule: Form over
"
| f |  f _ AbstractFont default.
f glyphAt: $  put: (Form extent: (f widthOf: $ )+10@f lineSpacing)
"! !

!StrikeFont methodsFor: 'building' stamp: 'pb 3/3/2020 03:57:08'!
buildFromForm: allGlyphs data: data name: aString

	| x shortUnderscore firstGlyphInFiles |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.

	firstGlyphInFiles _ 257-(data size-3).
	minAscii _ 1.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs depth > 16 ifTrue: [ allGlyphs asFormOfDepth: 16 ] ifFalse: [ allGlyphs ].
	x _ 0.
	4 to: data size do: [ :i |
		x _ (data at: i) asNumber.
		xTable at: i+firstGlyphInFiles+1-4 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil.

	self makeCrVisible.

	"Replace glyph for 127 (a box) with a short version of the underscore (used to optionally mark subscript in code)"
	shortUnderscore _ self glyphAt: $_.
	shortUnderscore _ shortUnderscore copy: (`0@0` extent: 1@shortUnderscore height).
	self glyphAt: (Character numericValue: 127) put: shortUnderscore! !

!StrikeFont methodsFor: 'building' stamp: 'pb 3/3/2020 03:57:59'!
stripHighGlyphs
	"Remove glyphs for characters above 128"
	| i |

	characterToGlyphMap _ nil.
	maxAscii _ 127.
	
	xTable _ xTable copyFrom: 1 to: maxAscii + 3.
	i _ xTable at: maxAscii + 2.
	xTable at: maxAscii + 3 put: i.
	glyphs _ glyphs copy: (`0@0` extent: i+1@glyphs height).
	maxWidth _ 0.
	2 to: xTable size do: [ :ii |
		maxWidth _ maxWidth max: (xTable at: ii) - (xTable at: ii-1)-1 ].
	self reset! !


!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'pb 3/3/2020 03:58:50'!
resizeAtPoint: aPoint
	
	|region|
		
	region _ (aPoint min: extent - 1) // (extent // 3).
	
	^ region caseOf: {
		[`0@0`] -> [#topLeft].
		[`1@0`] -> [#top].
		[`2@0`] -> [#topRight].
		[`0@1`] -> [#left].
		[`1@1`] -> [#full].
		[`2@1`] -> [#right].
		[`0@2`] -> [#bottomLeft].
		[`1@2`] -> [#bottom].
		[`2@2`] -> [#bottomRight].
	} otherwise: [nil]! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'pb 3/3/2020 03:59:07'!
selectionRectangle: region

	^ region caseOf: {
		[#topLeft] -> [`0@0` corner: (extent // 2)].
		[#top] -> [`0@0` corner: (extent x@(extent y // 2))].
		[#topRight] -> [(extent x // 2)@0 corner: (extent x@(extent y // 2))].
		[#left] -> [`0@0` corner: (extent x // 2)@extent y].
		[#full] -> [`0@0` corner: extent].
		[#right] -> [(extent x // 2)@0 corner: extent].
		[#bottomLeft] -> [0@(extent y // 2) corner: (extent x // 2)@extent y].
		[#bottomRight] -> [(extent x // 2)@(extent y // 2) corner: extent].
		[#bottom] -> [0@(extent y // 2) corner: extent].
	}! !


!DraggingGuideMorph methodsFor: 'initialization' stamp: 'pb 3/3/2020 03:54:58'!
initialize

	super initialize.
	extent _ `0@0`.! !


!HandMorph methodsFor: 'drawing' stamp: 'pb 3/3/2020 03:50:54'!
drawOn: aCanvas
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	aCanvas
		stencil: (Cursor cursorAt: #moveCursor)
		at: `0 @ 0`
		color: Color black.! !

