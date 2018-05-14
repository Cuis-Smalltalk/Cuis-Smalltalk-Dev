'From Cuis 5.0 of 7 November 2016 [latest update: #3293] on 10 May 2018 at 8:53:39 pm'!

!ColorForm class methodsFor: 'as yet unclassified' stamp: 'jmv 5/10/2018 20:52:39'!
extent: aPoint
	"Native depth"

	^self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ])! !


!Color class methodsFor: 'class initialization' stamp: 'jmv 5/10/2018 20:43:18'!
initializeIndexedColors
	"Build an array of colors corresponding to the fixed colormap used
	 for display depths of 1, 2, 4, or 8 bits."
	"
	Color initializeIndexedColors
	"
	"
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 1 g: 0 b: 0); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0 g: 1 b: 0); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0 g: 0 b: 1); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0.5 g: 0.5 b: 0.5); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color blue; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color green; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color red; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color cyan; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color yellow; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color magenta; display.
	"

	| a index grayVal |
	a _ Array new: 256.

	"1-bit colors (monochrome)"
	a at: 1 put: `Color r: 1.0 g: 1.0 b: 1.0`.		"white or transparent"
	a at: 2 put: `Color r: 0.0 g: 0.0 b: 0.0`.	"black"

	"additional colors for 2-bit color"
	a at: 3 put: `Color r: 1.0 g: 1.0 b: 1.0`.	"opaque white"
	a at: 4 put: `Color r: 0.5 g: 0.5 b: 0.5`.	"1/2 gray"

	"additional colors for 4-bit color"
	a at:  5 put: Color red.
	a at:  6 put: Color green.
	a at:  7 put: Color blue.
	a at:  8 put: Color cyan.
	a at:  9 put: Color yellow.
	a at: 10 put: Color magenta.

	a at: 11 put: `Color r: 0.125 g: 0.125 b: 0.125`.		"1/8 gray"
	a at: 12 put: `Color r: 0.25 g: 0.25 b: 0.25`.			"2/8 gray"
	a at: 13 put: `Color r: 0.375 g: 0.375 b: 0.375`.		"3/8 gray"
	a at: 14 put: `Color r: 0.625 g: 0.625 b: 0.625`.		"5/8 gray"
	a at: 15 put: `Color r: 0.75 g: 0.75 b: 0.75`.			"6/8 gray"
	a at: 16 put: `Color r: 0.875 g: 0.875 b: 0.875`.		"7/8 gray"

	"additional colors for 8-bit color"
	"24 more shades of gray (1/32 increments but not repeating 1/8 increments)"
	index _ 17.
	1 to: 31 do: [:v |
		(v \\ 4) = 0 ifFalse: [
			grayVal _ v / 32.0.
			a at: index put: (Color r: grayVal g: grayVal b: grayVal).
			index _ index + 1]].

	"The remainder of color table defines a color cube with six steps
	 for each primary color. Note that the corners of this cube may repeat
	 previous colors, but this simplifies the mapping between RGB colors
	 and color map indices. This color cube spans indices 40 through 255
	 (indices 41-256 in this 1-based array)."
	0 to: 5 do: [:r |
		0 to: 5 do: [:g |
			0 to: 5 do: [:b |
				index _ 41 + ((36 * r) + (6 * b) + g).
				index > 256 ifTrue: [
					self error: 'index out of range in color table compuation'].
				a at: index put: (Color r: r g: g b: b range: 5)]]].

	IndexedColors _ a.
! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Color initializeIndexedColors!

