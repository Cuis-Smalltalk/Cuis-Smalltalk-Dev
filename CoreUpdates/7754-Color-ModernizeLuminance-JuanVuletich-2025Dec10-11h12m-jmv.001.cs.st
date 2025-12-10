'From Cuis7.5 [latest update: #7753] on 10 December 2025 at 11:12:25 am'!

!Color methodsFor: 'accessing' stamp: 'jmv 10/Dec/2025 09:20:26'!
luminance
	"Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity.
	See https://en.wikipedia.org/wiki/HSL_and_HSV"

	^ (0.2627 * self red) +
	   (0.6780 * self green) +
	   (0.0593 * self blue)! !

!Color methodsFor: 'private' stamp: 'jmv 10/Dec/2025 09:34:45'!
setHue: hue chroma: chroma luminance: luma
	"Initialize this color to the given hue, chroma, and luma.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 

	hf := hue \\ 360.
	i := hf // 60.  				"integer part of hue"
	x := (hf \\ 60) / 60.0 .         	"fractional part of hue"
	x := x \\  2 .
	i \\ 2 = 1 ifTrue: [ x := 1.0 - x ].
	x := chroma  * x.

	0 = i ifTrue: [ r1 := chroma. g1 := x. b1 := 0.0 ].
	1 = i ifTrue: [ r1 := x. g1 := chroma. b1 := 0.0 ].
	2 = i ifTrue: [ r1 := 0.0. g1 := chroma. b1 := x ].
	3 = i ifTrue: [ r1 := 0.0. g1 := x. b1 := chroma ].
	4 = i ifTrue: [ r1 := x. g1 := 0.0. b1 := chroma ].
	5 = i ifTrue: [ r1 := chroma. g1 := 0.0. b1 := x ].

	m := luma - (0.2627*r1) - (0.6780*g1) - (0.0593*b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 := r1 + m.
	r1 > 1.0 
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 := g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 := b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1! !

