'From Cuis7.5 [latest update: #7753] on 10 December 2025 at 11:13:19 am'!

!Color methodsFor: 'private' stamp: 'jmv 10/Dec/2025 09:34:17'!
setHue: hue chroma: chroma brightness: brightness
	"Initialize this color to the given hue, chroma, and luma.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 
	hf := hue \\ 360.
	i := hf // 60.  				"integer part of hue"
	x := (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x := x \\  2 .
	i \\ 2 = 1 ifTrue: [ x := 1.0 - x ].
	x := chroma  * x.

	0 = i ifTrue: [ r1 := chroma. g1 := x. b1 := 0.0 ].
	1 = i ifTrue: [ r1 := x. g1 := chroma. b1 := 0.0 ].
	2 = i ifTrue: [ r1 := 0.0. g1 := chroma. b1 := x ].
	3 = i ifTrue: [ r1 := 0.0. g1 := x. b1 := chroma ].
	4 = i ifTrue: [ r1 := x. g1 := 0.0. b1 := chroma ].
	5 = i ifTrue: [ r1 := chroma. g1 := 0.0. b1 := x ].

	m := brightness - ((r1 max: g1) max: b1).
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


!Color class methodsFor: 'instance creation' stamp: 'jmv 10/Dec/2025 09:37:32'!
hue: hue saturation: saturation brightness: brightness
	"Create a color with the given hue, saturation, and brightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1) is pure red."
	"Note: By convention, brightness is abbreviated 'v' to to avoid confusion with blue."

	^ self h: hue s: saturation v: brightness! !

