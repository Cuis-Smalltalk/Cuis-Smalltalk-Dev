'From Cuis7.7 [latest update: #7779] on 24 December 2025 at 9:30:02 am'!

!Color class methodsFor: 'instance creation' stamp: 'len 24/Dec/2025 09:29:13'!
okl: lightness c: chroma h: hue
	"Create a color from OKLCH: lightness (0.0 - 1.0), chroma (0.0 - ~0.4), hue (0.0 - 360.0).
	If the color is out of sRGB gamut, reduce chroma until it fits. Use binary search for efficiency.
	
	See https://en.wikipedia.org/wiki/Oklab_color_space
	See https://evilmartians.com/chronicles/oklch-in-css-why-quit-rgb-hsl

		| step |
		step := 360 / 10.0.
		ColoredBoxMorph allInstancesDo: [ :a | a delete ].
		0 to: 360 - step by: step :: collect: [:h|
			(ColoredBoxMorph new color: (Color okl: 0.75 c: 0.4 h: h)) ]
	"
	| answer minChroma maxChroma midChroma |
	(answer := self okOrNilL: lightness c: chroma h: hue) ifNotNil: [^ answer].
	"The color is impossible to represent as an instance of Color, so find the maximum chroma that fits:"
	minChroma := 0.0.
	maxChroma := chroma.
	"backstop answer"
	answer := self okOrNilL: lightness c: minChroma h: hue.
	"Binary search. 20 iterations is enough precision for any screen."
	20 timesRepeat:
		[midChroma := minChroma + maxChroma / 2.0.
		(self okOrNilL: lightness c: midChroma h: hue)
			ifNil: [maxChroma := midChroma]
			ifNotNil: [:aColor| answer := aColor. minChroma := midChroma]].
	^ answer! !

