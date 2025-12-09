'From Cuis7.5 [latest update: #7748] on 9 December 2025 at 1:20:05 pm'!

!Color class methodsFor: 'instance creation' stamp: 'len 30/Nov/2025 22:29:00'!
okl: lightness c: chroma h: hue
	"Create a color from OKLCH: lightness (0.0 - 1.0), chroma (0.0 - ~0.4), hue (0.0 - 360.0).
	If the color is out of gamut, reduce chroma until it fits. Use binary search for efficiency.
	
	See https://en.wikipedia.org/wiki/Oklab_color_space
	See https://evilmartians.com/chronicles/oklch-in-css-why-quit-rgb-hsl

		| step |
		step := 360 / 10.0.
		ColoredBoxMorph allInstancesDo: [ :a | a delete ].
		0 to: 360 - step by: step :: do: [:h|
			(ColoredBoxMorph new color: (Color okl: 0.75 c: 0.4 h: h)) openInWorld ]
		
	"
	| min max minChroma maxChroma midChroma |
	min := -0.00001.
	max := 1.00001.
	self okl: lightness c: chroma h: hue sRGBDo: [:r :g :b|
		((r between: min and: max) and: [(g between: min and: max) and: [b between: min and: max]])
			ifTrue: [^ self r: r g: g b: b]].
	"The color is impossible to display, so find the maximum chroma that fits:"
	minChroma := 0.0.
	maxChroma := chroma.
	"Binary search. 20 iterations is enough precision for any screen."
	20 timesRepeat:
		[midChroma := minChroma + maxChroma / 2.0.
		self okl: lightness c: midChroma h: hue sRGBDo: [:r :g :b|
			((r between: 0.0 and: 1.0) and: [(g between: 0.0 and: 1.0) and: [b between: 0.0 and: 1.0]])
				ifTrue: [minChroma := midChroma]
				ifFalse: [maxChroma := midChroma]]].
	self okl: lightness c: minChroma h: hue sRGBDo: [:r :g :b|
		^ self r: (r min: 1.0 max: 0.0) g: (g min: 1.0 max: 0.0) b: (b min: 1.0 max: 0.0)]! !

!Color class methodsFor: 'instance creation' stamp: 'len 30/Nov/2025 21:58:00'!
okl: lightness c: chroma h: hue sRGBDo: aBlock
	"Create a color from OKLCH: lightness (0.0 - 1.0), chroma (0.0 - ~0.4), hue (0.0 - 360.0)."
	| h a b ℓ m s red green blue |
	"OKLCH to OKLAB (Cylindrical to Cartesian)"
	h := hue * Float pi / 180.0.
	a := chroma * h cos.
	b := chroma * h sin.
    	"OKLAB to Linear LMS
	Use the standard OKLAB matrices to move to LMS cone response space.
	Note: The original non-linearity in OKLAB is a cube root, so we cube here to undo it."
	ℓ := (lightness + (0.3963377774*a) + (0.2158037573*b)) cubed.
	m := (lightness - (0.1055613458*a) - (0.0638541728*b)) cubed.
	s := (lightness - (0.0894841775*a) - (1.2914855480*b)) cubed.
	"Linear LMS to Linear sRGB (matrix multiplication to shift from Cone response to RGB channels),
	and then Linear sRGB to sRGB (Gamma Correction)"
	red := self linearTosRGBGamma: 4.0767416621*ℓ - (3.3077115913*m) + (0.2309699292*s).
	green := self linearTosRGBGamma: -1.2684380046*ℓ + (2.6097574011*m) - (0.3413193965*s).
	blue := self linearTosRGBGamma: -0.0041960863*ℓ - (0.7034186147*m) + (1.7076147010*s).
	^ aBlock value: red value: green value: blue! !

