'From Cuis6.3 [latest update: #6313] on 5 April 2024 at 4:29:34 pm'!

!Color methodsFor: 'transformations' stamp: 'jmv 4/5/2024 16:26:20'!
adjustSaturation: saturation brightness: brightness
	"Adjust the relative saturation and brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	| v |
	v := self brightness + brightness min: 1.0 max: 0.005.
	self saturation < 0.005 ifTrue: [
		"Don't adjust saturation if gray. Hue is meaningless."
		^ (Color gray: v) alpha: self alpha ].
	^ Color
		h: self hue
		s: (self saturation + saturation min: 1.0 max: 0.005)
		v: v
		alpha: self alpha! !

