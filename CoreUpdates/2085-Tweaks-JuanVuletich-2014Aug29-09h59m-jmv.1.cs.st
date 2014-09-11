'From Cuis 4.2 of 25 July 2013 [latest update: #2083] on 29 August 2014 at 9:59:37.520001 am'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 8/27/2014 15:46'!
hue: hue saturation: saturation brightness: brightness
	"Create a color with the given hue, saturation, and brightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1) is pure red."
	"Note: By convention, brightness is abbreviated 'v' to to avoid confusion with blue."

	^ self basicNew setHue: hue saturation: saturation brightness: brightness! !


!Point methodsFor: 'point functions' stamp: 'jmv 8/25/2014 13:07'!
nearestPointAlongLineFrom: p1 to: p2
	"Note this will give points beyond the endpoints.
	Streamlined by Gerardo Richarte 11/3/97"
	| x21 y21 t x1 y1 |
	p1 x = p2 x ifTrue: [^ p1 x @ y].
	p1 y = p2 y ifTrue: [^ x @ p1 y].
	x1 _ p1 x asFloat.
	y1 _ p1 y asFloat.
	x21 _ p2 x asFloat - x1.
	y21 _ p2 y asFloat - y1.
	t _ ((y asFloat - y1 / x21) + (x asFloat - x1 / y21))
			/ ((x21 / y21) + (y21 / x21)).
	^ (x1 + (t * x21)) @ (y1 + (t * y21))
"
	| old new |
	Pen new place: 200@100; goto: (old _ 500@300).
	Display reverse: (old extent: 10@10).
	[Sensor isAnyButtonPressed] whileFalse:
		[(new _ (Sensor mousePoint nearestPointAlongLineFrom: 200@100 to: 500@300) )
			= old ifFalse:
				[Display reverse: (old extent: 10@10).
				Display reverse: ((old _ new) extent: 10@10)]]
"
! !

