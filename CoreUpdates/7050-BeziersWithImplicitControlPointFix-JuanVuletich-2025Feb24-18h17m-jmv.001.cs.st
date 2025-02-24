'From Cuis7.3 [latest update: #7049] on 24 February 2025 at 6:18:45 pm'!

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 2/24/2025 18:14:46'!
cubicBezierTo: aPoint control1: controlPoint1 control2: controlPoint2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ].
Display forceToScreen
	"
	engine cubicBezierTo: aPoint control1: controlPoint1 control2: controlPoint2.
	currentPoint := aPoint.
	lastControlPoint := controlPoint2.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 2/24/2025 18:14:37'!
quadraticBezierTo: aPoint control: controlPoint
	"
(VectorCanvas onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			quadraticBezierTo: 300@200 control: 100@210 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas quadraticBezierTo: 60@10 control: 15@30 ].
Display forceToScreen
	"
	engine quadraticBezierTo: aPoint control: controlPoint.
	currentPoint := aPoint.
	lastControlPoint := controlPoint.! !

