'From Cuis7.3 [latest update: #7071] on 18 March 2025 at 12:11:55 pm'!
!classDefinition: #Sample02Bezier category: #'Morphic-Examples'!
PlacedMorph subclass: #Sample02Bezier
	instanceVariableNames: 'aStart aEnd aControlPoint bStart bEnd bControlPoint1 bControlPoint2 dashOffset '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!

!Sample02Bezier methodsFor: 'stepping' stamp: 'jmv 3/18/2025 10:49:48'!
step

	dashOffset := dashOffset + 2.
	self redrawNeeded.! !

!Sample02Bezier methodsFor: 'stepping' stamp: 'jmv 3/18/2025 10:49:37'!
stepTime
	^200! !

!Sample02Bezier methodsFor: 'stepping' stamp: 'jmv 3/18/2025 10:47:48'!
wantsSteps

	^ true! !


!Sample02Bezier methodsFor: 'initialization' stamp: 'jmv 3/18/2025 10:46:27'!
initialize
	super initialize.
	aStart := 500 atRandom @ 500 atRandom.
	aEnd := 500 atRandom @ 500 atRandom.
	aControlPoint := 500 atRandom @ 500 atRandom.
	bStart := 500 atRandom @ 500 atRandom.
	bEnd := 500 atRandom @ 500 atRandom.
	bControlPoint1 := 500 atRandom @ 500 atRandom.
	bControlPoint2 := 500 atRandom @ 500 atRandom.
	dashOffset := 0.! !

!Sample02Bezier methodsFor: 'drawing' stamp: 'jmv 3/18/2025 12:10:40'!
drawOn: aCanvas
	"Two Bezier curves. A Quadratic one (with start, end and a single control point), and a Cubic one (with start, end, and two control points)
	Sample02Bezier new openInHand
	"

	aCanvas strokeWidth: 2 color: Color green strokeDashArray: #(10 10) strokeDashArrayOffset: dashOffset do: [
		aCanvas
				moveTo: aStart;
				quadraticBezierTo: aEnd control: aControlPoint ].
	aCanvas strokeWidth: 1 color: Color darkGray strokeDashArray: #(1 3) strokeDashArrayOffset: Random next * 4 do: [
		aCanvas
				moveTo: aStart;
				lineTo: aControlPoint;
				lineTo: aEnd ].

	aCanvas strokeWidth: 3 color: Color red strokeDashArray: #(10 35) strokeDashArrayOffset: dashOffset do: [
		aCanvas
				moveTo: bStart;
				cubicBezierTo: bEnd control1: bControlPoint1 control2: bControlPoint2 ].
	aCanvas strokeWidth: 1 color: Color darkGray strokeDashArray: #(15 8) strokeDashArrayOffset: dashOffset do: [
		aCanvas
				moveTo: bStart;
				lineTo: bControlPoint1;
				lineTo: bControlPoint2;
				lineTo: bEnd ].! !

!classDefinition: #Sample02Bezier category: #'Morphic-Examples'!
PlacedMorph subclass: #Sample02Bezier
	instanceVariableNames: 'aStart aEnd aControlPoint bStart bEnd bControlPoint1 bControlPoint2 dashOffset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!

!Sample02Bezier reorganize!
('initialization' initialize)
('drawing' drawOn:)
('stepping' step stepTime wantsSteps)
('geometry testing' yAxisPointsUp)
!

