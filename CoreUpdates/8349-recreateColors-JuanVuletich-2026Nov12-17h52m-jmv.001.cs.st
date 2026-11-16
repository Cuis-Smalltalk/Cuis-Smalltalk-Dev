'From Cuis7.9 [latest update: #8347] on 12 November 2026 at 6:04:10 pm'!

!Sample01Star methodsFor: 'drawing' stamp: 'jmv 21/Mar/2025, 8:36:27 pm (UTC)'!
drawOn: aCanvas
	"A 5 point star.
	Feel free to experiment with this code.
	The worst thing that could happen is that your morph breaks. That will only mean extra fun!!
	Sample01Star new openInHand
	"

	aCanvas strokeWidth: 12 color: `Color lightOrange` do: [
	"aCanvas fillColor: Color lightOrange do: ["
	"aCanvas strokeWidth: 12 color: Color lightOrange fillColor: Color yellow do: ["
		aCanvas
				moveTo: `(Point rho: 100 theta: 90 degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*2+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*4+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*6+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*8+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: 90 degreesToRadians)`.
		].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Theme withAllSubclassesDo: [ :c | c compileAll ]!

