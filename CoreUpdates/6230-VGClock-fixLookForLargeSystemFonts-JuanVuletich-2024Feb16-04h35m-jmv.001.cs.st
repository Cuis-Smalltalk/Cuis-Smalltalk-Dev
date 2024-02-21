'From Cuis6.3 [latest update: #6229] on 16 February 2024 at 4:45:55 am'!

!Sample09Clock methodsFor: 'drawing' stamp: 'jmv 2/16/2024 04:42:56'!
drawOn: aCanvas

	| f |
	aCanvas
		strokeWidth: 10 color: Color lightCyan
		fillColor: Color veryVeryLightGray do: [ :canvas |
			canvas circleCenter:  0@0 radius: 100 ].
	f := FontFamily familyName: 'DejaVu Sans' pointSize: 14.
	aCanvas drawString: 'XII' at: -13 @ 58 font: f color: Color brown.
	aCanvas drawString: 'III' at: 73 @ -24 font: f color: Color brown.
	aCanvas drawString: 'VI' at: -9 @ -106 font: f color: Color brown.
	aCanvas drawString: 'IX' at: -90 @ -24 font: f color: Color brown.

	f := FontFamily familyName: 'Alex Brush' pointSize: 16.
	aCanvas drawString: 'Chaque Heure' atCenterX: 0@ 18 font: f color: Color black.
	f := FontFamily familyName: 'Alex Brush' pointSize: 8.
	aCanvas drawString: 'pour la Minorie' atCenterX: 0@ -60 font: f color: Color black.! !

