'From Cuis 6.0 [latest update: #5834] on 6 June 2023 at 12:21:42 pm'!

!Sample09Clock methodsFor: 'drawing' stamp: 'jmv 6/6/2023 12:20:25'!
drawOn: aCanvas

	| f |
	aCanvas
		strokeWidth: 10 color: Color lightCyan
		fillColor: Color veryVeryLightGray do: [ :canvas |
			canvas circleCenter:  0@0 radius: 100 ].
	aCanvas drawString: 'XII' at: -13 @ 58 font: nil color: Color brown.
	aCanvas drawString: 'III' at: 72 @ -23 font: nil color: Color brown.
	aCanvas drawString: 'VI' at: -8 @ -105 font: nil color: Color brown.
	aCanvas drawString: 'IX' at: -90 @ -23 font: nil color: Color brown.
	
	f := FontFamily familyName: 'Alex Brush' pointSize: 16.
	aCanvas drawString: 'Chaque Heure' atCenterX: 0@ 18 font: f color: Color black.
	f := FontFamily familyName: 'Alex Brush' pointSize: 8.
	aCanvas drawString: 'pour la Minorie' atCenterX: 0@ -60 font: f color: Color black.! !

