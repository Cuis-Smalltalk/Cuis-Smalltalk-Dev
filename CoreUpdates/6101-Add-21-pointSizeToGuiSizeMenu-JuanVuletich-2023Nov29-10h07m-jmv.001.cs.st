'From Cuis 6.0 [latest update: #6101] on 29 November 2023 at 10:07:36 am'!

!Theme class methodsFor: 'user interface' stamp: 'jmv 11/29/2023 09:23:08'!
changeFontSizes
	| options menu |
	options := #(
		"'40 - very large'"
		'32 - very high resolution laptops'
		'28 - very high resolution laptops'
		'24 - very high resolution laptops'
		'21 - very high resolution laptops'
		'18 - high resolution laptops'
		'16 - high resolution laptops'
		'14 - large desktop displays'
		'13 - large desktop displays'
		'12 - standard desktop and laptops'
		'11 - standard desktop and laptops'
		'10 - screen with limited space'
		'  9 - screen with limited space'
		"'  6 - displays with very limited space'"
		).
	menu := MenuMorph new.
	menu addTitle: 'Point size of default Font'; addStayUpIcons; removeStayUpBox.
	options do: [ :option | | pt |
		pt := option findPositiveInteger.
		(menu add: option target: self action: #setDefaultFontSize: argument: pt)
			isSelected: pt = FontFamily defaultPointSize ].
	menu popUpInWorld: self runningWorld.! !

