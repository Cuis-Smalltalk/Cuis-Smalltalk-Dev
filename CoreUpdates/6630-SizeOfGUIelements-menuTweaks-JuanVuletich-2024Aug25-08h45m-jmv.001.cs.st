'From Cuis7.1 [latest update: #6629] on 25 August 2024 at 8:52:21 am'!

!Theme class methodsFor: 'user interface' stamp: 'jmv 8/25/2024 08:49:38'!
changeFontSizes
	| options menu |
	options := #(
		'40 - very large GUI elements'
		'32 - large GUI elements'
		'28 - large GUI elements'
		'24 - very high dpi laptops'
		'21 - very high dpi laptops'
		'18 - high dpi laptops'
		'16 - high dpi laptops'
		'14 - large desktop displays'
		'13 - large desktop displays'
		'12 - standard dpi displays'
		'11 - standard dpi displays'
		'10 - small GUI elements'
		'  9 - small GUI elements'
		'  6 - very small GUI elements'
		).
	menu := MenuMorph new.
	menu addTitle: 'Point size of default Font'; addStayUpIcons; removeStayUpBox.
	options do: [ :option | | pt |
		pt := option findPositiveInteger.
		(menu add: option target: self action: #setDefaultFontSize: argument: pt)
			isSelected: pt = FontFamily defaultPointSize ].
	menu popUpInWorld: self runningWorld.! !

