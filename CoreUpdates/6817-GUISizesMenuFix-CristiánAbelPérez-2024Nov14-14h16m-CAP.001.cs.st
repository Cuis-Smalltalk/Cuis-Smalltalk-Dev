'From Cuis7.1 [latest update: #6770] on 14 November 2024 at 2:19:54 pm'!

!Theme class methodsFor: 'user interface' stamp: 'CAP 11/14/2024 14:16:20'!
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
	options do: [ :option | | pt addedItem |
		pt := option findPositiveInteger.
		addedItem := menu add: option target: self action: #setDefaultFontSize: argument: pt.
		(pt = FontFamily defaultPointSize) ifTrue: [ menu selectItem: addedItem ] ].
	menu popUpInWorld: self runningWorld.! !

