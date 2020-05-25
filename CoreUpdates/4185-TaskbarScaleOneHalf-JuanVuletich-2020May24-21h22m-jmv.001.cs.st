'From Cuis 5.0 [latest update: #4176] on 24 May 2020 at 9:30:33 pm'!

!TaskbarMorph methodsFor: 'resize' stamp: 'jmv 5/24/2020 21:29:40'!
scaleSmall

	self scale: 1/2! !


!TaskbarMorph methodsFor: 'menus' stamp: 'jmv 5/24/2020 21:29:02'!
mouseButton2Activity

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	menu
		addLine;
		add: 'Small Height' action: #scaleSmall;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4.
	menu popUpInWorld! !

!TaskbarMorph methodsFor: 'resize' stamp: 'jmv 5/24/2020 21:30:02'!
scale: anInteger

	(anInteger between: 1/2 and: 4) ifFalse: [ self error: 'scale should be 1/2, 1, 2 or 4' ].
	scale := anInteger.
	self screenSizeChanged. "rescale self"
	viewBox ifNotNil: [ "rescale buttons"
		viewBox submorphs do: [ :button | 
			button layoutSpec fixedWidth: self defaultHeight
		]
	]! !

