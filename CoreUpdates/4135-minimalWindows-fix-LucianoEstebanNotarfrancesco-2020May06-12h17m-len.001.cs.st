'From Cuis 5.0 [latest update: #4134] on 6 May 2020 at 12:17:40 pm'!

!SystemWindow methodsFor: 'label' stamp: 'len 5/1/2020 06:34:51'!
labelHeight
	"Answer the height for the window label."
	Theme current minimalWindows ifTrue: [^ 0].
	^ Preferences windowTitleFont lineSpacing+1! !

