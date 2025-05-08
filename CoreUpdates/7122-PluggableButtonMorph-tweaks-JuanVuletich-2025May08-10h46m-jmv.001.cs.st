'From Cuis7.3 [latest update: #7121] on 8 May 2025 at 10:51:38 am'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/8/2025 10:30:23'!
drawOn: aCanvas
	self isRoundButton
		ifTrue: [ self drawRoundLookOn: aCanvas ]
		ifFalse: [ self draw3DLookOn: aCanvas ].
	aCanvas drawButtonIconFromCurrentMorph .! !


!ScrollBar methodsFor: 'access' stamp: 'jmv 5/8/2025 10:51:22'!
color: aColor
	"Change the color of the scrollbar to go with aColor."
	| buttonColor sliderColor |
	super color: aColor.

	buttonColor := Color transparent.
	upButton color: buttonColor.
	downButton color: buttonColor.

	sliderColor := color alphaMixed: 0.7 with: Theme current scrollbarButtonColor.
	slider color: sliderColor slightlyLighter.

	sliderShadow color: (color alphaMixed: 0.45 with: Theme current scrollbarSliderShadowColor)! !

