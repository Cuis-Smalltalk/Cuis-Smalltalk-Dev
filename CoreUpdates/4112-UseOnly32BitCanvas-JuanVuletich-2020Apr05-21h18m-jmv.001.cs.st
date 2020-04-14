'From Cuis 5.0 [latest update: #4106] on 5 April 2020 at 9:27:25 pm'!

!String methodsFor: 'displaying' stamp: 'jmv 4/5/2020 21:19:20'!
displayOn: aDisplayMedium at: aPoint 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	^self displayOn: aDisplayMedium at: aPoint textColor: `Color black`
	"
	'Display' displayOn: Display at: 10@10. Display forceToScreen.
	"! !


!MenuItemMorph methodsFor: 'private' stamp: 'jmv 4/5/2020 21:18:11'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 32.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: `(Color gray: 0.9)`
			borderWidth: 1 borderColor: `Color black`.
	^form! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 4/5/2020 21:18:07'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 32.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: `Color gray: 0.8`
			borderWidth: 1 borderColor: `Color black`;
		fillRectangle: (form boundingBox insetBy: 2) color: `Color black`.
	^form! !

