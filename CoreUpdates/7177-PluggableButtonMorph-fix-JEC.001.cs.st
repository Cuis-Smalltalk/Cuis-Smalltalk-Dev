'From Cuis7.3 [latest update: #7122] on 8 May 2025 at 7:24:10 pm'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'JEC 5/8/2025 14:54:06'!
label: aStringOrNil font: aFontOrNil
	"Label this button with the given string."
	label := aStringOrNil.
	font := aFontOrNil.
	(self fontToUse notNil and: [ label notNil ])
		ifTrue: [ "Add a bit of padding"
			self morphExtent: (self fontToUse widthOfString: label) + 10 @ (self fontToUse lineSpacing + 10) ]
		ifFalse: [ self redrawNeeded ].! !

