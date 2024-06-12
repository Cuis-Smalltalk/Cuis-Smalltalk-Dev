'From Cuis7.1 [latest update: #6465] on 12 June 2024 at 11:15:13 am'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 6/12/2024 11:14:45'!
icon: aForm
	icon := aForm.
	magnifiedIcon := nil.
	self redrawNeeded.! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 6/12/2024 11:14:32'!
label: aStringOrNil font: aFontOrNil
	"Label this button with the given string."
	label := aStringOrNil.
	font := aFontOrNil.
	(self fontToUse notNil and: [ label notNil ])
		ifTrue: [ "Add a bit of padding"
			extent := (self fontToUse widthOfString: label) + 10 @ (self fontToUse lineSpacing + 10) ].
	self redrawNeeded.! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 6/12/2024 11:14:41'!
magnifiedIcon: aForm
	"Meaning: Don't magnify aForm to adapt it to our extent."

	icon := nil.
	magnifiedIcon := aForm.
	self redrawNeeded.! !

