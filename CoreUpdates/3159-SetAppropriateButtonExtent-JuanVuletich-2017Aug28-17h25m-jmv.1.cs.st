'From Cuis 5.0 of 7 November 2016 [latest update: #3158] on 28 August 2017 at 5:26:35 pm'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 8/25/2017 15:05:56'!
label: aStringOrNil font: aFontOrNil
	"Label this button with the given string."
	label _ aStringOrNil.
	font _ aFontOrNil.
	(self fontToUse notNil and: [ label notNil ])
		ifTrue: [ "Add a bit of padding"
			extent := (self fontToUse widthOfString: label) + 10 @ (self fontToUse height + 10) ]! !

