'From Cuis7.1 [latest update: #6793] on 24 October 2024 at 4:46:07 pm'!

!LabelMorph class methodsFor: 'instance creation' stamp: 'jmv 10/24/2024 14:18:43'!
contents: aString fontSize: pointSize
	"
	(LabelMorph contents: 'I am a LabelMorph' fontSize: 25) openInWorld.
	"
	^ self basicNew
		initWithContents: aString
		font: (FontFamily defaultFamilyPointSize: pointSize)
		emphasis: 0.! !

