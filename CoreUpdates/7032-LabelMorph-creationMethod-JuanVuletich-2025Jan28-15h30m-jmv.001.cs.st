'From Cuis7.3 [latest update: #7029] on 28 January 2025 at 3:31:03 pm'!

!LabelMorph class methodsFor: 'instance creation' stamp: 'jmv 1/28/2025 15:17:11'!
contents: aString fontSize: pointSize emphasis: emphasisCode
	"
	(LabelMorph contents: 'I am a LabelMorph' fontSize: 25 emphasis: 3) openInWorld.
	"
	^ self basicNew
		initWithContents: aString
		font: (FontFamily defaultFamilyPointSize: pointSize)
		emphasis: emphasisCode.! !

