'From Cuis 6.0 [latest update: #5534] on 26 October 2022 at 5:23:01 pm'!
!classDefinition: #TrueTypeFont category: 'Graphics-TrueType'!
AbstractFont subclass: #TrueTypeFont
	instanceVariableNames: 'ttFontDescription pointSize emphasis glyphForms glyphFormsByUtf8 family baseFont derivativeFonts '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 10/26/2022 17:21:51'!
family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber
	ttFontDescription _ aTTFontDescription.
	emphasis _ aTTFontDescription emphasis.
	pointSize _ aNumber.
	glyphFormsByUtf8 _ Array new: 256.
	family _ aTrueTypeFontFamily.
	baseFont _ nil.
	derivativeFonts _ emphasis = 0 ifTrue: [Dictionary new].! !

!TrueTypeFont methodsFor: 'emphasis' stamp: 'jmv 10/26/2022 17:22:11'!
addEmphasis: code
	"Set the integer code for emphasis. See senders."

	emphasis _ code.
	(emphasis anyMask: 4+8) ifTrue: [
		"They need to be recreated"
		glyphFormsByUtf8 _ Array new: 256 ].! !

!TrueTypeFont methodsFor: 'caching' stamp: 'jmv 10/26/2022 17:22:00'!
releaseCachedState
	glyphFormsByUtf8 atAllPut: nil.
	derivativeFonts notNil ifTrue: [
		derivativeFonts keysAndValuesDo: [ :anEmphasis :derivedFont |
			derivedFont releaseCachedState  ]]! !

!classDefinition: #TrueTypeFont category: 'Graphics-TrueType'!
AbstractFont subclass: #TrueTypeFont
	instanceVariableNames: 'ttFontDescription pointSize emphasis glyphFormsByUtf8 family baseFont derivativeFonts'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
