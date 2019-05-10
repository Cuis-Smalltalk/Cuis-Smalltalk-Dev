'From Cuis 5.0 of 7 November 2016 [latest update: #3727] on 5 May 2019 at 2:40:12 pm'!

!FontFamily class methodsFor: 'initialization' stamp: 'jmv 5/5/2019 14:33:28'!
defaultFamilyName: aString
	DefaultFamilyName _ aString! !

!FontFamily class methodsFor: 'initialization' stamp: 'jmv 5/5/2019 14:33:45'!
defaultPointSize: aNumber
	DefaultPointSize _ aNumber! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 5/5/2019 14:34:40'!
setSystemFontTo: aFont
	"Establish the default text font and style"

	aFont ifNil: [^ self].
	FontFamily defaultFamilyName: aFont familyName.
	FontFamily defaultPointSize: aFont pointSize.! !

!methodRemoval: AbstractFont class #default:!
AbstractFont class removeSelector: #default:!
