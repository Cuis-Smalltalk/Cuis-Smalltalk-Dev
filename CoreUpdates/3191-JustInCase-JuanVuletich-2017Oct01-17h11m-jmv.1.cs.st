'From Cuis 5.0 of 7 November 2016 [latest update: #3190] on 1 October 2017 at 5:11:37 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 10/1/2017 17:11:24'!
defaultFontFamily
	"Answer the default font family name"

	^self parameters at: #defaultFontFamily ifAbsentPut: [ AbstractFont familyNames first ]! !

