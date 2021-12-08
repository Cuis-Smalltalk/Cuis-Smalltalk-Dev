'From Cuis 5.0 [latest update: #4902] on 7 October 2021 at 5:32:15 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 10/7/2021 17:30:56'!
cacheTrueTypeGlyphs
	"Provides significant performance improvements for text if not rotated or scaled.
	Placement of each character is rounded to integer coordinates: text layout is not perfect."

	^ self
		valueOfFlag: #cacheTrueTypeGlyphs
		ifAbsent: [ true ].! !

