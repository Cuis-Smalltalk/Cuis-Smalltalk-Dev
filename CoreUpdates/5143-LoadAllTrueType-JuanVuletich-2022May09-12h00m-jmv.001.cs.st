'From Cuis 6.0 [latest update: #5145] on 9 May 2022 at 12:02:54 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 5/9/2022 12:02:27'!
loadOnlyLatinGlyphData
	^ self
		valueOfFlag: #loadOnlyLatinGlyphData
		ifAbsent: [false]! !

