'From Cuis 5.0 [latest update: #4172] on 24 May 2020 at 10:50:45 am'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 5/24/2020 10:36:55'!
loadOnlyLatinGlyphData
	^ self
		valueOfFlag: #loadOnlyLatinGlyphData
		ifAbsent: [true]! !


!FontFamily methodsFor: 'caching' stamp: 'jmv 5/24/2020 10:46:19'!
releaseCachedState

	baseFontBySizes keysAndValuesDo: [ :size :font |
		font releaseCachedState ]! !


!FontFamily class methodsFor: 'cached state access' stamp: 'jmv 5/24/2020 10:46:40'!
releaseClassCachedState

	AvailableFamilies keysAndValuesDo: [ :familyName :fontFamily |
		fontFamily releaseCachedState ]! !

