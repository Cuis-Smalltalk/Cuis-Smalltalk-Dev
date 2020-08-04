'From Cuis 5.0 [latest update: #4332] on 3 August 2020 at 7:04:29 pm'!

!FontFamily methodsFor: 'testing' stamp: 'jmv 8/3/2020 19:00:28'!
isTrueTypeFontFamily
	^false! !


!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 8/3/2020 19:01:01'!
defaultOrTrueTypeFamilyAndPointSize
	"
	FontFamily defaultOrTrueTypeFamilyAndPointSize
	"
	| family |
	family _ AvailableFamilies at: DefaultFamilyName.
	family isTrueTypeFontFamily ifFalse: [
		family _ AvailableFamilies detect: [ :any | any isTrueTypeFontFamily ] ifNone: [ ^nil ]].
	^family atPointSize: DefaultPointSize ! !


!FontFamily reorganize!
('initialization' familyName:)
('accessing' atPointSize: familyName folderName pointSizes)
('caching' releaseCachedState)
('testing' isTrueTypeFontFamily)
!

