'From Cuis 5.0 [latest update: #4882] on 24 September 2021 at 10:19:33 am'!

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 9/24/2021 10:03:42'!
enableTrueTypeFontsOnly

	AvailableFamilies _ AvailableFamilies select: [ :f | f isTrueTypeFontFamily ].! !

