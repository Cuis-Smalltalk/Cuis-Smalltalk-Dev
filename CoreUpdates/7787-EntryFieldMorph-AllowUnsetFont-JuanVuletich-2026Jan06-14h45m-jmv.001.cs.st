'From Cuis7.7 [latest update: #7785] on 6 January 2026 at 2:48:21 pm'!

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 6/Jan/2026 14:12:27'!
baseFont

	^font ifNil: [ FontFamily defaultFamilyAndPointSize ]! !

!methodRemoval: EntryFieldMorph #fontPreferenceChanged stamp: 'jmv 6/Jan/2026 14:47:59'!
EntryFieldMorph removeSelector: #fontPreferenceChanged!
!methodRemoval: EntryFieldMorph #font stamp: 'jmv 6/Jan/2026 14:47:59'!
EntryFieldMorph removeSelector: #font!
