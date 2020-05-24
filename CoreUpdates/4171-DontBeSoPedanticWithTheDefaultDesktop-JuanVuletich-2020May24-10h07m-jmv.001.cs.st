'From Cuis 5.0 [latest update: #4170] on 24 May 2020 at 10:08:42 am'!

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 5/24/2020 10:07:38'!
fontPreferenceChanged
	self submorphsDo: [ :m |
		m morphExtent: (m morphExtent max: m minimumExtent).
		m fontPreferenceChanged ]! !

