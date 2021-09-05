'From Cuis 5.0 [latest update: #4837] on 5 September 2021 at 7:51:29 pm'!

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/5/2021 18:38:10'!
fontPreferenceChanged

	super fontPreferenceChanged.
	scrollBar recreateSubmorphs.
	hScrollBar recreateSubmorphs.
	self setScrollDeltas.! !

!methodRemoval: ScrollBar #fontPreferenceChanged stamp: 'jmv 9/5/2021 19:50:06'!
ScrollBar removeSelector: #fontPreferenceChanged!
