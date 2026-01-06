'From Cuis7.7 [latest update: #7784] on 6 January 2026 at 2:33:25 pm'!

!TaskbarMorph methodsFor: 'notifications' stamp: 'jmv 6/Jan/2026 14:32:49'!
fontPreferenceChanged
	clock font: (Preferences at: #windowTitleFont).
	self scale: self scale.! !

