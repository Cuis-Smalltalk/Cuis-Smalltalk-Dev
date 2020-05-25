'From Cuis 5.0 [latest update: #4171] on 24 May 2020 at 10:21:53 am'!

!Preferences class methodsFor: 'shout' stamp: 'len 5/22/2020 20:10:48'!
useAlwaysLeftArrow
	"
	Preferences useAlwaysLeftArrow
	"
	Preferences setPreference: #assignmentGlyphSelector toValue: #useAlwaysLeftArrow.
	AbstractFont withAllSubclassesDo: [ :fontClass | fontClass assignmentGlyphSelectorPreferenceChanged ].! !


!StrikeFont methodsFor: 'glyphs' stamp: 'len 5/22/2020 20:09:51'!
useAlwaysLeftArrow
	^ self useLeftArrow! !

