'From Cuis 5.0 [latest update: #4071] on 21 March 2020 at 10:32:12 am'!

!AbstractFont class methodsFor: 'initialization' stamp: 'jmv 3/21/2020 10:30:25'!
assignmentGlyphSelectorPreferenceChanged
	"Subclasses should adjust their instances accordingly."! !


!StrikeFont class methodsFor: 'initialization' stamp: 'jmv 3/21/2020 10:18:08'!
assignmentGlyphSelectorPreferenceChanged
	self allInstancesDo: [ :each | each reset ]! !


!Preferences class methodsFor: 'shout' stamp: 'jmv 3/21/2020 10:30:40'!
useAssignmentGlyphLeftArrow
	"
	Preferences useAssignmentGlyphLeftArrow
	"
	Preferences setPreference: #assignmentGlyphSelector toValue: #useLeftArrow.
	AbstractFont withAllSubclassesDo: [ :fontClass | fontClass assignmentGlyphSelectorPreferenceChanged ].! !

!Preferences class methodsFor: 'shout' stamp: 'jmv 3/21/2020 10:30:45'!
useAssignmentGlyphRightArrow
	"
	Preferences useAssignmentGlyphRightArrow
	"
	Preferences setPreference: #assignmentGlyphSelector toValue: #useRightArrow.
	AbstractFont withAllSubclassesDo: [ :fontClass | fontClass assignmentGlyphSelectorPreferenceChanged ].! !

