'From Cuis 4.2 of 25 July 2013 [latest update: #2634] on 13 December 2015 at 10:46:45.350928 am'!

!Preferences class methodsFor: 'standard queries'!
assignmentGlyphSelector
	^ self
		valueOfFlag: #assignmentGlyphSelector
		ifAbsent: [ #useRightArrow ].! !

!Preferences class methodsFor: 'shout' stamp: 'jmv 12/13/2015 10:44'!
useAssignmentGlyphLeftArrow
	"
	Preferences useAssignmentGlyphLeftArrow
	"
	Preferences setPreference: #assignmentGlyphSelector toValue: #useLeftArrow.
	StrikeFont allInstancesDo: [ :font | font reset ]! !

!Preferences class methodsFor: 'shout' stamp: 'jmv 12/13/2015 10:44'!
useAssignmentGlyphRightArrow
	"
	Preferences useAssignmentGlyphRightArrow
	"
	Preferences setPreference: #assignmentGlyphSelector toValue: #useRightArrow.
	StrikeFont allInstancesDo: [ :font | font reset ]! !


!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/13/2015 10:45'!
syntheticST80Glyphs
	"Build and answer a derivative that includes ST-80 glyphs:
	a left arrow instead of the underscore, and an up arrow instead of the caret."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'ST80';
		perform: Preferences assignmentGlyphSelector.
	^ derivative! !


!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 12/13/2015 10:34'!
useUnderscore
	"Sets underscore and caret glyphs for chars 95 and 94. 
	ASCII standard glyphs"
	"
	StrikeFont allInstancesDo: [ :font | font reset ]. StrikeFont useUnderscore
	"
	self allInstances do: [ :font | font useUnderscore ]! !

!methodRemoval: StrikeFont class #useLeftArrow!
StrikeFont class removeSelector: #useLeftArrow!
!methodRemoval: StrikeFont class #useRightArrow!
StrikeFont class removeSelector: #useRightArrow!
