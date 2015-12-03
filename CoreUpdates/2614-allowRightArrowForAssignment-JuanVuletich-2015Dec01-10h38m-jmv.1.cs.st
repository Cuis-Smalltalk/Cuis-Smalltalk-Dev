'From Cuis 4.2 of 25 July 2013 [latest update: #2613] on 1 December 2015 at 10:38:20.581137 am'!

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 12/1/2015 10:33'!
useRightArrow
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 29.
	characterToGlyphMap at: 95 put: 30! !


!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 12/1/2015 10:33'!
useRightArrow
	"Sets right arrow for asignment (char 95) and up arrow for return (char 94).
	A variation on the classic Smalltalk-80 glyphs"
	"
	StrikeFont useRightArrow
	"
	self allInstances do: [ :font | font useRightArrow ]! !

