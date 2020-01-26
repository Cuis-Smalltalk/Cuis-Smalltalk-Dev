'From Cuis 5.0 [latest update: #4029] on 19 January 2020 at 2:37:19 pm'!

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 1/19/2020 14:06:22'!
useLeftArrow
	"Use left arrow glyph instead of underscore, and up arrow glyph instead of caret"
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 28.
	characterToGlyphMap at: 95 put: 30! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 1/19/2020 14:07:34'!
useRightArrow
	"Use right arrow glyph instead of underscore, and up arrow glyph instead of caret"
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 29.
	characterToGlyphMap at: 95 put: 30! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 1/19/2020 14:06:46'!
useUnderscore
	"Sets underscore and caret glyphs for chars 95 and 94. 
	ASCII standard glyphs"
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 95.
	characterToGlyphMap at: 95 put: 94! !

