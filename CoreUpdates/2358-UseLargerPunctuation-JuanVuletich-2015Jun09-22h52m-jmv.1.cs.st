'From Cuis 4.2 of 25 July 2013 [latest update: #2356] on 9 June 2015 at 10:59:09.23263 pm'!

!StrikeFont methodsFor: 'character shapes' stamp: 'jmv 6/9/2015 22:57'!
takeGlyphFor: aCharacter from: sourceCharacter in: aFont
	"Copy characterForm over the glyph for the argument, character."
	| f r characterForm |
	characterForm _ aFont glyphAt: sourceCharacter.
	r _ 0@(0 + aFont ascent - self ascent) extent: characterForm width @ glyphs height.
	f _ characterForm copy: r.
	self glyphAt: aCharacter put: f! !

!StrikeFont methodsFor: 'character shapes' stamp: 'jmv 6/9/2015 22:16'!
useLargerPunctuation
	self characterToGlyphMap.
	characterToGlyphMap at: 47 put: 134.
	characterToGlyphMap at: 45 put: 135.
	characterToGlyphMap at: 59 put: 136.
	characterToGlyphMap at: 60 put: 137! !

!StrikeFont methodsFor: 'character shapes' stamp: 'jmv 6/9/2015 22:15'!
useRegularPunctuation
	self characterToGlyphMap.
	characterToGlyphMap at: 47 put: 46.
	characterToGlyphMap at: 45 put: 44.
	characterToGlyphMap at: 59 put: 58.
	characterToGlyphMap at: 60 put: 59! !


!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 6/9/2015 22:19'!
useLargerPunctuation
	"Sets larger sized glyphs for . , : ; for selected font sizes, to enhance legibility
	Classic Smalltalk-80 glyphs"
	"
	StrikeFont useLargerPunctuation
	"
	self allInstances do: [ :font | font useLargerPunctuation ]! !

!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 6/9/2015 22:18'!
useRegularPunctuation
	"Sets regular sized glyphs for . , : ;
	Classic Smalltalk-80 glyphs"
	"
	StrikeFont useRegularPunctuation
	"
	self allInstances do: [ :font | font useRegularPunctuation ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

| f9 f10 f11 f12 |

StrikeFont allInstancesDo: [ :font |
	'.,:;' doWithIndex: [ :char :i |
		font
			glyphAt: (Character value: 133+i)
			put: (font glyphAt: char ) ]].

f9 _ AbstractFont familyName: 'DejaVu' pointSize: 9.
f10 _ AbstractFont familyName: 'DejaVu' pointSize: 10.
f11 _ AbstractFont familyName: 'DejaVu' pointSize: 11.
f12 _ AbstractFont familyName: 'DejaVu' pointSize: 12.

f9 takeGlyphFor: (Character value: 134) from: $. in: f12.
f10 takeGlyphFor: (Character value: 134) from: $. in: f12.
f11 takeGlyphFor: (Character value: 134) from: $. in: f12.

f9 takeGlyphFor: (Character value: 135) from: $, in: f12.
f10 takeGlyphFor: (Character value: 135) from: $, in: f12.
f11 takeGlyphFor: (Character value: 135) from: $, in: f12.

f9 takeGlyphFor: (Character value: 136) from: $: in: f12.
f10 takeGlyphFor: (Character value: 136) from: $: in: f12.

f9 takeGlyphFor: (Character value: 137) from: $; in: f12.
f10 takeGlyphFor: (Character value: 137) from: $; in: f12.

StrikeFont useLargerPunctuation
!

