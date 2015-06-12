'From Cuis 4.2 of 25 July 2013 [latest update: #2360] on 11 June 2015 at 12:58:44.423377 pm'!

!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 6/11/2015 12:56'!
buildLargerPunctuation
	"
	StrikeFont buildLargerPunctuation.
	StrikeFont useLargerPunctuation.

	To go back to standard glyphs:
		StrikeFont useRegularPunctuation
	"
	| form form2 f10 f11 f12 f9 |
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
	f9 takeGlyphFor: (Character value: 135) from: $, in: f12.
	form _ f9 glyphAt: (Character value: 134).
	form copy: (0@9 extent: 3@14) from: form to: 0@4 rule: Form and.
	f9 glyphAt: (Character value: 136) put: form.
	form _ f9 glyphAt: (Character value: 135).
	form2 _ f9 glyphAt: (Character value: 134).
	form copy: (0@9 extent: 3@14) from: form2 to: 1@4 rule: Form and.
	f9 glyphAt: (Character value: 137) put: form.

	f10 takeGlyphFor: (Character value: 134) from: $. in: f12.
	f10 takeGlyphFor: (Character value: 135) from: $, in: f12.
	form _ f10 glyphAt: (Character value: 134).
	form copy: (0@9 extent: 3@14) from: form to: 0@4 rule: Form and.
	f10 glyphAt: (Character value: 136) put: form.
	form _ f10 glyphAt: (Character value: 135).
	form2 _ f10 glyphAt: (Character value: 134).
	form copy: (0@9 extent: 3@14) from: form2 to: 1@4 rule: Form and.
	f10 glyphAt: (Character value: 137) put: form.

	f11 takeGlyphFor: (Character value: 134) from: $. in: f12.
	f11 takeGlyphFor: (Character value: 135) from: $, in: f12.
	f11 takeGlyphFor: (Character value: 136) from: $: in: f12.
	f11 takeGlyphFor: (Character value: 137) from: $; in: f12! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	StrikeFont buildLargerPunctuation.
	StrikeFont useLargerPunctuation.!

