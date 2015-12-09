'From Cuis 4.2 of 25 July 2013 [latest update: #2628] on 9 December 2015 at 9:43:08.952808 am'!

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/9/2015 09:40'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [ derivativeFonts _ Dictionary new ].
	derivative _ derivativeFonts at: code ifPresent: [ :der | ^ der ].		"Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis _ 1 bitShift: code highBit - 1.
	derivedFrom _ self emphasized: code - addedEmphasis.

	"Order is Bold(B), Italic(i), Underlined(U), StruckThrough(X), Superscript(Sup), Subscript(Sub), WithST80Glyphs(ST80)"
	derivative _ addedEmphasis caseOf: {
		[ 1 ] -> [ derivedFrom syntheticBold ].
		[ 2 ] -> [ derivedFrom syntheticItalic ].
		[ 4 ] -> [ derivedFrom syntheticUnderlined ].
		[ 8 ] -> [ derivedFrom syntheticStruckThrough ].
		[ 16 ] -> [ derivedFrom syntheticSuperscript ].
		[ 32 ] -> [ derivedFrom syntheticSubscript ].
		[ 64 ] -> [ derivedFrom syntheticST80Glyphs ] }.

	derivative baseFont: self.
	derivative emphasis: code.
	derivativeFonts at: code put: derivative.
	^ derivative! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/9/2015 09:41'!
reset
	"Reset the cache of derivative emphasized fonts
	StrikeFont allInstancesDo: [ :f | f reset ]
	"
	
	| newDict |
	derivativeFonts ifNotNil: [
		newDict _ Dictionary new.
		"all after 16 are synthetic, and can be recreated on demand"
		1 to: 15 do: [ :i |
			derivativeFonts at: i ifPresent: [ :der |
				newDict at: i put: der ]].
		derivativeFonts _ newDict ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	StrikeFont allInstancesDo: [ :f | f reset ]!

