'From Cuis 4.2 of 25 July 2013 [latest update: #2624] on 8 December 2015 at 6:10:32.506781 pm'!

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 12/5/2015 23:09'!
bold
	^self emphasized: AbstractFont boldCode! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 12/5/2015 23:09'!
boldItalic
	^self emphasized: AbstractFont boldCode+AbstractFont italicCode! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 12/5/2015 23:09'!
italic
	^self emphasized: AbstractFont italicCode! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 12/5/2015 23:10'!
struckThrough
	^self emphasized: AbstractFont struckThroughCode! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 12/5/2015 23:10'!
underlined
	^self emphasized: AbstractFont underlinedCode! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/5/2015 23:28'!
emphasis: code 
	"Set the integer code for emphasis. See senders."

	emphasis _ code! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/5/2015 23:49'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [derivativeFonts := Array new: 128].
	derivativeFonts size < 128 ifTrue: [
		derivativeFonts := derivativeFonts, (Array new: 128-derivativeFonts size)].

	derivative _ derivativeFonts at: code.
	derivative ifNotNil: [^ derivative].  "Already have this style"

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

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/5/2015 23:51'!
reset
	"Reset the cache of derivative emphasized fonts"
	
	derivativeFonts ifNotNil: [
		derivativeFonts withIndexDo: [ :f :i |
			(f notNil and: [ f isSynthetic ]) 
				ifTrue: [ derivativeFonts at: i put: nil ]].
		"all after 16 are synthetic, and can be recreated on demand"
		derivativeFonts size > 15 ifTrue: [
			derivativeFonts _ derivativeFonts copyFrom: 1 to: 15 ]]! !

!StrikeFont methodsFor: 'copying' stamp: 'jmv 12/5/2015 23:14'!
postCopy
	characterToGlyphMap ifNotNil: [
		characterToGlyphMap _ characterToGlyphMap copy ]! !

!methodRemoval: StrikeFont #boldDerivative!
StrikeFont removeSelector: #boldDerivative!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
StrikeFont allInstancesDo: [:sf | sf reset]!

