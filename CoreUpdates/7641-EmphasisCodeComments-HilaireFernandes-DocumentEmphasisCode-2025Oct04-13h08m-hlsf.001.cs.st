'From Cuis7.5 [latest update: #7621] on 4 October 2025 at 1:11:26 pm'!

!TrueTypeFont methodsFor: 'emphasis' stamp: 'hlsf 10/4/2025 13:10:01'!
addEmphasis: code
"	Set the integer code for emphasis.
	See AbstractFont, 'constants' class category."

	emphasis := code.
	(emphasis anyMask: 4+8) ifTrue: [
		"They need to be recreated"
		glyphFormsByUtf8 := Array new: 256 ].! !

!TrueTypeFont methodsFor: 'emphasis' stamp: 'hlsf 10/4/2025 13:11:14'!
emphasized: code
"	Argument is one of the emphases defined in AbstractFont, 'constants' class category.
	Codes can be combined by OR-ing them.
	Add the requested emphasis to those we may already have.
"
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [ derivativeFonts := Dictionary new ].
	derivative := derivativeFonts at: code ifPresent: [ :der | ^ der ].		"Already have this style"

	"Bold+Italic requested but not available.
	Handle this case before decomposing, because neither can be built synthetic.
	Answer Italic if present.
	Otherwise answer Bold if present. Or just the base font."
	code = 3 ifTrue: [
		derivativeFonts at: 2 ifPresent: [ :der | ^ der ].
		derivativeFonts at: 1 ifPresent: [ :der | ^ der ].
		^self baseFont ].
	"Bold or Italic requested but not available. Just use base font."
	code < 3 ifTrue: [
		^self baseFont ].

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis := 1 bitShift: code highBit - 1.
	derivedFrom := self emphasized: code - addedEmphasis.

	derivative := addedEmphasis caseOf: {
		[ 4 ] -> [ derivedFrom copy ].  												"Underlined(U)"
		[ 8 ] -> [ derivedFrom copy ].  												"StruckThrough(X)"
		[ 16 ] -> [ derivedFrom syntheticSuperscript ].  												"Superscript(Sup)"
		[ 32 ] -> [ derivedFrom syntheticSubscript ].  												"Subscript(Sub)"
	}.

	derivative baseFont: self.
	derivative addEmphasis: code.
	derivativeFonts at: code put: derivative.
	^ derivative! !

