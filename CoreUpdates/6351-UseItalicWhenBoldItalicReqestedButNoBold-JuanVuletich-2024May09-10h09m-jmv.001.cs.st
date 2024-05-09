'From Cuis6.3 [latest update: #6350] on 9 May 2024 at 10:50:24 am'!

!TrueTypeFont methodsFor: 'emphasis' stamp: 'jmv 5/9/2024 10:48:14'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
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

