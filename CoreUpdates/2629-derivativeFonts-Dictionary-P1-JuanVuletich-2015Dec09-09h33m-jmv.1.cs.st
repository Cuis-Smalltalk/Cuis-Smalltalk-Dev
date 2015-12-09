'From Cuis 4.2 of 25 July 2013 [latest update: #2628] on 9 December 2015 at 9:35:32.655666 am'!

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/9/2015 09:21'!
derivativeFont: aStrikeFontOrNil at: index

	| newDeriv |
	(aStrikeFontOrNil isNil and: [ index = 0 ]) 
		ifTrue: [
			derivativeFonts _ nil.
			^ self].

	self isBaseFont ifFalse: [ 
		derivativeFonts _ nil.
		self error: 'Derivative fonts can not have derivatives' ].
	
	derivativeFonts ifNil: [derivativeFonts := Array new: 128].
	derivativeFonts size < 128 ifTrue: [
		newDeriv := Array new: 128.
		newDeriv replaceFrom: 1 to: derivativeFonts size with: derivativeFonts.
		derivativeFonts := newDeriv.
	].
	derivativeFonts at: index put: aStrikeFontOrNil.
	aStrikeFontOrNil ifNotNil: [ aStrikeFontOrNil baseFont: self ]! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/9/2015 09:35'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [ derivativeFonts _ Dictionary new ].
	derivativeFonts class == Array ifTrue: [
		derivative _ derivativeFonts at: code ]
	ifFalse: [
		derivative _ derivativeFonts at: code ifAbsent: nil ].
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

