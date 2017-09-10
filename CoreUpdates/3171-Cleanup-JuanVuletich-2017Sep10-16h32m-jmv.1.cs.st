'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 10 September 2017 at 4:36:05 pm'!

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:32:21'!
alignmentAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ alignment ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:33:17'!
characterStyleOrNilAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ characterStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:33:23'!
characterStyleOrNilIfApplying: textAttributes
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		^characterStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:33:30'!
emphasisAt: characterIndex
	"Answer the emphasis for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ emphasis ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:33:36'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		^((AbstractFont familyName: familyName pointSize: pointSize) ifNil: [ defaultFont baseFont ])
			emphasized: emphasis ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:33:42'!
paragraphStyleOrNilAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ paragraphStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:33:48'!
paragraphStyleOrNilIfApplying: textAttributes
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self
		withAttributeValues: textAttributes
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^paragraphStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:34:25'!
withAttributeValues: attributes do: aBlock
	"Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order
	The order is (each overwriting the previous one)
	1) basic defaults
	2) ParagraphStyleReferene
	3) CharacterStyleReference
	4) TextFontReference
	5) TextEmphasis"

	| paragraphStyle characterStyle familyName pointSize emphasis alignment color backgroundColor |
	paragraphStyle _ nil.
	characterStyle _ nil.
	familyName _ nil.
	pointSize _ nil.
	emphasis _ 0.
	alignment _ 0.
	color _ nil.
	backgroundColor _ nil.
	
	"ParagraphStyle is the first to set several values"
	attributes do: [ :attribute |
		attribute forParagraphStyleReferenceDo: [ :s |
			paragraphStyle _ s.
			familyName _ s familyName.
			pointSize _ s pointSize.
			emphasis _ s emphasis.
			alignment _ s alignment.
			s color ifNotNil: [ :c | color _ c ]]].

	"CharacterStyle, if present, can override font and color"
	attributes do: [ :attribute |
		attribute forCharacterStyleReferenceDo: [ :s |
			characterStyle _ s.
			familyName _ s familyName.
			pointSize _ s pointSize.
			emphasis _ s emphasis.
			s color ifNotNil: [ :c | color _ c ]]].

	"These will not interfere with each other, and all of them take precedence over previous values"
	attributes do: [ :attribute |
		attribute forFontFamilyAndSizeDo: [ :fn :ps | familyName _ fn. pointSize _ ps ].
		attribute forTextEmphasisDo: [ :e | emphasis _ emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color _ c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor _ c ].
		attribute forTextAlignmentDo: [ :a | alignment _ a ].
	].
	
	"Done. Now evaluate the block."
	^aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor }! !

