'From Cuis 6.0 [latest update: #5796] on 8 May 2023 at 10:25:16 am'!

!Text methodsFor: 'emphasis' stamp: 'jmv 5/8/2023 10:24:56'!
withAttributeValues: attributes do: aBlock
	"Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order
	The order is (each overwriting the previous one)
	1) basic defaults
	2) ParagraphStyleReferene
	3) CharacterStyleReference
	4) TextFontReference
	5) TextEmphasis"

	| paragraphStyle characterStyle familyName pointSize emphasis alignment color backgroundColor |
	paragraphStyle := nil.
	characterStyle := nil.
	familyName := nil.
	pointSize := nil.
	emphasis := 0.
	alignment := nil.
	color := nil.
	backgroundColor := nil.
	
	"ParagraphStyle is the first to set several values"
	attributes do: [ :attribute |
		attribute forParagraphStyleReferenceDo: [ :s |
			paragraphStyle := s.
			familyName := s familyName.
			pointSize := s pointSize.
			emphasis := s emphasis.
			alignment := s alignment.
			s color ifNotNil: [ :c | color := c ]]].

	"CharacterStyle, if present, can override font and color"
	attributes do: [ :attribute |
		attribute forCharacterStyleReferenceDo: [ :s |
			characterStyle := s.
			familyName := s familyName.
			pointSize := s pointSize.
			emphasis := s emphasis.
			s color ifNotNil: [ :c | color := c ]]].

	"These will not interfere with each other, and all of them take precedence over previous values"
	attributes do: [ :attribute |
		attribute forFontFamilyAndSizeDo: [ :fn :ps | fn notNil ifTrue: [familyName := fn]. pointSize := ps ].
		attribute forTextEmphasisDo: [ :e | emphasis := emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color := c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor := c ].
		attribute forTextAlignmentDo: [ :a | alignment := a ].
	].
	
	"Done. Now evaluate the block."
	^aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor }! !

