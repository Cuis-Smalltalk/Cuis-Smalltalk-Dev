'From Cuis7.5 [latest update: #7617] on 26 September 2025 at 3:56:06 pm'!

!CharacterScanner methodsFor: 'private' stamp: 'jmv 9/26/2025 15:54:05'!
setFont
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	font := text fontAt: lastIndex default: defaultFont.
	attributes := text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText :backgroundColorInText :lineSpacingFactor |
			colorInText ifNotNil: [ self textColor: colorInText ].
			self backgroundColor: backgroundColorInText.
			alignment := alignmentInText.
			paragraphStyle := paragraphStyleInText.
			self lineSpacingFactor: lineSpacingFactor ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth := font widthOf: Character space.
	tabWidth := font pointSize * 3.! !


!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:15'!
alignmentAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ alignment ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:19'!
characterStyleOrNilAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ characterStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:25'!
characterStyleOrNilIfApplying: textAttributes
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
		^characterStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:31'!
colorAt: characterIndex
	"Answer the color for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ color ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:35'!
emphasisAt: characterIndex
	"Answer the emphasis for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ emphasis ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:40'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
		| font |
		font := defaultFont ifNil: [FontFamily defaultFamilyAndPointSize].
		familyName notNil | pointSize notNil ifTrue: [
			| fn ps |
			fn := familyName ifNil: [ defaultFont familyName ].
			ps := pointSize ifNil: [ defaultFont pointSize ].
			(FontFamily familyName: fn pointSize: ps) ifNotNil: [ :f |
				font := f emphasized: defaultFont emphasis]].
		^font emphasized: (defaultFont emphasis bitOr: emphasis) ].! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:45'!
paragraphStyleOrNilAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ paragraphStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/25/2025 18:06:50'!
paragraphStyleOrNilIfApplying: textAttributes
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self
		withAttributeValues: textAttributes
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^paragraphStyle ]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 9/26/2025 15:53:10'!
withAttributeValues: attributes do: aBlock
	"Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order
	The order is (each overwriting the previous one)
	1) basic defaults
	2) ParagraphStyleReferene
	3) CharacterStyleReference
	4) TextFontReference
	5) TextEmphasis"

	| paragraphStyle characterStyle familyName pointSize emphasis alignment color backgroundColor lineSpacingFactor |
	paragraphStyle := nil.
	characterStyle := nil.
	familyName := nil.
	pointSize := nil.
	emphasis := 0.
	alignment := nil.
	color := nil.
	backgroundColor := nil.
	lineSpacingFactor := Preferences at: #defaultLineSpacingFactor.
	
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
		attribute forFontFamilyDo: [ :fn | familyName := fn ].
		attribute forFontSizeDo: [ :ps | pointSize := ps ].
		attribute forTextEmphasisDo: [ :e | emphasis := emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color := c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor := c ].
		attribute forTextAlignmentDo: [ :a | alignment := a ].
	].
	
	"Done. Now evaluate the block."
	^
	aBlock numArgs = 8
		ifTrue: [ aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor } ]
		ifFalse: [ aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor. lineSpacingFactor } ]! !

