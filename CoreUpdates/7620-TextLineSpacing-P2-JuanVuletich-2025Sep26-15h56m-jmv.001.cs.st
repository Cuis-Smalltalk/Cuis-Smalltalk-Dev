'From Cuis7.5 [latest update: #7617] on 26 September 2025 at 4:06:36 pm'!
!classDefinition: #TextLineSpacing category: #'System-Text-Attributes'!
TextAttribute subclass: #TextLineSpacing
	instanceVariableNames: 'lineSpacingFactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text-Attributes'!

!TextLineSpacing commentStamp: 'jmv 9/26/2025 16:05:23' prior: 0!
(
(Text string: 'This text has no style set', String newLineString),
(Text string: 'This is has big line spacing', String newLineString attribute: (TextLineSpacing factor: 2.4)),
(Text string: 'This is has big line spacing', String newLineString attribute: (TextLineSpacing factor: 2.4)),
(Text string: 'This text has no style set', String newLineString)
) edit!


!TextAttribute methodsFor: 'iterating' stamp: 'jmv 9/25/2025 18:11:45'!
forLineSpacingDo: aBlock
	"No action is the default"! !


!TextLineSpacing methodsFor: 'accessing' stamp: 'jmv 9/26/2025 15:59:48'!
lineSpacingFactor
	^lineSpacingFactor! !

!TextLineSpacing methodsFor: 'accessing' stamp: 'jmv 9/26/2025 15:59:57'!
lineSpacingFactor: aNumber
	lineSpacingFactor := aNumber! !

!TextLineSpacing methodsFor: 'comparing' stamp: 'jmv 9/26/2025 16:03:13'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other lineSpacingFactor = lineSpacingFactor]! !

!TextLineSpacing methodsFor: 'comparing' stamp: 'jmv 9/26/2025 16:02:56'!
dominates: other
	^ other class == self class! !

!TextLineSpacing methodsFor: 'comparing' stamp: 'jmv 9/26/2025 16:03:22'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^ lineSpacingFactor hash! !

!TextLineSpacing methodsFor: 'iterating' stamp: 'jmv 9/26/2025 16:04:13'!
forLineSpacingDo: aBlock
	aBlock value: lineSpacingFactor! !

!TextLineSpacing methodsFor: 'testing' stamp: 'jmv 9/26/2025 16:04:21'!
isParagraphAttribute
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^true! !


!TextLineSpacing class methodsFor: 'instance creation' stamp: 'jmv 9/26/2025 16:00:46'!
factor: aNumber
	^self new lineSpacingFactor: aNumber! !


!Text methodsFor: 'emphasis' stamp: 'jmv 9/26/2025 15:57:23'!
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
		attribute forLineSpacingDo: [ :f | lineSpacingFactor := f ].
	].
	
	"Done. Now evaluate the block."
	^ aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor. lineSpacingFactor }! !


!TextLineSpacing reorganize!
('accessing' lineSpacingFactor lineSpacingFactor:)
('comparing' = dominates: hash)
('iterating' forLineSpacingDo:)
('testing' isParagraphAttribute)
!


!TextLineSpacing class reorganize!
('instance creation' factor:)
!

