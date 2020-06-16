'From Cuis 5.0 [latest update: #4224] on 16 June 2020 at 7:41:14 pm'!

!Text methodsFor: 'emphasis' stamp: 'jmv 6/16/2020 19:36:42'!
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
		attribute forFontFamilyAndSizeDo: [ :fn :ps | fn notNil ifTrue: [familyName _ fn]. pointSize _ ps ].
		attribute forTextEmphasisDo: [ :e | emphasis _ emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color _ c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor _ c ].
		attribute forTextAlignmentDo: [ :a | alignment _ a ].
	].
	
	"Done. Now evaluate the block."
	^aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor }! !


!TextFontFamilyAndSize methodsFor: 'accessing' stamp: 'jmv 6/16/2020 19:37:03'!
familyName: aStringOrNil pointSize: aNumber
	familyName _ aStringOrNil.
	pointSize _ aNumber! !

!TextFontFamilyAndSize methodsFor: 'accessing' stamp: 'jmv 6/16/2020 19:35:53'!
font

	^familyName isNil
		ifTrue: [ FontFamily defaultFamilyPointSize: pointSize]
		ifFalse: [ FontFamily familyName: familyName pointSize: pointSize ]! !


!TextFontFamilyAndSize class methodsFor: 'instance creation' stamp: 'jmv 6/16/2020 19:39:55'!
pointSize: aNumber
	"Reference only default family baseFont. Any emphasis should be done with TextEmphasis.
	Store only pointSize"
	^ self new familyName: nil pointSize: aNumber! !

