'From Cuis7.5 [latest update: #7720] on 27 November 2025 at 6:01:26 pm'!

!Text methodsFor: 'emphasis' stamp: 'FJG 27/Nov/2025 17:53:19'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
		| font |
		font := defaultFont ifNil: [FontFamily defaultFamilyAndPointSize].
		familyName notNil | pointSize notNil ifTrue: [
			| fn ps |
			fn := familyName ifNil: [ font familyName ].
			ps := pointSize ifNil: [ font pointSize ].
			(FontFamily familyName: fn pointSize: ps) ifNotNil: [ :f |
				font := f emphasized: font emphasis]].
		^font emphasized: (font emphasis bitOr: emphasis) ].! !

