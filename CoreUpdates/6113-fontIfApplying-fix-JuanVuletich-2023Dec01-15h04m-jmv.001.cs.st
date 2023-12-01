'From Cuis6.0 [latest update: #6111] on 1 December 2023 at 3:05:42 pm'!

!Text methodsFor: 'emphasis' stamp: 'jmv 12/1/2023 15:01:53'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	| font fn ps |
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		font := defaultFont ifNil: [FontFamily defaultFamilyAndPointSize].
		familyName notNil | pointSize notNil ifTrue: [
			fn := familyName ifNil: [ defaultFont familyName ].
			ps := pointSize ifNil: [ defaultFont pointSize ].
			(FontFamily familyName: fn pointSize: ps) ifNotNil: [ :f |
				font := f emphasized: defaultFont emphasis]].
		^font 	emphasized: (defaultFont emphasis bitOr: emphasis) ].! !

