'From Cuis 5.0 [latest update: #4214] on 11 June 2020 at 10:33:07 am'!

!Text methodsFor: 'emphasis' stamp: 'jmv 6/11/2020 10:32:11'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	| fn ps |
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		fn _ familyName ifNil: [ defaultFont ifNotNil: [ defaultFont familyName ] ifNil: [ FontFamily defaultFamilyName ]].
		ps _ pointSize ifNil: [ defaultFont ifNotNil: [ defaultFont pointSize ] ifNil: [ FontFamily defaultPointSize ]].
		^((FontFamily familyName: fn pointSize: ps) ifNil: [ FontFamily defaultFamilyAndPointSize ])
			emphasized: emphasis ]! !


!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 6/10/2020 23:17:59'!
fromUser
	"
	 AbstractFont fromUser
	"
	^self fromUser: FontFamily defaultFamilyAndPointSize ! !


!StrikeFont methodsFor: 'objects from disk' stamp: 'jmv 6/11/2020 10:30:27'!
objectForDataStream: refStrm

	"I am about to be written on an object file.  Write a textual reference instead.
	Warning: This saves a lot of space, but might fail if using other fonts than those in AvailableFonts"

	^ DiskProxy
		global: #FontFamily
		selector: #familyName:pointSize:
		args: (Array with: self familyName with: self pointSize)! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 6/11/2020 10:30:32'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((FontFamily familyName: self familyName pointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative useShortUnderscore.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 6/11/2020 10:30:38'!
syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ ((FontFamily familyName: self familyName pointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative name: self name , 'Sup'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !


!FontFamily methodsFor: 'accessing' stamp: 'jmv 6/11/2020 10:29:28'!
atPointSize: aNumber
"
	^baseFontBySizes at: aNumber ifAbsent: nil
"
	^baseFontBySizes at: aNumber ifAbsent: [ |found |
		found _ nil.
		baseFontBySizes do: [ :font |
			(found isNil or: [ (found pointSize - aNumber) abs > (font pointSize - aNumber) abs ])
				ifTrue: [ found _ font ]].
			found
		]! !

!methodRemoval: FontFamily class #familyName:aroundPointSize: stamp: 'jmv 6/11/2020 10:31:03'!
FontFamily class removeSelector: #familyName:aroundPointSize:!
!methodRemoval: FontFamily #aroundPointSize: stamp: 'jmv 6/11/2020 10:29:56'!
FontFamily removeSelector: #aroundPointSize:!
!methodRemoval: Preferences class #setDefaultFonts: stamp: 'jmv 6/11/2020 10:28:25'!
Preferences class removeSelector: #setDefaultFonts:!
