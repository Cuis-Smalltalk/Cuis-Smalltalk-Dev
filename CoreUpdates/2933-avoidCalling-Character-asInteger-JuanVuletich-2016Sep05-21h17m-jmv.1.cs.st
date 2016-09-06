'From Cuis 4.2 of 25 July 2013 [latest update: #2929] on 5 September 2016 at 10:20:33 pm'!

!Character class methodsFor: 'converting' stamp: 'jmv 9/5/2016 22:11:36'!
nextUnicodeCodePointFromUtf8: anUtf8Stream
	"anUtf8Stream can be over a ByteArray
	Answer nil if conversion not possible, because of invalid UTF-8.
	Also answer nil for codePoint U+FEFF (BOM, unneededly added by Win clipboard)"

	| byte1 byte2 byte3 byte4 codePoint |
	byte1 _ anUtf8Stream next.
	byte1 < 128 ifTrue: [	"single byte"
		^byte1 ].
	
	"At least 2 bytes"
	byte2 _ anUtf8Stream next.
	(byte2 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rE0) = 192 ifTrue: [ "two bytes"
		^ ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63) ].
	
	"At least 3 bytes"
	byte3 _ anUtf8Stream next.
	(byte3 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF0) = 224 ifTrue: [ "three bytes"
		codePoint _  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
		^codePoint = 16rFEFF
			ifFalse: [ codePoint ]
			ifTrue: [ nil ]].

	"4 bytes"
	byte4 _ anUtf8Stream next.
	(byte4 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF8) = 240 ifTrue: [  "four bytes"
		^((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12) + ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63) ].

	^nil! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/5/2016 22:15:35'!
alphabeticalMorphMenu
	| list splitLists menu firstChar lastChar subMenu |
	list := Morph withAllSubclasses select: [:m | m includeInNewMorphMenu].
	list := list asArray sort: [:c1 :c2 | c1 name < c2 name].
	splitLists := self splitNewMorphList: list depth: 3.
	menu := MenuMorph new defaultTarget: self.
	1 to: splitLists size
		do: 
			[:i | 
			firstChar := i = 1 
				ifTrue: [$A]
				ifFalse: 
					[((splitLists at: i - 1) last name first numericValue + 1) 
								asCharacter].
			lastChar := i = splitLists size 
						ifTrue: [$Z]
						ifFalse: [(splitLists at: i) last name first].
			subMenu := MenuMorph new.
			(splitLists at: i) do: 
					[:cl | 
					subMenu 
						add: cl name
						target: self
						selector: #newMorphOfClass:event:
						argument: cl].
			menu add: firstChar asString , ' - ' , lastChar asString subMenu: subMenu].
	^menu! !

