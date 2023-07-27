'From Haver 6.0 [latest update: #5950] on 27 July 2023 at 12:18:49 pm'!

!Editor methodsFor: 'typing/selecting keys' stamp: 'KLG 7/27/2023 12:16:57'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |
	"Convert strings like '\+161 ' (š) to unicode"
	aKeyboardEvent isSpace ifTrue: [ | string |
		string := self privateCurrentString.
		self pointIndex - 1 to: (self pointIndex - 7 max: 1) by: -1 do: [ :index |
		string at: index :: = $\ ifTrue: [ | key |
			key := string copyFrom: index + 1 to: self pointIndex - 1.
			key size > 2 and: [ key at: 1 :: = $+ ] ::
				ifTrue: [ | stream number value |
					stream := ReadStream on: key asUppercase from: 2 to: key size.
					[ number := Integer readFrom: stream  base: 16 ] on: Error do: [ ^ false ].
					value := UnicodeCodePoint codePoint: number.
					self
						selectFrom: index to: self pointIndex - 1;
						replaceSelectionWith: (UnicodeString with: value).
						^ false ] ] ] ].
	character := aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsability to do that in Linux."
	character isDiacriticalMark ifTrue: [
		i := self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) = $  ]) ifTrue: [
			replacement := character caseOf: {
				[ `UnicodeCodePoint codePoint: 16r300` ] -> [ '`' ].
				[ `UnicodeCodePoint codePoint: 16r301` ] -> [ '''' ].
				[ `UnicodeCodePoint codePoint: 16r302` ] -> [ '^' ].
				[ `UnicodeCodePoint codePoint: 16r303` ] -> [ '~' ].
				[ `UnicodeCodePoint codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false! !

