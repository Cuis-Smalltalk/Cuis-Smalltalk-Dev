"
A UnicodeString is an indexed collection of Characters. In Cuis, Characters are Unicode Code Points. See also String.

Based on https://github.com/svenvc/UTF8String by Sven Van Caekenberghe. Good ideas and implementation. Thanks Sven!

Play with the class side demo methods.
"
Class {
	#name : 'UnicodeString',
	#superclass : 'CharacterSequence',
	#instVars : [
		'bytes',
		'count',
		'strideIndex',
		'hash'
	],
	#category : 'Kernel-Text',
	#package : 'Kernel',
	#tag : 'Text'
}

{ #category : 'services' }
UnicodeString class >> codePointsFromUtf8: aByteArray [
	"Convert the given string from UTF-8 to UTF-32"

	^WordArray streamContents: [ :strm | | s byteIndex n |
		s := aByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					strm nextPut: codePoint ].
			byteIndex := byteIndex + n
		]].
]

{ #category : 'services' }
UnicodeString class >> compare: string1 with: string2 [
	"Return 1, 2 or 3, if string1 is <, =, or > string2.
	Case sensitive.
	Can handle any kind of Character sequences.
	Slower than implementation in String class"

	| len1 len2 c1 c2 |

	len1 := string1 size.
	len2 := string2 size.
	1 to: (len1 min: len2) do: [ :i |
		c1 := string1 at: i.
		c2 := string2 at: i.
		c1 = c2 ifFalse: [
			c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
]

{ #category : 'instance creation' }
UnicodeString class >> crString [
	"Answer a string containing a carriage return.
	UnicodeString crString
	"
	^ `UnicodeString with: Character cr`
]

{ #category : 'instance creation' }
UnicodeString class >> crlfString [
	"Answer a string containing a carriage return and a linefeed.
	UnicodeString crlfString
	"
	^ `UnicodeString with: Character cr with: Character lf`
]

{ #category : 'unicode demo' }
UnicodeString class >> demoAncientEgyptian [
	"
	UnicodeString demoAncientEgyptian.
	"
	| utf8 fontFamily |
	utf8 :=  #[240 147 128 128 240 147 133 184 240 147 137 128 240 147 144 174 240 147 128 144 240 147 128 145 240 147 128 146 32 240 147 131 176 240 147 131 177 240 147 131 178 240 147 131 179 240 147 131 180 240 147 131 181 240 147 131 182 32 240 147 130 128 32 240 147 133 184 240 147 133 185 240 147 133 186 240 147 133 187 32 240 147 133 189 32 240 147 133 190 240 147 133 191 32 240 147 137 160 240 147 137 161 240 147 137 162 240 147 137 163 240 147 137 164 32 240 147 134 178 240 147 129 183 240 147 143 164 240 147 142 159 240 147 128 128 240 147 129 144 240 147 143 165 240 147 131 128 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 133 177 240 147 128 148 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 135 139 240 147 135 139 240 147 143 143 240 147 129 144 240 147 144 141 240 147 130 139 240 147 139 180 240 147 130 157 240 147 142 155 240 147 139 169 240 147 137 148 240 147 138 170 240 147 143 155 240 147 139 180 240 147 144 160 240 147 132 191 240 147 130 139 240 147 143 143 240 147 140 151 240 147 128 129 240 147 140 183 240 147 130 157 240 147 143 143 240 147 143 173 240 147 143 155 240 147 135 190 240 147 143 143 240 147 133 147 240 147 133 177 240 147 128 128 240 147 129 144 240 147 143 170 240 147 131 128 240 147 140 162 240 147 140 162 240 147 136 150 240 147 136 150 240 147 143 155].

	fontFamily := FontFamily readIfNeeded: DirectoryEntry trueTypeFontsDirectory / 'NotoEgyptianHieroglyphs'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attributes: {TextFontFamily familyName: fontFamily familyName. TextFontSize pointSize: 60})
		editLabel: 'Ancient Egyptian Hieroglyphs!'.
]

{ #category : 'unicode demo' }
UnicodeString class >> demoArabic [
	"
	UnicodeString demoArabic
	"
	| s |
	s := 'وقد إذ حصدت تجهيز, قد بعد والإتحاد بالولايات. من مما أوروبا بولندا، والفلبين. هو المسرح بتطويق التحالف الا. بسبب أساسي المتاخمة عل يبق, حاول أمام الحكومة ان فصل. للجزر وتنامت شيء ثم. ضرب و ألمانيا التبرعات, كان كل خطّة تزامناً.'.

	(Text string: s attribute: (TextFontSize pointSize: 22)) editLabel: 'Arabic'
]

{ #category : 'unicode demo' }
UnicodeString class >> demoArmenian [
	"
	UnicodeString demoArmenian
	"
	| s |
	s :=  'լոռեմ իպսում դոլոռ սիթ ամեթ, սթեթ ինթելլեգաթ ծու մեի, պեռթինախ ոպոռթեռե աթ սեա. եոս ան մութաթ բլանդիթ. նեծ աթ նիսլ վոլումուս ապպելլանթուռ, ան հիս բռութե քուոդսի. սիթ ծեթեռոս ծոնթենթիոնես աթ, թե սումմո դիսծեռե պռո.'.

	(Text string: s attribute: (TextFontSize pointSize: 22)) 
		editLabel: 'Armenian'.
]

{ #category : 'unicode demo' }
UnicodeString class >> demoChinese [
	"
	UnicodeString demoChinese
	"
	| utf8 fontFamily |
	utf8 :=  #[230 156 137 228 187 163 229 143 139 230 175 146 228 186 139 230 177 151 229 155 179 232 166 143 230 156 128 232 179 135 228 184 141 228 191 130 233 153 186 233 155 134 232 168 152 231 180 176 230 132 155 233 159 191 228 187 138 227 128 130 229 155 163 230 131 133 232 168 152 233 152 170 229 136 182 229 185 180 229 135 186 230 144 186 229 164 167 229 134 133 228 186 149 230 138 149 230 152 168 227 128 130 229 155 179 232 173 183 230 131 133 229 185 179 229 182 139 230 155 180 229 146 140 231 155 174 229 139 157 228 188 129 231 167 129 229 188 181 230 173 162 232 131 189 227 128 130 229 140 150 230 141 156 232 184 143 228 191 157 232 145 151 230 156 159 229 163 176 230 151 165 232 138 184 229 173 166 229 156 176 228 187 178 233 161 158 230 151 165 231 168 189 230 150 135 228 186 186 227 128 130 230 180 187 230 131 145 230 159 187 230 162 168 229 155 179 232 166 167 229 149 143 228 184 142 230 137 147 231 168 174 229 160 130 233 171 152 233 150 139 231 167 128 230 173 162 231 180 153 232 166 154 227 128 130 229 155 178 232 169 177 228 189 143 230 179 163 230 154 174 230 132 155 229 144 190 229 139 157 233 164 168 229 143 165 230 152 168 232 152 135 229 174 159 227 128 130 233 150 139 230 142 178 230 181 170 229 160 177 233 128 177 229 187 131 230 150 135 229 173 166 233 128 154 230 157 165 229 133 136 230 160 170 230 156 128 229 189 147 230 173 162 228 188 154 232 178 171 230 136 166 231 190 169 227 128 130 232 168 128 230 177 154 230 156 172 230 166 130 231 166 129 230 156 171 229 160 128 230 148 191 231 154 132 229 164 167 231 148 179 229 164 154 230 159 187 232 151 164 227 128 130 233 133 184 230 132 143 229 177 177 230 159 187 230 150 173 232 191 171 229 155 179 230 163 132 231 139 172 232 165 191 233 155 187 230 150 135 229 174 182 228 184 128 229 155 158 229 183 165 229 144 140 227 128 130].

	fontFamily := FontFamily readIfNeeded: DirectoryEntry trueTypeFontsDirectory / 'KiwiMaru'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attributes: {TextFontFamily familyName: fontFamily familyName. TextFontSize pointSize: 24})
		editLabel: 'Chinese'.
]

{ #category : 'unicode demo' }
UnicodeString class >> demoCyrillic [
	"
	UnicodeString demoCyrillic
	"
	| s |
	s := 'Съешь ещё этих мягких французских булок, да выпей же чаю.
В чащах юга жил бы цитрус? да но фальшивый экземпляръ!
Љубазни фењерџија чађавог лица хоће да ми покаже штос.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Cyrillic'.
]

{ #category : 'unicode demo' }
UnicodeString class >> demoGeorgian [
	"
	UnicodeString demoGeorgian
	"
	| s |
	s := 'ლორემ იფსუმ დოლორ სით ამეთ, ვის ეა ფრიმის ასსენთიორ სცრიფთორემ, ეუმ ვერითუს ინიმიცუს ან. ნონუმy ვირთუთე ეხ ჰას, ად ბონორუმ სუსციფით ცონსთითუამ ვის. ათ ვის უთამურ იისყუე დისფუთათიონი, დუის ვენიამ ვიმ ეი, იდ ნეც ფეუგაით სენსერით ელეცთრამ. სით ნო ბრუთე რეცუსაბო თემფორიბუს, უთ ნეც დიცთა ოფფიციის ელოყუენთიამ.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Georgian'.
]

{ #category : 'unicode demo' }
UnicodeString class >> demoGreek [
	"
	UnicodeString demoGreek
	"
	| s |
	s := 'Λορεμ ιπσθμ δολορ σιτ αμετ, νε πρι ρεγιονε cοπιοσαε σινγθλισ, αν εοσ αλια απειριαν ιραcθνδια. Φεθγαιτ διγνισσιμ μελ αν, ιν δθισ ιλλθδ λιβερ qθι. Ινερμισ μενανδρι λιβεραvισσε ναμ ετ. Ιδ νιβη σολετ αδιπισcινγ εστ. Cθ αδηθc δομινγ πρι. Εστ ιγνοτα τριτανι εα, μελ cηορο λαβιτθρ ιδ.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Greek'
]

{ #category : 'unicode demo' }
UnicodeString class >> demoHebrew [
	"
	UnicodeString demoHebrew
	"
	| s |
	s := 'עזה מדעי כלכלה ממונרכיה או, לערך מדינות האנציקלופדיה או לוח. היא לציין צרפתית לרפובליקה בה. מלא אחרים העברית בה, בשפה שימושיים של כתב. בקר שאלות זכויות מה. מדעי גיאוגרפיה אם צעד. באגים לרפובליקה שכל ב. שתי ב והוא העזרה תקשורת, דת ארץ ישראל אחרונים המקושרים.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Hebrew'
]

{ #category : 'unicode demo' }
UnicodeString class >> demoLatin [
	"
	UnicodeString demoLatin
	"
	| s |
	s := 'Lorem ipsum dolor sit amet, populo latine aliquid pro no, sed te graeco percipit adipiscing. Definiebas dissentias mea in, vel nulla tollit ex. Aliquip persequeris eloquentiam his et. Cu diam novum animal vix, nam et probatus tractatos deterruisset. Percipit efficiantur pri ex, id animal debitis vulputate eos, ad ius posse error populo.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Latin Alphabet'
]

{ #category : 'instance creation' }
UnicodeString class >> empty [
	"Return the empty UnicodeString instance.
	Since my instances are readonly, this could be cached."

	^ self fromUtf8Bytes: `#[]` codePointCount: 0.
]

{ #category : 'instance creation' }
UnicodeString class >> fromBytesStream: aByteStream [
	"Handle UTF-8 bytes, but convert invalid UTF-8 assuming it means ISO-8859-1.
	Argument can be, for example, a InflateStream."

	| utf8 codePointCount buffer readBytes leftover byteIndex |
	buffer := ByteArray new: 10000.
	leftover := nil.
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[aByteStream atEnd] whileFalse: [ 
			readBytes := aByteStream nextInto: buffer.
			leftover ifNotNil: [
				readBytes := leftover, readBytes.
				leftover := nil ].
			byteIndex := 1.
			[ byteIndex <= (aByteStream atEnd
						ifFalse: [readBytes size-3] "To be sure we don't break a possible partially read multibyte code point."
						ifTrue: [readBytes size])   						"But do taks _all_ the bytes if they are the last ones."
			] whileTrue: [
				| n |
				n := Character utf8BytesAndCodePointAt: byteIndex in: readBytes
					into: [ :byte1 :byte2 :byte3 :byte4 |
						utf8Stream nextPut: byte1.
						byte2 ifNotNil: [
							utf8Stream nextPut: byte2.
							byte3 ifNotNil: [
								utf8Stream nextPut: byte3.
								byte4 ifNotNil: [
									utf8Stream nextPut: byte4 ]]].
						codePointCount := codePointCount + 1]
					into: nil.
				byteIndex := byteIndex + n ].
			leftover := byteIndex > readBytes size ifFalse: [
				readBytes copyFrom: byteIndex to: readBytes size ].
		].
	].
	^self fromUtf8Bytes: utf8 codePointCount: codePointCount.
]

{ #category : 'instance creation' }
UnicodeString class >> fromCodePoints: aWordArray [

	^self fromUtf8Bytes: (self utf8FromCodePoints: aWordArray) codePointCount: aWordArray size.
]

{ #category : 'instance creation' }
UnicodeString class >> fromUtf8Bytes: utf8ByteArray [
	"Prefer #fromUtf8Bytes:codePointCount: if the number of CodePoints is cheap to know.
	Performance optimization.
	Note: The received ByteArray argument should not be modified elsewhere. Pass a copy if needed."

	^self basicNew privateBytes: utf8ByteArray codePointCount: nil.
]

{ #category : 'instance creation' }
UnicodeString class >> fromUtf8Bytes: utf8ByteArray codePointCount: aNumber [
	"Call this method if the number of CodePoints encoded in utf8ByteArray is known.
	This is an important optimization, especially if ASCII (i.e. utfByteArray size = aNumber).
	Note: The received ByteArray argument should not be modified elsewhere. Pass a copy if needed."

	^self basicNew privateBytes: utf8ByteArray codePointCount: aNumber.
]

{ #category : 'instance creation' }
UnicodeString class >> lfString [
	"Answer a string containing a single Lf character.
	UnicodeString lfString
	"
	^ `UnicodeString with: Character lf`
]

{ #category : 'instance creation' }
UnicodeString class >> new: n [
	"Answer will contain n null Characters"

	^self fromUtf8Bytes: (ByteArray new: n) codePointCount: n
]

{ #category : 'instance creation' }
UnicodeString class >> new: size withAll: codePoint  [
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^ self streamContents: [ :strm |
		size timesRepeat: [ strm nextPut: codePoint ]].
]

{ #category : 'instance creation' }
UnicodeString class >> newFrom: aSimilarObject [
	^self streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: UnicodeString
"
]

{ #category : 'instance creation' }
UnicodeString class >> newLineString [
	"Answer a string containing a single newLine (i.e. Lf) character.
	UnicodeString newLineString
	"
	^ `UnicodeString with: Character newLineCharacter`
]

{ #category : 'stream creation' }
UnicodeString class >> streamContents: blockWithArg [
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, UnicodeStrings.
	Answer is an instance of us."

	| stream |	
	stream := Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream contents
]

{ #category : 'system' }
UnicodeString class >> symbolClassToUse [
	"Answer the class to create symbols from us"
	^UnicodeSymbol
]

{ #category : 'instance creation' }
UnicodeString class >> tab [
	"Answer a string containing a single tab character.
	UnicodeString tab
	"
	^ `UnicodeString with: Character tab`
]

{ #category : 'services' }
UnicodeString class >> utf16BytesFromUtf8: utf8ByteArray bigEndian: bigEndian includeBOM: wantsBOM [
	"Convert the given string from UTF-8 to UTF-16.
	Honor requested endianess. Include BOM if requested."

	^ByteArray streamContents: [ :strm | | s byteIndex n |
		wantsBOM ifTrue: [
			strm nextUint16Put: 16rFEFF bigEndian: bigEndian ].
		s := utf8ByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: utf8ByteArray
				into: nil
				into: [ :codePoint |
					Character
						evaluate: [ :word | strm nextUint16Put: word bigEndian: bigEndian ]
						withUtf16WordsOf: codePoint  ].
			byteIndex := byteIndex + n ]].
]

{ #category : 'services' }
UnicodeString class >> utf16WordsFromUtf8: utf8ByteArray  includeBOM: wantsBOM [
	"Convert the given string from UTF-8 to UTF-16.
	Include BOM if requested.
	https://en.wikipedia.org/wiki/UTF-16
	UnicodeString codePointsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162].
	(UnicodeString utf16WordsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162]
		includeBOM: false) asArray collect: [ :w | w hex ].
	(UnicodeString utf16WordsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162]
		includeBOM: true) asArray collect: [ :w | w hex ].
	"

	^DoubleByteArray streamContents: [ :strm | | s byteIndex n |
		wantsBOM ifTrue: [
			strm nextPut: 16rFEFF ].
		s := utf8ByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: utf8ByteArray
				into: nil
				into: [ :codePoint |
					Character
						evaluate: [ :word | strm nextPut: word ]
						withUtf16WordsOf: codePoint ].
			byteIndex := byteIndex + n ]].
]

{ #category : 'services' }
UnicodeString class >> utf8FromCodePoints: aWordArray [
	"Convert the given string from UTF-32 to UTF-8"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOf: codePoint ]]
]

{ #category : 'services' }
UnicodeString class >> utf8FromUtf16Bytes: utf16ByteArray [
	"Convert the given string from UTF-16 to UTF-8.
	Use BOM if present to detect endianess.
	If no BOM, assume little endian."

	^self utf8FromUtf16Bytes: utf16ByteArray bigEndianHint: nil
]

{ #category : 'services' }
UnicodeString class >> utf8FromUtf16Bytes: utf16ByteArray bigEndianHint: bigEndianHintOrNil [
	"Convert the given string from UTF-16 to UTF-8.
	Use BOM if present to detect endianess, or bigEndianHintOrNil ifnotNil.
	If no BOM and bigEndianHintOrNil is nil, assume little endian."

	| isBigEndian startIndex |
	utf16ByteArray size = 0 ifTrue: [
		^#[] ].
	isBigEndian := bigEndianHintOrNil ifNil: [false]. "best guess"
	startIndex := 1.
	(utf16ByteArray uint16At: 1 bigEndian: true) = 16rFEFF ifTrue: [
		isBigEndian := true.
		startIndex := 3 ].
	(utf16ByteArray uint16At: 1 bigEndian: false) = 16rFEFF  ifTrue: [
		isBigEndian := false.
		startIndex := 3 ].
	^ByteArray streamContents: [ :strm | | s byteIndex word |
		s := utf16ByteArray size.
		byteIndex := startIndex.
		[ byteIndex <= s ] whileTrue: [
			word := utf16ByteArray uint16At: byteIndex bigEndian: isBigEndian.
			(word between: 16rD800 and: 16rDFFF)
				ifFalse: [
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: word.
					byteIndex := byteIndex + 2 ]
				ifTrue: [ | highSurrogatePart lowSurrogatePart codePoint |
					(word between: 16rD800 and: 16rDBFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					highSurrogatePart := word - 16rD800.
					word := utf16ByteArray uint16At: byteIndex+2 bigEndian: isBigEndian.
					(word between: 16rDC00 and: 16rDFFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					lowSurrogatePart := word - 16rDC00.
					codePoint := highSurrogatePart << 10 + lowSurrogatePart + 16r10000.
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: codePoint.
					byteIndex := byteIndex + 4 ]]].
]

{ #category : 'services' }
UnicodeString class >> utf8FromUtf16Words: utf16WordArray [
	"Convert the given string from UTF-16 to UTF-8.
	Skip BOM if present."

	| startIndex |
	utf16WordArray size = 0 ifTrue: [
		^#[] ].
	startIndex := 1.
	(utf16WordArray at: 1) = 16rFEFF ifTrue: [
		startIndex := 2 ].
	^ByteArray streamContents: [ :strm | | s wordIndex word |
		s := utf16WordArray size.
		wordIndex := startIndex.
		[ wordIndex <= s ] whileTrue: [
			word := utf16WordArray at: wordIndex.
			(word between: 16rD800 and: 16rDFFF)
				ifFalse: [
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: word.
					wordIndex := wordIndex + 1 ]
				ifTrue: [ | highSurrogatePart lowSurrogatePart codePoint |
					(word between: 16rD800 and: 16rDBFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					highSurrogatePart := word - 16rD800.
					word := utf16WordArray at: wordIndex+1.
					(word between: 16rDC00 and: 16rDFFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					lowSurrogatePart := word - 16rDC00.
					codePoint := highSurrogatePart << 10 + lowSurrogatePart + 16r10000.
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: codePoint.
					wordIndex := wordIndex + 2 ]]].
]

{ #category : 'instance creation' }
UnicodeString class >> with: aCharacter  [
	"Answer a new instance of me, containing utf bytes for aCharacter.
	UnicodeString with: $a
	UnicodeString with: $á
	UnicodeString with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOf: aCharacter codePoint)
		codePointCount: 1.
]

{ #category : 'instance creation' }
UnicodeString class >> with: aCharacter with: otherCharacter [
	"Answer a new instance of me, containing utf bytes for aCharacter and otherCharacter.
	UnicodeString with: $a
	UnicodeString with: $á with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOf: aCharacter codePoint),
			(Character utf8BytesOf: otherCharacter codePoint)
		codePointCount: 2.
]

{ #category : 'instance creation' }
UnicodeString class >> with: aCharacter with: otherCharacter with: thirdCharacter [
	"Answer a new instance of me, containing utf bytes for aCharacter, otherCharacter and thirdCharacter.
	UnicodeString with: $a
	UnicodeString with: $á with: Character euro
	UnicodeString with: $á with: Character euro with: $¿
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOf: aCharacter codePoint),
			(Character utf8BytesOf: otherCharacter codePoint),
			(Character utf8BytesOf: thirdCharacter codePoint)
		codePointCount: 3.
]

{ #category : 'instance creation' }
UnicodeString class >> withAll: aSequenceableCollection [
	"Create a new instance containing all the elements from aSequenceableCollection."
	^ self newFrom: aSequenceableCollection
]

{ #category : 'stream creation' }
UnicodeString class >> writeStream [
	"Answer a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, UnicodeStrings.
	Generally prefer this. It offers best overall performance."

	| stream |	
	stream := Utf8EncodedWriteStream on: (ByteArray new: 100).
	^stream
]

{ #category : 'copying' }
UnicodeString >> , aStringOrText [
	^ aStringOrText appendToString: self
]

{ #category : 'comparing' }
UnicodeString >> < aString [
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Comparing for equality may is faster in most casts. Try it first."
	self = aString ifTrue: [ ^false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 1].
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 1.
]

{ #category : 'comparing' }
UnicodeString >> = aString [
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, UnicodeString, UnicodeSymbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self hash = aString hash ifFalse: [
		^false ].

	(self isInCanonicalCompositionForm and: [ aString isInCanonicalCompositionForm ]) ifFalse: [
		^self canonicalComposition = aString canonicalComposition ].

	"Now we can assume we are both in NFC"
	self size = aString size ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [	
		"Fast lane."
		^String is: bytes equalTo: aString ].	

	aString isUnicodeString ifFalse: [
		^ self beginsWith: aString ].

	self isAscii = aString isAscii ifFalse: [
		^false ].

	^String is: bytes equalTo: aString bytes.
]

{ #category : 'copying' }
UnicodeString >> appendToString: aString [

	^ self species fromUtf8Bytes: aString asUtf8Bytes, bytes codePointCount: aString size + self size.
]

{ #category : 'converting' }
UnicodeString >> asByteArray [
	"Answer a ByteArray with a valid UTF-8 sequence."

	^bytes
]

{ #category : 'converting' }
UnicodeString >> asByteString [
	"Answer an instance of the Byte oriented String class"

	"Fast lane."
	self isAscii ifTrue: [
		^bytes asString ].

	"Need to do UTF-8 decoding."
	^super asByteString
]

{ #category : 'converting' }
UnicodeString >> asByteStringIfAscii [
	"Answer an instance of the Byte oriented String class"

	self isAscii ifTrue: [
		^self asByteString ].
	^self
]

{ #category : 'converting' }
UnicodeString >> asCodePoints [
	"Answer a WordArray."

	^UnicodeString codePointsFromUtf8: bytes
]

{ #category : 'converting' }
UnicodeString >> asHex [

	^bytes asHex
]

{ #category : 'converting' }
UnicodeString >> asLowercase [
	| b |
	self isAscii ifFalse: [ ^super asLowercase ].
	b := bytes copy.
	String translateToLowercase: b.
	^ self species fromUtf8Bytes: b
]

{ #category : 'Unicode normalization' }
UnicodeString >> asNFC [
	"Convenience"
	^self canonicalComposition
]

{ #category : 'Unicode normalization' }
UnicodeString >> asNFD [
	"Convenience"
	^self canonicalDecomposition
]

{ #category : 'converting' }
UnicodeString >> asUnicodeString [
	"Convert me to an UnicodeString"
	
	^ self
]

{ #category : 'converting' }
UnicodeString >> asUppercase [
	| b |
	self isAscii ifFalse: [ ^super asUppercase ].
	b := bytes copy.
	String translateToUppercase: b.
	^ self species fromUtf8Bytes: b
]

{ #category : 'converting' }
UnicodeString >> asUtf8Bytes [
	"Answer a ByteArray with a valid UTF-8 sequence."

	^bytes
]

{ #category : 'converting' }
UnicodeString >> asUtf8BytesOrByteStringOrByteArray [
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self asUtf8Bytes
]

{ #category : 'accessing' }
UnicodeString >> at: codePointIndex [
	"Answer the Character with the CodePoint at code point index codePointIndex.
	(not a byte index!)"

	| byteIndex |
	self boundsCheck: codePointIndex.
	self isAscii ifTrue: [ ^ Character codePoint: (bytes at: codePointIndex) ].
	byteIndex := self byteIndexAt: codePointIndex.
	Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
		^Character codePoint: codePoint ].
	^nil "Invalid UTF-8"
]

{ #category : 'accessing' }
UnicodeString >> at: codePointIndex put: aCharacter [
	"Store argument at position codePointIndex "

	| byteIndex currentByteCount argumentBytes newBytes |

	self boundsCheck: codePointIndex.
	byteIndex := self byteIndexAt: codePointIndex.
	currentByteCount := self encodedByteCountAt: byteIndex.

	"Fastest path if both old and new are ASCII"
	currentByteCount = 1 ifTrue: [
		aCharacter isAscii ifTrue: [
			bytes at: byteIndex put: aCharacter asciiValue.
			^ aCharacter ]].

	argumentBytes := Character utf8BytesOf: aCharacter codePoint.

	"Avoid creating new bytes object if both old and new require same number of bytes"
	argumentBytes size = currentByteCount ifTrue: [
		bytes replaceFrom: byteIndex to: byteIndex + currentByteCount - 1 with: argumentBytes startingAt: 1.
		strideIndex := nil.
		hash := nil.
		^ aCharacter ].

	"More expensive: needs a new bytes object of different size"
	newBytes := ByteArray new: bytes size - currentByteCount + argumentBytes size.
	newBytes
		replaceFrom: 1 to: byteIndex-1 with: bytes startingAt: 1;
		replaceFrom: byteIndex to: byteIndex + argumentBytes size - 1 with: argumentBytes startingAt: 1;
		replaceFrom: byteIndex + argumentBytes size to: newBytes size with: bytes startingAt: byteIndex + currentByteCount.

	self privateBytes: newBytes codePointCount: count.
	^ aCharacter.
]

{ #category : 'Unicode normalization' }
UnicodeString >> basicCanonicalComposition [
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalComposition.
	(UnicodeString fromCodePoints: #(119128 119141)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119135 119151)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119128 119141 119151)) basicCanonicalComposition asCodePoints
	"
	| byteIndex stopByteIndex combining |
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			| n |
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				(Character combiningClass: codePoint) = 0 ifTrue: [
					self basicCompose: combining on: strm.
					combining := OrderedCollection new ].
				combining add: codePoint ].
			byteIndex := byteIndex + n ].
		self basicCompose: combining on: strm ].
]

{ #category : 'Unicode normalization' }
UnicodeString >> basicCanonicalDecomposition [
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalDecomposition.
	(Character codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	(Character codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex |
	byteIndex := 1.
	stopByteIndex := bytes size.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			| n |
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				Character canonicalDecomposition: codePoint on: strm ].
			byteIndex := byteIndex + n ]].
]

{ #category : 'Unicode normalization' }
UnicodeString >> basicCanonicallyOrdered [
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(UnicodeString fromCodePoints: #(16r61  16r0300 16r0315 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0300 16r031B 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r031B 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r0300 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0300 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0315 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	"
	| byteIndex stopByteIndex combining |
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			| n |
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				| cls |
				cls := Character combiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
							combining do: [ :e | strm nextPutCodePoint: e third ]].
						strm nextPutCodePoint: codePoint.
						combining := OrderedCollection new ]
					ifFalse: [
						| element |
						element := { cls. combining size+1. codePoint }.
						combining add: element ]].
			byteIndex := byteIndex + n ].
		combining notEmpty ifTrue: [
			combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
			combining do: [ :e | strm nextPutCodePoint: e third ] ]].
]

{ #category : 'Unicode normalization' }
UnicodeString >> basicCompose: combiningCodePoints on: aStream [
	| continue composition i base |
	combiningCodePoints notEmpty ifTrue: [
		continue := combiningCodePoints size > 1.
		[continue] whileTrue: [
			continue := false.
			base := combiningCodePoints first.
			i := 2.
			composition := nil.
			[composition isNil and: [i <= combiningCodePoints size ]] whileTrue: [
				composition := Character canonicalCompositionOf: base and: (combiningCodePoints at: i).
				composition ifNotNil: [
					combiningCodePoints at: 1 put: composition.
					combiningCodePoints removeAt: i.
					continue := combiningCodePoints size > 1 ].
				i := i + 1 ]].
		combiningCodePoints do: [ :e | aStream nextPutCodePoint: e ]].
]

{ #category : 'comparing' }
UnicodeString >> basicHash [
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859-1 non ASCII characters are present.
	Note: will answer different values for UnicodeString's  that are equal if #canonicalDecomposition is taken."
	bytes size >  ByteArray minSizeForHashQuick ifTrue: [
		^ bytes hashQuick: String hash ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash
]

{ #category : 'private' }
UnicodeString >> boundsCheck: codePointIndex [
	(self isInBounds: codePointIndex) 
		ifFalse: [ self errorSubscriptBounds: codePointIndex ]
]

{ #category : 'private' }
UnicodeString >> byteIndexAt: codePointIndex [
	| byteIndex |
	codePointIndex < 2 ifTrue: [ ^codePointIndex ]. "For 0 and 1"
	self isAscii ifTrue: [ ^ codePointIndex ].
	codePointIndex = (self size+1) ifTrue: [
		"The position a hypotetical extra code point would start at.
		Useful to find the last byte position for some codePointIndex. See senders."
		^bytes size+1 ].
	byteIndex := codePointIndex > self stride
		ifTrue: [
			strideIndex ifNil: [ self computeCountAndIndex ].
			strideIndex at: codePointIndex - 1 // self stride ]
		ifFalse: [ 1 ].
	codePointIndex - 1 \\ self stride timesRepeat: [ 
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ].
	^ byteIndex
]

{ #category : 'accessing' }
UnicodeString >> bytes [
	^bytes
]

{ #category : 'Unicode normalization' }
UnicodeString >> canonicalComposition [
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	The canonical composition is cached. Therefor it doesn't need to be computed every time it is needed.
	This makes it actually cheaper than #canonicalDecomposition!

	UnicodeString fromUtf8Bytes: #[195 162].
	(UnicodeString fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalComposition asUtf8Bytes

	UnicodeString fromUtf8Bytes: #[97 204 130].
	(UnicodeString fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalComposition asUtf8Bytes
	"

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	^hash isNumber
		ifTrue: [
			"If 'hash' is a number it means we are already in canonicalComposition form (NFC)."
			self ]
		ifFalse: [
			"If 'hash' is not a number, it actually contains our #canonicalComposition."
			hash ].
]

{ #category : 'Unicode normalization' }
UnicodeString >> canonicalDecomposition [
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(Character codePoint: 16r1EBF) asString
	(Character codePoint: 16r1EBF) asString asUtf8Bytes
	(Character codePoint: 16r1EBF) asString basicCanonicalDecomposition asUtf8Bytes
	(Character codePoint: 16r1EBF) asString asCodePoints asArray collect: [ :e | e hex ]
	(Character codePoint: 16r1EBF) asString basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]).
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(Character codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	(Character codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	"
	^self basicCanonicalDecomposition basicCanonicallyOrdered
]

{ #category : 'comparing' }
UnicodeString >> caseInsensitiveLessOrEqual: aString [
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^(String compareIgnoreCase: bytes with: aString) <= 2 ].
		(aString isUnicodeString and: [ aString isAscii ]) ifTrue: [
			^(String compareIgnoreCase: bytes with: aString bytes) <= 2 ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase <= aString asLowercase
]

{ #category : 'private' }
UnicodeString >> codePointIndexAt: byteIndex [
	| codePointIndex byteCursor |
	byteIndex < 2 ifTrue: [ ^byteIndex ]. "For 0 and 1"
	self isAscii ifTrue: [ ^ byteIndex ].
	byteIndex = (bytes size+1) ifTrue: [
		"The index of the code point starting at a hypotetical extra byte.
		Useful to find the code point index for the last byte in a sequence. See senders."
		^self size+1 ].
	codePointIndex := byteCursor := 1.
	strideIndex ifNil: [ self computeCountAndIndex ].
	strideIndex ifNotEmpty: [  
		strideIndex 
			findBinaryIndex: [ :x | byteIndex - x ] 
			do: [ :found | ^ (found * self stride) + 1 ] 
			ifNone: [ :lower :upper |
				lower = 0 
			 		ifFalse: [ 
						byteCursor := strideIndex at: lower. 
						codePointIndex := (lower * self stride) + 1 ] ] ].
	[ byteCursor <= bytes size ] whileTrue: [
		byteCursor = byteIndex 
			ifTrue: [ ^ codePointIndex ].
		byteCursor := byteCursor + (self encodedByteCountAt: byteCursor).
		codePointIndex := codePointIndex + 1 ].
	^ 0
]

{ #category : 'private' }
UnicodeString >> computeCountAndIndex [
	self isEmpty ifTrue: [
		count := 0.
		strideIndex := #[].
		^ self ].
	"the number of characters cannot be larger than the byte size"
	bytes size <= self stride
		ifTrue: [ self computeCountNoIndex ]
		ifFalse: [ self computeCountAndIndexFull ]
]

{ #category : 'private' }
UnicodeString >> computeCountAndIndexFull [
	| strideIndexClass |
	count := 0.
	strideIndexClass := bytes size < 256
		ifTrue: [ ByteArray ]
		ifFalse: [ bytes size < 65536
			ifTrue: [ DoubleByteArray ]
			ifFalse: [ bytes size < 4294967296
				ifTrue: [ WordArray ]
				ifFalse: [DoubleWordArray ]]].
	strideIndex := strideIndexClass streamContents: [ :out |
		| byteIndex |
		byteIndex := 1.
		[ byteIndex <= bytes size ] whileTrue: [
			count := count + 1.
			(count > self stride and: [ (count - 1 \\ self stride) = 0 ]) 
				ifTrue: [ out nextPut: byteIndex ].
			byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ] ]
]

{ #category : 'private' }
UnicodeString >> computeCountNoIndex [
	| byteIndex |
	count := 0.
	strideIndex := #[].
	byteIndex := 1.
	[ byteIndex <= bytes size ] whileTrue: [
		count := count + 1.
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ]
]

{ #category : 'comparing' }
UnicodeString >> computeHash [
	"Same hash value as the equivalent ASCII String, provided that all our CodePoints exist as Characters"

	| byteString |
	self isAscii ifTrue: [
		hash := self basicHash.
		^self ].

	"If there's a String that is equal to us (i.e. no characters were lost during conversion)"
	byteString := self asByteString.
	byteString size = self size ifTrue: [
		hash := byteString hash.
		^self ].

	"Otherwise, use a hash that doesn't need to equal that of a String"
	hash := self basicHash.
]

{ #category : 'copying' }
UnicodeString >> copyFrom: startIndex to: stopIndex [
	"Return a new string, a copy of me from startIndex to stopIndex, both character indices.
	Signal SubscriptOutOfBounds or CollectionIsEmpty in case of illegal indices."
	
	| startByteIndex stopByteIndex |
	startIndex > self size ifTrue: [ ^ self species empty ].
	stopIndex <= 0 ifTrue: [ ^ self species empty ].
	self boundsCheck: startIndex. 
	self boundsCheck: stopIndex. 
	startByteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	stopByteIndex := stopByteIndex + (self encodedByteCountAt: stopByteIndex) - 1.
	^ self species
		fromUtf8Bytes: (bytes copyFrom: startByteIndex to: stopByteIndex)
		codePointCount: stopIndex-startIndex+1.
]

{ #category : 'copying' }
UnicodeString >> copyReplaceFrom: startIndex to: endIndex with: replacement [
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| epilogFirstIndex prologLastIndex newBytes replacementBytes |
	prologLastIndex := (self byteIndexAt: startIndex)-1.
	epilogFirstIndex := self byteIndexAt: endIndex+1.
	replacementBytes := replacement string asUtf8BytesOrByteString.
	newBytes := (bytes copyFrom: 1 to: prologLastIndex),
		replacementBytes,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size).
	^ self species fromUtf8Bytes: newBytes codePointCount: (startIndex-1) + replacement size + (self size-endIndex).
]

{ #category : 'copying' }
UnicodeString >> copyWith: aCharacter [

	^ self species
		fromUtf8Bytes: bytes, (Character utf8BytesOf: aCharacter codePoint)
		codePointCount: self size + 1.
]

{ #category : 'enumerating' }
UnicodeString >> do: aBlock [
	"Evaluate aBlock for all elements."

	| byteIndex stopByteIndex n |
	byteIndex := 1.
	stopByteIndex := bytes size.
	[ byteIndex <= stopByteIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			aBlock value: (Character codePoint: codePoint) ].
		byteIndex := byteIndex + n ].
]

{ #category : 'dispatch' }
UnicodeString >> drawOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color [

	| byteStartIndex byteStopIndex |
	byteStartIndex := self byteIndexAt: startIndex.
	byteStopIndex := (self byteIndexAt: stopIndex+1)-1.
	^engine
		drawUtf8: bytes
		fromByte: byteStartIndex
		toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.
]

{ #category : 'dispatch' }
UnicodeString >> drawOnVectorEngine: engine from: startIndex to: stopIndex at: aPoint font: aTrueTypeFont color: color [

	| byteStartIndex byteStopIndex p answer d |
	byteStartIndex := self byteIndexAt: startIndex.
	byteStopIndex := (self byteIndexAt: stopIndex+1)-1.
	"Please keep consistent with #basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:"
	p := aTrueTypeFont isSuperscript
		ifTrue: [ aPoint x  @ (aPoint y - (aTrueTypeFont ascent*0.55)) ]
		ifFalse: [
			aTrueTypeFont isSubscript
				ifTrue: [aPoint x @ (aPoint y + (aTrueTypeFont ascent*0.3))]
				ifFalse: [ aPoint ]].
	answer := engine
		drawUtf8String: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true.
	"Please keep consistent with #makeUnderlinedGlyph:"
	aTrueTypeFont isUnderlined ifTrue: [
		d := aTrueTypeFont descent *0.7.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p + (0@d); lineTo: answer + (0@d) ]].
	"Please keep consistent with #makeStruckThroughGlyph: and #drawOnVectorEngine:from:to:at:font:color:borderWidth:borderColor:"
	aTrueTypeFont isStruckThrough ifTrue: [
		d := aTrueTypeFont ascent * 0.3.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer
]

{ #category : 'dispatch' }
UnicodeString >> drawOnVectorEngine: engine from: startIndex to: stopIndex at: aPoint font: aTrueTypeFont color: color borderWidth: sw borderColor: sc [

	| byteStartIndex byteStopIndex p answer d |
	byteStartIndex := self byteIndexAt: startIndex.
	byteStopIndex := (self byteIndexAt: stopIndex+1)-1.
	"Please keep consistent with #basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:"
	p := aTrueTypeFont isSuperscript
		ifTrue: [ aPoint x  @ (aPoint y - (aTrueTypeFont ascent*0.55)) ]
		ifFalse: [
			aTrueTypeFont isSubscript
				ifTrue: [aPoint x @ (aPoint y + (aTrueTypeFont ascent*0.3))]
				ifFalse: [ aPoint ]].
	answer := engine
		drawUtf8String: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		strokeWidth: sw strokeColor: sc
		scale: aTrueTypeFont pointSize
		preventMirroring: true.
	"Please keep consistent with #makeUnderlinedGlyph: and #drawOnVectorEngine:from:to:at:font:color:"
	aTrueTypeFont isUnderlined ifTrue: [
		d := aTrueTypeFont descent *0.7.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p + (0@d); lineTo: answer + (0@d) ]].
	"Please keep consistent with #makeStruckThroughGlyph:"
	aTrueTypeFont isStruckThrough ifTrue: [
		d := aTrueTypeFont ascent * 0.3.
		engine strokeWidth: (aTrueTypeFont descent / 4) color: color do: [
			engine moveTo: p - (0@d); lineTo: answer - (0@d) ]].
	^answer
]

{ #category : 'private' }
UnicodeString >> encodedByteCountAt: byteIndex [
	| byte |
	(byte := bytes at: byteIndex) < 128 ifTrue: [ ^ 1 ].
	(byte bitAnd: 2r11100000) = 2r11000000 ifTrue: [ ^ 2 ].
	(byte bitAnd: 2r11110000) = 2r11100000 ifTrue: [ ^ 3 ].
	(byte bitAnd: 2r11111000) = 2r11110000 ifTrue: [ ^ 4 ].
	self error: 'IllegalLeadingByte '
]

{ #category : 'paragraph support' }
UnicodeString >> encompassLine: aCodePointInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte left nullPosition right |
	argumentLeftByte := self byteIndexAt: aCodePointInterval first.
	argumentRightByte := self byteIndexAt: aCodePointInterval last.

	"Find byte position and code point position following last newLine before us."
	leftByte := (bytes lastIndexOf: `Character newLineCharacter codePoint` startingAt: argumentLeftByte - 1 ifAbsent: [0]) + 1.
	left := self codePointIndexAt: leftByte.

	"Find byte position and code point position for next newLine. Don't include it."
	right := count.
	rightCrByte := bytes indexOf: `Character newLineCharacter codePoint` startingAt: (argumentRightByte max: argumentLeftByte).
	rightCrByte = 0 ifFalse: [
		right := (self codePointIndexAt: rightCrByte)-1].

	"But stop before if any nulls. Don't include them!"
	nullPosition := bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte).
	nullPosition = 0 ifFalse: [
		right := right min: ((self codePointIndexAt: nullPosition)-1) ].

	^left to: right
]

{ #category : 'paragraph support' }
UnicodeString >> encompassParagraph: aCodePointInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte left nullPosition right |
	argumentLeftByte := self byteIndexAt: aCodePointInterval first.
	argumentRightByte := self byteIndexAt: aCodePointInterval last.

	"Find byte position and code point position following last newLine before us."
	leftByte := (bytes lastIndexOf: `Character newLineCharacter codePoint` startingAt: argumentLeftByte - 1 ifAbsent: [0]) + 1.
	left := self codePointIndexAt: leftByte.

	"Find byte position and code point position for next newLine. Include it."
	right := count.
	rightCrByte := bytes indexOf: `Character newLineCharacter codePoint` startingAt: (argumentRightByte max: argumentLeftByte).
	rightCrByte = 0 ifFalse: [
		right := self codePointIndexAt: rightCrByte].

	"But stop before if any nulls. Don't include them!"
	nullPosition := bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte).
	nullPosition = 0 ifFalse: [
		right := right min: ((self codePointIndexAt: nullPosition)-1) ].

	^left to: right
]

{ #category : 'Unicode normalization' }
UnicodeString >> findCanonicalCompositionAndHash [
	"Find our canonical composition (NFC). If we are found to be in NFC, store our hash.
	Otherwise, store our NFC. Queries of #hash will be forwarded to it."

	| canonicalComposition |
	"Ascii is already normalized to NFC."
	self isAscii ifTrue: [
		^self computeHash ].

	"Find our canonical composition"
	canonicalComposition := self basicCanonicalDecomposition basicCanonicallyOrdered basicCanonicalComposition.
	(String is: bytes equalTo: canonicalComposition bytes)
		ifTrue: [
			"If we were already in NFC, compute and store our hash"
			self computeHash ]
		ifFalse: [
			"Otherwise, point to our NFC, and ensure it has its hash, for us to query when needed."
			hash := canonicalComposition.
			canonicalComposition computeHash ].
]

{ #category : 'accessing' }
UnicodeString >> findString: subString startingAt: start [
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findString: subString in: bytes startingAt: start ].
		subString isUnicodeString ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findString: subString bytes in: bytes startingAt: start ]]].

	arg := subString asUtf8BytesOrByteString.
	"Do UTF-8 encoding as needed."
	startByteIndex := self byteIndexAt: start.
	byteIndex := String findString: arg in: bytes startingAt: startByteIndex.
	^self codePointIndexAt: byteIndex
]

{ #category : 'accessing' }
UnicodeString >> findStringCaseInsensitive: subString startingAt: start [
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findStringIgnoreCase: subString in: bytes startingAt: start ].
		subString isUnicodeString ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findStringIgnoreCase: subString bytes in: bytes startingAt: start ]]].

"caseSensitive = false is very expensive. Do better."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase findString: subString asLowercase startingAt: start
]

{ #category : 'enumerating' }
UnicodeString >> from: startIndex to: stopIndex do: aBlock [
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	| byteIndex cpIndex |
	byteIndex := self byteIndexAt: startIndex.
	cpIndex := startIndex.
	[ cpIndex <= stopIndex ] whileTrue: [
		| n |
		n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			aBlock value: (Character codePoint: codePoint).
			cpIndex := cpIndex + 1 ].
		byteIndex := byteIndex + n ].
]

{ #category : 'enumerating' }
UnicodeString >> from: startIndex to: stopIndex doWithUtf8Bytes: aBlock [
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	| byteIndex n stopByteIndex |
	byteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	[ byteIndex <= stopByteIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: aBlock into: nil.
		byteIndex := byteIndex + n ].
]

{ #category : 'comparing' }
UnicodeString >> hash [
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	"If 'hash' is not a number, it actually contains out #canonicalComposition. Ask for its hash then."
	hash isNumber ifFalse: [
		^hash hash ].

	"If 'hash' is a number it means we are in canonicalComposition form (NFC), and 'hash' is an good hash value."
	^hash
]

{ #category : 'accessing' }
UnicodeString >> indexOf: aCharacter startingAt: start ifAbsent: aBlock [
	| codePointBytes startByteIndex foundByteIndex answer |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	codePointBytes := Character utf8BytesOf: aCharacter codePoint.
	startByteIndex := self byteIndexAt: start.
	foundByteIndex := String findString: codePointBytes in: bytes startingAt: startByteIndex.
	answer := self codePointIndexAt: foundByteIndex.
	^answer = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ answer ]
]

{ #category : 'accessing' }
UnicodeString >> indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock [
	"returns the index of the first character in the given set, starting from start.
	The set can only contain ASCII + Latin1 codePoints, i.e. < 256."

	"Fast lane."
	| index |
	self isAscii ifTrue: [
		^ bytes indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock ].

	"If only ASCII bytes are included in the map..."
	(ByteArray indexOfByte: 1 inBytes: aByteSetByteArray startingAt: 128) = 0 ifTrue: [
		|startBI foundBI|
		startBI := self byteIndexAt: start.
		foundBI := bytes indexOfAnyIn: aByteSetByteArray startingAt: startBI ifAbsent: aBlock.
		^ self codePointIndexAt: foundBI ].

	index := start.
	self do: [ :codePoint |
		codePoint < 256 ifTrue: [
			(aByteSetByteArray at: codePoint + 1) > 0 ifTrue: [ ^index ]].
		index := index + 1].

	^aBlock value
]

{ #category : 'initialization' }
UnicodeString >> initialize [
	"Just an empty string."

	self privateBytes: `#[]` codePointCount: 0.
]

{ #category : 'comparing' }
UnicodeString >> is: subString substringAt: index [
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes byteIndex subStringIsAscii |
	index < 1 ifTrue: [ ^false ].
	sequenceSize := subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].

	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self at: index) = (subString at: 1) ifFalse: [ ^false ].
	sequenceSize = 1 ifTrue: [ ^true ].
	(self at: index+sequenceSize-1) = (subString at: sequenceSize) ifFalse: [ ^false ].
	sequenceSize = 2 ifTrue: [ ^true ].
	subStringIsAscii := subString isAscii.
	(self isAscii and: [subStringIsAscii not]) ifTrue: [ ^false ].

	"Try to find arguments for the primitive in #findString:in:startingAt: cheaply"
	subStringBytes := nil.
	subString isUnicodeString ifTrue: [
		"Simple. Take the bytes"
		subStringBytes := subString bytes ].
	(subString isByteString and: [ subStringIsAscii ]) ifTrue: [
		"Can use instance of String, because it has just ASCII bytes"
		subStringBytes := subString ].
	subStringBytes ifNotNil: [
		byteIndex := self byteIndexAt: index.
		^(String findString: subStringBytes in: bytes startingAt: byteIndex) = byteIndex ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to UnicodeString"
	^super is: subString substringAt: index
]

{ #category : 'testing' }
UnicodeString >> isAscii [
	"Return true when I am pure 7-bit ASCII, which means each character is encoded as one byte."

	^ bytes size = self size
]

{ #category : 'testing' }
UnicodeString >> isEmpty [
	"Return true when I am empty, when I do not contain any characters"
	
	^ bytes isEmpty
]

{ #category : 'testing' }
UnicodeString >> isInCanonicalCompositionForm [
	"Answer if we are in NFC"

	self isAscii ifTrue: [ ^true ].

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	"See findCanonicalCompositionAndHash"
	^hash isNumber
]

{ #category : 'accessing' }
UnicodeString >> isLastLineSeparator [

	| s butButLastByte butLastByte lastByte |
	s := bytes size.
	s = 0 ifTrue: [ ^false ].
	(#(10 13 12) statePointsTo: 	(bytes at: s)) ifTrue: [ ^true ].
	s = 1 ifTrue: [ ^false ].
	lastByte := bytes at: s.
	butLastByte := bytes at: s-1.
	(butLastByte = 16rC2 and: [ lastByte = 16r85 ]) ifTrue: [ ^true ]. "Next Line, NEL"
	s = 2 ifTrue: [ ^false ].
	butButLastByte := bytes at: s-2.
	(butButLastByte = 16rE2 and: [butLastByte = 16r80 and: [ "Line Separator and Paragraph Separator"
		lastByte = 16rA8 or: [lastByte = 16rA9]]]) ifTrue: [ ^true ].
	^false.
	
]

{ #category : 'accessing' }
UnicodeString >> isLastSeparator [

	| s |
	s := bytes size.
	s = 0 ifTrue: [ ^false ].
	(#(32 9) statePointsTo: 	(bytes at: s)) ifTrue: [ ^true ].
	self isLastLineSeparator ifTrue: [ ^true ].
	^false.
]

{ #category : 'printing' }
UnicodeString >> isLiteral [
	"Answer whether the receiver is a valid Smalltalk literal."
	^true
]

{ #category : 'testing' }
UnicodeString >> isUnicodeString [

	^true
]

{ #category : 'accessing' }
UnicodeString >> lineIndicesDo: aBlock [
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	String string: bytes lineIndicesDo: [ :startByte :endByteWithoutDelimiters :endByte |
		| start end endWithoutDelimiters |
		start := self codePointIndexAt: startByte.
		end := (self codePointIndexAt: endByte+1)-1.
		endWithoutDelimiters := endByteWithoutDelimiters = endByte
			ifTrue: [end]
			ifFalse: [(self codePointIndexAt: endByteWithoutDelimiters+1)-1].
		aBlock
			value: start
			value: endWithoutDelimiters
			value: end
		]
]

{ #category : 'services' }
UnicodeString >> numArgs [
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self isEmpty ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar := self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Reject if any chars are non-alphanumeric"
		self do: [ :codePoint | codePoint tokenish ifFalse: [ ^-1 ]].
		"Fast colon count"
		numColons := 0.
		start := 1.
		[ (ix := self findString: ':' startingAt: start) > 0 ] whileTrue: [
			numColons := numColons + 1.
			start := ix + 1 ].
		^ numColons ].
	^ -1.
]

{ #category : 'private' }
UnicodeString >> privateBytes: utf8ByteArray codePointCount: aNumberOrNil [
	"Initialize the receiver. If we already know that utf8ByteArray only holds bytes less than 128 (i.e. ASCII characters), initialize 'count'. This is an important optimization."

	bytes := utf8ByteArray.
	count := aNumberOrNil.
	strideIndex := nil.
	hash := nil.
]

{ #category : 'comparing' }
UnicodeString >> sameAs: aString [
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	self size = aString size ifFalse: [
		^false ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^(String compareIgnoreCase: bytes with: aString) = 2 ].
		aString isUnicodeString ifTrue: [
			^ aString isAscii
				ifFalse: [ false ] "One is ASCII, other isn't."
				ifTrue: [
					"Both are ASCII"
					(String compareIgnoreCase: bytes with: aString bytes) = 2 ]]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase
]

{ #category : 'accessing' }
UnicodeString >> size [
	"Return how many characters I contain"
	
	self isEmpty ifTrue: [ ^ 0 ].
	count ifNil: [ self computeCountAndIndex ].
	^ count
]

{ #category : 'private' }
UnicodeString >> stride [
	"Return the stride, the constant I use internally to build my index"
	
	^ 32
]

{ #category : 'converting' }
UnicodeString >> substrings [
	"Answer an array of the substrings that compose the receiver.
	Whitespace characters are considered separators, and not included in the result.

	'  Hello World! Some  		1234 more  €s ' substrings

	'  Hello World!     Some  		1234 more  €s 
	for you
	to play with ' substrings
	"
	^ self isAscii
		ifTrue: [ (String substringsIn: bytes) collect: [ :eachBytes |
				self species fromUtf8Bytes: eachBytes codePointCount: eachBytes size ]]
		ifFalse: [ (String substringsIn: bytes) collect: [ :eachBytes |
				self species fromUtf8Bytes: eachBytes ]].
]

{ #category : 'converting' }
UnicodeString >> surroundedBySingleQuotes [
	"Answer the receiver with leading and trailing quotes.  "

	^ `(UnicodeString with: $')`, self, `(UnicodeString with: $')`
]

{ #category : 'converting' }
UnicodeString >> thatCanBeModified [
	"In UnicodeString #at:put: could be slow. Prefer ArrayOfCharactersAndCodePoints.
	(This comment used to say: Because UnicodeString instances don't support #at:put:)"

	^ArrayOfCharactersAndCodePoints withAll: self
]

{ #category : 'accessing' }
UnicodeString >> utf8BytesSize [
	"We are already UTF-8 encoded."

	^bytes size
]

{ #category : 'converting' }
UnicodeString >> withLineEndings: lineEndingString [
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	| correctedBytes |
	correctedBytes := String string: bytes withLineEndings: lineEndingString asByteArray.
	"The codePointCount trick can be done because the line endings considered are all single byte UTF-8.
	If we ever consider unicode line endings taking more than one byte, this is no longer valid (pass nil instead)."
	^ self species fromUtf8Bytes: correctedBytes codePointCount: self size - bytes size + correctedBytes size.
]
