'From Cuis 4.2 of 25 July 2013 [latest update: #2608] on 1 December 2015 at 9:39:26.845985 am'!

!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 11/30/2015 16:02'!
buildLargerPunctuation: familyName
	"
	StrikeFont buildLargerPunctuation: 'DejaVu'
	StrikeFont buildLargerPunctuation: 'DejaVu Sans Mono'
	"
	| form form2 f10 f11 f12 f9 |

	f9 _ AbstractFont familyName: familyName pointSize: 9.
	f10 _ AbstractFont familyName: familyName pointSize: 10.
	f11 _ AbstractFont familyName: familyName pointSize: 11.
	f12 _ AbstractFont familyName: familyName pointSize: 12.


	f9 takeGlyphFor: $. from: $. in: f12.
	f9 takeGlyphFor: $, from: $, in: f12.
	
	form _ f9 glyphAt: $..
	form copy: (0@9 extent: 3@14) from: form to: 0@4 rule: Form and.
	f9 glyphAt: $: put: form.

	form _ f9 glyphAt: $,.
	form2 _ f9 glyphAt: $..
	form copy: (0@9 extent: 3@14) from: form2 to: 1@4 rule: Form and.
	f9 glyphAt: $; put: form.



	f10 takeGlyphFor: $. from: $. in: f12.
	f10 takeGlyphFor: $, from: $, in: f12.

	form _ f10 glyphAt: $. .
	form copy: (0@9 extent: 3@14) from: form to: 0@4 rule: Form and.
	f10 glyphAt: $: put: form.

	form _ f10 glyphAt: $,.
	form2 _ f10 glyphAt: $..
	form copy: (0@9 extent: 3@14) from: form2 to: 1@4 rule: Form and.
	f10 glyphAt: $; put: form.



	f11 takeGlyphFor: $. from: $. in: f12.
	f11 takeGlyphFor: $, from: $, in: f12.
	f11 takeGlyphFor: $: from: $: in: f12.
	f11 takeGlyphFor: $; from: $; in: f12! !


!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:28'!
aleph
	"
	Character aleph
	"
	^ self value: 16r8D! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:37'!
bullet
	"
	Character bullet
	"
	^ self value: 16r9F! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:23'!
complexNumbers
	"
	Character complexNumbers
	"
	^ self value: 16r86! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:32'!
contourIntegral
	"
	Character contourIntegral
	"
	^ self value: 16r93! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:30'!
directProduct
	"
	Character directProduct
	"
	^ self value: 16r8E! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:30'!
directSum
	"
	Character directSum
	"
	^ self value: 16r8F! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:38'!
dot
	"
	Character dot
	"
	^ self value: 16rB7! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:24'!
emptySet
	"
	Character emptySet
	"
	^ self value: 16r84! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:22'!
forAll
	"
	Character forAll
	"
	^ self value: 16r80! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:37'!
greaterNotEqual
	"
	Character greaterNotEqual
	"
	^ self value: 16r9D! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:35'!
greaterOrEqual
	"
	Character greaterOrEqual
	"
	^ self value: 16r99! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:36'!
greaterOverEqual
	"
	Character greaterOverEqual
	"
	^ self value: 16r9B! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:25'!
hamilton
	"
	Character hamilton
	"
	^ self value: 16r87! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:34'!
identical
	"
	Character identical
	"
	^ self value: 16r95! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:24'!
infinity
	"
	Character infinity
	"
	^ self value: 16r85! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:27'!
integerNumbers
	"
	Character integerNumbers
	"
	^ self value: 16r8C! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:31'!
integral
	"
	Character integral
	"
	^ self value: 16r92! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:36'!
lessNotEqual
	"
	Character lessNotEqual
	"
	^ self value: 16r9C! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:34'!
lessOrEqual
	"
	Character lessOrEqual
	"
	^ self value: 16r98! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:36'!
lessOverEqual
	"
	Character lessOverEqual
	"
	^ self value: 16r9A! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:26'!
naturalNumbers
	"
	Character naturalNumbers
	"
	^ self value: 16r88! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:34'!
notEqual
	"
	Character notEqual
	"
	^ self value: 16r94! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:34'!
notIdentical
	"
	Character notIdentical
	"
	^ self value: 16r96! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:22'!
partialDifferential
	"
	Character partialDifferential
	"
	^ self value: 16r81! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:26'!
primeNumbers
	"
	Character primeNumbers
	"
	^ self value: 16r89! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:26'!
rationalNumbers
	"
	Character rationalNumbers
	"
	^ self value: 16r8A! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:27'!
realNumbers
	"
	Character realNumbers
	"
	^ self value: 16r8B! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:37'!
ring
	"
	Character ring
	"
	^ self value: 16r9E! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:35'!
strictlyEquivalent
	"
	Character strictlyEquivalent
	"
	^ self value: 16r97! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:31'!
summation
	"
	Character summation
	"
	^ self value: 16r91! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:30'!
tensorProduct
	"
	Character tensorProduct
	"
	^ self value: 16r90! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:23'!
thereDoesNotExists
	"
	Character thereDoesNotExists
	"
	^ self value: 16r83! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 12/1/2015 09:23'!
thereExists
	"
	Character thereExists
	"
	^ self value: 16r82! !


!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 11/30/2015 16:14'!
useLeftArrow
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 28.
	characterToGlyphMap at: 95 put: 30! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 11/30/2015 16:14'!
useUnderscore
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 95.
	characterToGlyphMap at: 95 put: 94! !

!StrikeFont methodsFor: 'building' stamp: 'jmv 11/30/2015 16:16'!
buildFromForm: allGlyphs data: data name: aString

	| x |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.
	
	minAscii _ 28.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs depth > 16 ifTrue: [ allGlyphs asFormOfDepth: 16 ] ifFalse: [ allGlyphs ].
	x _ 0.
	minAscii to: maxAscii+1 do: [ :i |
		x _ (data at: i-minAscii+4) asNumber.
		xTable at: i+1 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil.
	
	self useLeftArrow! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 11/30/2015 16:45'!
create: fontName size: pointSize bold: includeBold italic: includeItalic boldItalic: includeBoldItalic
	"
	self create: 'DejaVu Sans Mono' size: 12 bold: true italic: true boldItalic: true
	"
	| base bold oblique boldOblique point prefix |
	prefix _ 'AdditionalFontData/', fontName.
	fontName = 'DejaVu' ifTrue: [
		prefix _ 'AdditionalFontData/DejaVu Sans' ].
	point _ pointSize asString.
	base _ [ (StrikeFont new
		buildFromForm: ((Form fromFileNamed: prefix, '-0-', point, '.bmp') asFormOfDepth: 16)
		data: (prefix, '-0-', point, '.txt') asFileEntry fileContents substrings
		name: fontName, ' ', point)
			pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ].
	includeBold ifTrue: [
		bold _ [ (StrikeFont new
			buildFromForm: ((Form fromFileNamed: prefix, '-1-', point, '.bmp') asFormOfDepth: 16)
			data: (prefix, '-1-', point, '.txt') asFileEntry fileContents substrings
			name: fontName, ' ', point, 'B')
				emphasis: 1;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeItalic ifTrue: [
		oblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileNamed: prefix, '-2-', point, '.bmp') asFormOfDepth: 16)
			data: (prefix, '-2-', point, '.txt') asFileEntry fileContents substrings
			name: fontName, ' ', point, 'I')
				emphasis: 2;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeBoldItalic ifTrue: [
		boldOblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileNamed: prefix, '-3-', point, '.bmp') asFormOfDepth: 16)
			data: (prefix, '-3-', point, '.txt') asFileEntry fileContents substrings
			name: fontName, ' ', point, 'BI')
				emphasis: 3;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	"We have a regular, base font. Make others derivatives of it"
	base ifNotNil: [
		bold ifNotNil: [
			base derivativeFont: bold at: 1 ].
		oblique ifNotNil: [
			base derivativeFont: oblique at: 2].
		boldOblique ifNotNil: [
			base derivativeFont: boldOblique at: 3 ].
		^base ].
	"We don't have a base, regular font."
	oblique ifNotNil: [
		oblique emphasis: 0.	"Hacky. Non regular fonts can not have derivatives. Should change this?"
		bold ifNotNil: [
			oblique derivativeFont: bold at: 1 ].
		boldOblique ifNotNil: [
			oblique derivativeFont: boldOblique at: 3 ].
		^oblique ].
	bold ifNotNil: [
		bold emphasis: 0.	"Hacky. Non regular fonts can not have derivatives. Should change this?"
		boldOblique ifNotNil: [
			bold derivativeFont: boldOblique at: 3 ].
		^bold ].
	boldOblique ifNotNil: [
		^boldOblique ].
	^nil! !

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 11/30/2015 16:48'!
install: aString
"
StrikeFont install: 'DejaVu'.
StrikeFont buildLargerPunctuation: 'DejaVu'.

StrikeFont install: 'DejaVu Sans Mono'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans Mono'.
"
"
StrikeFont install: 'Inconsolata'
StrikeFont install: '#PilGi'
StrikeFont install: 'Optima'
StrikeFont install: 'Herculanum'
StrikeFont install: 'Papyrus'
StrikeFont install: 'Handwriting - Dakota'
StrikeFont install: 'Times New Roman'
StrikeFont install: 'Apple Chancery'
StrikeFont install: 'Cochin'
StrikeFont install: 'Cracked'
StrikeFont install: 'Zapfino'
StrikeFont install: 'Brush Script MT'
StrikeFont install: 'Chalkboard'
"
	| fontDict |
	fontDict _ Dictionary new.
	"Just try a lot of sizes. Will ignore missing files."
	1 to: 200 do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			fontDict
				at: s
				put: font ]].
	fontDict notEmpty ifTrue: [
		AvailableFonts at: aString put: fontDict ].
	Preferences restoreDefaultFonts! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 11/30/2015 16:25'!
initializeUnicodeCodePoints
	"
	String streamContents: [ :strm | 28 to: 255 do: [ :i | strm nextPut: (Character value: i) ]]
	"


	"Initialize the table of Unicode code points"
	UnicodeCodePoints _ Array new: 256.
	0 to: 255 do: [ :code |
		UnicodeCodePoints at: code + 1 put: code ].
	
	"The following codes are different in ISO 8859-15 from those in ISO 8859-1,
	so the character code is not equal to the Unicode code point"
	UnicodeCodePoints at: 16rA4+1 put: 16r20AC.		"euro sign"
	UnicodeCodePoints at: 16rA6+1 put: 16r160.		"latin capital letter S with caron"
	UnicodeCodePoints at: 16rA8+1 put: 16r161.		"latin small letter s with caron"
	UnicodeCodePoints at: 16rB4+1 put: 16r17D.		"latin capital letter Z with caron"
	UnicodeCodePoints at: 16rB8+1 put: 16r17E.		"latin small letter z with caron"
	UnicodeCodePoints at: 16rBC+1 put: 16r152.		"latin capital ligature OE"
	UnicodeCodePoints at: 16rBD+1 put: 16r153.		"latin small ligature oe"
	UnicodeCodePoints at: 16rBE+1 put: 16r178.		"latin capital letter Y with diaeresis"

	UnicodeCodePoints at: 16r80+1 put: 16r2200.		"FOR ALL"
	UnicodeCodePoints at: 16r81+1 put: 16r2202.		"PARTIAL DIFFERENTIAL"
	UnicodeCodePoints at: 16r82+1 put: 16r2203.		"THERE EXISTS"
	UnicodeCodePoints at: 16r83+1 put: 16r2204.		"THERE DOES NOT EXIST"
	UnicodeCodePoints at: 16r84+1 put: 16r2205.		"EMPTY SET"
	UnicodeCodePoints at: 16r85+1 put: 16r221E.		"INFINITY"
	UnicodeCodePoints at: 16r86+1 put: 16r2102.		"DOUBLE-STRUCK CAPITAL C"
	UnicodeCodePoints at: 16r87+1 put: 16r210D.		"DOUBLE-STRUCK CAPITAL H"
	UnicodeCodePoints at: 16r88+1 put: 16r2115.		"DOUBLE-STRUCK CAPITAL N"
	UnicodeCodePoints at: 16r89+1 put: 16r2119.		"DOUBLE-STRUCK CAPITAL P"
	UnicodeCodePoints at: 16r8A+1 put: 16r211A.		"DOUBLE-STRUCK CAPITAL Q"
	UnicodeCodePoints at: 16r8B+1 put: 16r211D.		"DOUBLE-STRUCK CAPITAL R"
	UnicodeCodePoints at: 16r8C+1 put: 16r2124.		"DOUBLE-STRUCK CAPITAL Z"
	UnicodeCodePoints at: 16r8D+1 put: 16r2135.		"ALEF SYMBOL"
	UnicodeCodePoints at: 16r8E+1 put: 16r2A00.		"N-ARY CIRCLED DOT OPERATOR"
	UnicodeCodePoints at: 16r8F+1 put: 16r2A01.		"N-ARY CIRCLED PLUS OPERATOR"
	UnicodeCodePoints at: 16r90+1 put: 16r2A02.		"N-ARY CIRCLED TIMES OPERATOR"
	UnicodeCodePoints at: 16r91+1 put: 16r2211.		"N-ARY SUMMATION"
	UnicodeCodePoints at: 16r92+1 put: 16r222B.		"INTEGRAL"
	UnicodeCodePoints at: 16r93+1 put: 16r2A15.		"INTEGRAL AROUND A POINT OPERATOR"
	UnicodeCodePoints at: 16r94+1 put: 16r2260.		"NOT EQUAL TO"
	UnicodeCodePoints at: 16r95+1 put: 16r2261.		"IDENTICAL TO"
	UnicodeCodePoints at: 16r96+1 put: 16r2262.		"NOT IDENTICAL TO"
	UnicodeCodePoints at: 16r97+1 put: 16r2263.		"STRICTLY EQUIVALENT TO"
	UnicodeCodePoints at: 16r98+1 put: 16r2264.		"LESS-THAN OR EQUAL TO"
	UnicodeCodePoints at: 16r99+1 put: 16r2265.		"GREATER-THAN OR EQUAL TO"
	UnicodeCodePoints at: 16r9A+1 put: 16r2266.		"LESS-THAN OVER EQUAL TO"
	UnicodeCodePoints at: 16r9B+1 put: 16r2267.		"GREATER-THAN OVER EQUAL TO"
	UnicodeCodePoints at: 16r9C+1 put: 16r2268.		"LESS-THAN BUT NOT EQUAL TO"
	UnicodeCodePoints at: 16r9D+1 put: 16r2269.		"GREATER-THAN BUT NOT EQUAL TO"
	UnicodeCodePoints at: 16r9E+1 put: 16r2218.		"RING OPERATOR"
	UnicodeCodePoints at: 16r9F+1 put: 16r2219.		"BULLET OPERATOR"! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/30/2015 16:21'!
arrowDown
	"
	Character arrowDown
	"
	^ self value: 31! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/30/2015 16:21'!
arrowLeft
	"
	Character arrowLeft
	"
	^ self value: 28! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/30/2015 16:22'!
arrowRight
	"
	Character arrowRight
	"
	^ self value: 29! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/30/2015 16:22'!
arrowUp
	"
	Character arrowUp
	"
	^ self value: 30! !

!methodRemoval: Character class #identicalTo!
Character class removeSelector: #identicalTo!
!methodRemoval: Character class #notEqualTo!
Character class removeSelector: #notEqualTo!
!methodRemoval: Character class #notIdenticalTo!
Character class removeSelector: #notIdenticalTo!
!methodRemoval: Character class #strictlyEquivalentTo!
Character class removeSelector: #strictlyEquivalentTo!

!Character class reorganize!
('class initialization' accentedLetters initialize initializeClassificationTable initializeUnicodeCodePoints uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' allCharacters digitValue: new safeValue: separators unicodeCodePoint: value:)
('accessing untypeable characters' arrowDown arrowLeft arrowRight arrowUp backspace cr crCharacter delete end enter escape euro home insert lf lfCharacter nbsp newLineCharacter newPage null pageDown pageUp space tab)
('constants' characterTable)
('converting' evaluate:withUtf8BytesOfUnicodeCodePoint: iso8859s15CodeForUnicodeCodePoint: latin1ToMacRoman: macRomanToLatin1: nextUnicodeCodePointFromUtf8: unicodeCodePointOfUtf8Bytes: utf8BytesOfUnicodeCodePoint:)
('accessing mathematical symbols' aleph bullet complexNumbers contourIntegral directProduct directSum dot emptySet forAll greaterNotEqual greaterOrEqual greaterOverEqual hamilton identical infinity integerNumbers integral lessNotEqual lessOrEqual lessOverEqual naturalNumbers notEqual notIdentical partialDifferential primeNumbers rationalNumbers realNumbers ring strictlyEquivalent summation tensorProduct thereDoesNotExists thereExists)
!

!methodRemoval: StrikeFont class #buildLargerPunctuation!
StrikeFont class removeSelector: #buildLargerPunctuation!
!methodRemoval: StrikeFont class #useLargerPunctuation!
StrikeFont class removeSelector: #useLargerPunctuation!
!methodRemoval: StrikeFont class #useRegularPunctuation!
StrikeFont class removeSelector: #useRegularPunctuation!
!methodRemoval: StrikeFont #useLargerPunctuation!
StrikeFont removeSelector: #useLargerPunctuation!
!methodRemoval: StrikeFont #useRegularPunctuation!
StrikeFont removeSelector: #useRegularPunctuation!
