'From Cuis 6.0 [latest update: #5983] on 27 August 2023 at 8:38:19 pm'!

!TTFontReader methodsFor: 'public' stamp: 'bp 8/27/2023 10:07:09'!
readTTCFrom: fontData
	"
	Answer an Array of TTFontDescription.
	Note: The purpose of TTC is completely defeated: there will be no memory savings, as each
		TTFontDescription will include the glyph data for all the glyphs it includes.
		Actually sharing glyph data between TTFontDescriptions needs some work (jmv, 16-Sep-2022).
	https://docs.fileformat.com/font/ttc/
	https://docs.microsoft.com/en-us/typography/opentype/spec/otff#font-collections
	"
	| offset tag majorVersion minorVersion numFonts fontDescriptions |
	tag := fontData copyFrom: 1 to: 4.
	tag = `'ttcf' asUtf8Bytes` ifFalse: [
		^self error:'This TTF Collection does not have a correct header'].
	majorVersion := fontData uint16At: 5 bigEndian: true.
	minorVersion := fontData uint16At: 7 bigEndian: true.
	numFonts := fontData uint32At: 9 bigEndian: true.
	(#((1 0) (2 0)) includes: {majorVersion. minorVersion}) ifFalse: [
		^self error: 'This TTF Collection has an unsupported header version'].
	fontDescriptions := Array new: numFonts.
	1 to: numFonts do: [ :i |
		fontDescription := TTFontDescription new.
		offset := fontData uint32At: i*4+9 bigEndian: true.
		self readFrom: fontData fromOffset: offset.
		fontDescriptions at: i put: fontDescription.
		fontDescription := nil. "To avoid any confusion"
	].
	^fontDescriptions! !

