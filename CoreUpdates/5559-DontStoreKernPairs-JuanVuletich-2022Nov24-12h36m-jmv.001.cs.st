'From Cuis 6.0 [latest update: #5557] on 24 November 2022 at 12:42:23 pm'!

!TTFontReader methodsFor: 'processing' stamp: 'jmv 11/24/2022 12:42:06'!
readFrom: fontData fromOffset: offset

	| headerEntry maxProfileEntry nameEntry indexLocEntry charMapEntry glyphEntry horzHeaderEntry horzMetricsEntry kerningEntry glyphOffset characterMap numHMetrics indexToLocFormat |

	"Search the tables required to build the font"
	(headerEntry := self getTableDirEntry: 'head' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a header table'].
	(maxProfileEntry := self getTableDirEntry: 'maxp' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a maximum profile table'].
	(nameEntry := self getTableDirEntry: 'name' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a name table'].
	(indexLocEntry := self getTableDirEntry: 'loca' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a relocation table'].
	(charMapEntry := self getTableDirEntry: 'cmap' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a character map table'].
	(glyphEntry := self getTableDirEntry: 'glyf' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a glyph table'].
	(horzHeaderEntry := self getTableDirEntry: 'hhea' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal header table'].
	(horzMetricsEntry := self getTableDirEntry: 'hmtx' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal metrics table'].
	(kerningEntry := self getTableDirEntry: 'kern' from: fontData offset: offset) == nil ifTrue: [
		"Transcript cr; show:'This font does not have a kerning table';endEntry" ].


	"Process the data"
	indexToLocFormat := self processFontHeaderTable: headerEntry.
	self processMaximumProfileTable: maxProfileEntry.
	self processNamingTable: nameEntry.
	glyphOffset := self processIndexToLocationTable: indexLocEntry format: indexToLocFormat.
	characterMap := self processCharacterMappingTable: charMapEntry.
	characterMap isNil
		ifTrue: [^self error:'This font has no Unicode character mappings'].
	self processGlyphDataTable: glyphEntry offsets: glyphOffset.
	numHMetrics := self processHorizontalHeaderTable: horzHeaderEntry.
	self processHorizontalMetricsTable: horzMetricsEntry length: numHMetrics.
	kerningEntry isNil 
		ifTrue: [kernPairs := #()]
		ifFalse: [self processKerningTable: kerningEntry].
	self mapAndStoreGlyphsUsing: characterMap.
	"jmv kernPairs refer to glyph number (in the .ttf file) while fontDescription glyphs are indexed by Unicode code point...
	Correct this before making any use of the kernPairs data."
	false ifTrue: [
		fontDescription setKernPairs: kernPairs ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans.ttf'!

