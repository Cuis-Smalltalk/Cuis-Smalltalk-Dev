'From Cuis 4.1 of 12 December 2012 [latest update: #1759] on 17 July 2013 at 12:56:04.13233 am'!
'Description Please enter a description for this package.'!
!provides: 'RTFExporting' 1 0!
!requires: 'Graphics-Files-Additional' 1 nil!

!String methodsFor: '*rtfExporting' stamp: 'jmv 11/22/2011 14:52'!
iso8859s15ToRTFEncoding
	"Convert the given string to RTF escaped Unicode from the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"
	self assert: ('A¢¤' iso8859s15ToRTFEncoding) hex = ' 'A\u162?\u8364?''
	"
	| c cp |
	^String streamContents: [ :strm | | characters |
		characters _ self readStream.
		[ characters atEnd ] whileFalse: [
			c _  characters next.
			cp _ c unicodeCodePoint.
			cp < 128
				ifTrue: [ strm nextPut: c ]
				ifFalse: [
					strm
						nextPut: $\;
						nextPut: $u.
					cp printOn: strm.
					strm nextPut: $? ]]]! !

!Text methodsFor: '*rtfExporting' stamp: 'jmv 11/22/2011 15:30'!
rtfString
	"
	| text |
	text _ RTFConversionTest textSample1 asStyledText.
	StyledTextModel new contents: text; openAsStyledEditorLabel: 'Styled Text Editor' inWorld: World.
	Clipboard default storeObject: text
	"
	| prevAttributes colors fonts s |
	"Build colors and fonts tables"
	colors _ Set new.
	fonts _ Set new.
	runs withStartStopAndValueDo: [ :start :stop :attributes |
		attributes do: [ :attribute |
			attribute forParagraphStyleReferenceDo: [ :ts | ts color ifNotNil: [ :color | colors add: color ]].
			attribute forCharacterStyleReferenceDo: [ :cs | cs color ifNotNil: [ : color | colors add: color ]].
			attribute forTextColorDo: [ :color | colors add: color ].
			attribute forBaseFontDo: [ :font | fonts add: font familyName ]]].
	colors _ colors asArray.
	fonts _ fonts asArray.
	^String
		streamContents: [ :strm |
			self writeRTFHeaderOn: strm colorTable: colors fontTable: fonts.
			prevAttributes _ #().
			runs withStartStopAndValueDo: [ :start :stop :attributes | | currentAttributes actualStart |
				currentAttributes _ attributes asSet.
				"Close attributes no longer present"
				prevAttributes do: [ :each |
					(currentAttributes includes: each) ifFalse: [
						each writeRTFStopOn: strm colorTable: colors fontTable: fonts ]].
				"Open attributes not previously present"
				actualStart _ start.
				currentAttributes do: [ :each |
					"Repeat existing, because the closing of other attributes, in some cases, sets defaults, and not the now active values...
					For example, finishing a CharStyle sets text to black. But what if the ParaStyle indicated some other color?"
					"(prevAttributes includes: each) ifFalse: ["
						actualStart _ actualStart + (each writeRTFStartOn: strm colorTable: colors fontTable: fonts) 
						"]"
					].
				"Add string now"
				s _ string copyFrom: actualStart to: stop.
				s _ s withLineEndings: '\par '.
				s _ s iso8859s15ToRTFEncoding.
				strm nextPutAll: s.
				prevAttributes _ currentAttributes ].
			strm nextPut: $} ]
		estimatedSize: string size! !

!Text methodsFor: '*rtfExporting' stamp: 'jmv 3/13/2012 12:55'!
writeRTFHeaderOn: aStream colorTable: colorArray fontTable: fontNamesArray

	aStream nextPutAll: '{\rtf'; newLine.

	aStream nextPutAll: '{\colortbl;'.
	colorArray do: [:each | 
		aStream
			nextPutAll: '\red'; nextPutAll: (each red * 255) rounded asString;
			nextPutAll: '\green'; nextPutAll: (each green * 255) rounded asString;
			nextPutAll: '\blue'; nextPutAll: (each blue * 255) rounded asString;
			nextPut: $; ].
	aStream nextPutAll: '}'; newLine! !

!Text class methodsFor: '*rtfExporting' stamp: 'jmv 11/1/2011 10:40'!
pointSizeConversionFactor
	"This constant is used for conversion of font point sizes ofr RTF export and import.
	In theory, it shouldn't be needed. After all, a 72point font in RTF should be imported as a 72 point font in Cuis.
	
	Cuis uses 96 pixels per inch. This means that the inter baseline height for a 72 point font is (at least) 96 pixels.
	This is also the convention used (by default) on Windows, so fonts look about the same in Cuis and in Windows native software (such as Wordpad).
	But the Mac uses (by default) a convention of 72 pixels per inch. So, text on the Mac looks smaller.
	If this method is modified to answer 96/72, then text on the Mac will look the same as in Cuis. This could be good to make Mac users happy, as long as we never show them the point size we believe our fonts have. The font that they happily use export and import content, and to look side by side, that is called '12 points' in Mac software, well call '9 point'"
	"^1.33333"
	^1! !

!TextAttribute methodsFor: '*rtfExporting' stamp: 'jmv 1/24/2011 11:51'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"
	^0! !

!TextAttribute methodsFor: '*rtfExporting' stamp: 'jmv 1/21/2011 13:41'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop."! !

!TextAlignment methodsFor: '*rtfExporting' stamp: 'jmv 1/24/2011 11:52'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"
	alignment caseOf: {
		[ 0 ] -> [ aStream nextPutAll: '\ql ' ].
		[ 1 ] -> [ aStream nextPutAll: '\qr ' ].
		[ 2 ] -> [ aStream nextPutAll: '\qc ' ].
		[ 3 ] -> [ aStream nextPutAll: '\qj ' ] }.
	^0! !

!TextAlignment methodsFor: '*rtfExporting' stamp: 'jmv 1/21/2011 14:57'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop."
	aStream nextPutAll: '\ql '! !

!TextAnchor methodsFor: '*rtfExporting' stamp: 'jmv 4/7/2011 15:08'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip: one in this case"
	aStream
		nextPutAll: '{\*\shppict{\pict\pngblip ';
		nextPutAll: (PNGReadWriter bytesFor: anchoredFormOrMorph) hex;
		nextPutAll: '}}'.
	"Skip the character holding the attribute"
	^1! !

!TextAnchor methodsFor: '*rtfExporting' stamp: 'jmv 1/24/2011 11:12'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop."! !

!TextColor methodsFor: '*rtfExporting' stamp: 'jmv 1/24/2011 11:52'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"
	aStream nextPutAll: '\cf'; nextPutAll: (colorArray indexOf: color) asString; space.
	^0! !

!TextColor methodsFor: '*rtfExporting' stamp: 'jmv 1/21/2011 13:51'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop."
	aStream nextPutAll: '\cf0 '! !

!TextEmphasis methodsFor: '*rtfExporting' stamp: 'jmv 4/12/2011 09:59'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"

	(emphasisCode allMask: AbstractFont boldCode) ifTrue: [
		aStream nextPutAll: '\b ' ].
	(emphasisCode allMask: AbstractFont italicCode) ifTrue: [
		aStream nextPutAll: '\i ' ].
	(emphasisCode allMask: AbstractFont underlinedCode) ifTrue: [
		aStream nextPutAll: '\ul ' ].
	(emphasisCode allMask: AbstractFont struckThroughCode) ifTrue: [
		aStream nextPutAll: '\strike \strikec0 ' ].
	(emphasisCode allMask: AbstractFont outlineCode) ifTrue: [
		aStream nextPutAll: '\outl\strokewidth60 ' ].
	^0! !

!TextEmphasis methodsFor: '*rtfExporting' stamp: 'jmv 4/12/2011 09:59'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop.
	Do it in the inverse order of writeRTFStartOn:colorTable:fontTable:"

	(emphasisCode allMask: AbstractFont outlineCode) ifTrue: [
		aStream nextPutAll: '\outl0\strokewidth0 ' ].
	(emphasisCode allMask: AbstractFont struckThroughCode) ifTrue: [
		aStream nextPutAll: '\strike0\striked0 ' ].
	(emphasisCode allMask: AbstractFont underlinedCode) ifTrue: [
		aStream nextPutAll: '\ulnone ' ].
	(emphasisCode allMask: AbstractFont italicCode) ifTrue: [
		aStream nextPutAll: '\i0 ' ].
	(emphasisCode allMask: AbstractFont boldCode) ifTrue: [
		aStream nextPutAll: '\b0 ' ]! !

!TextFontFamilyAndSize methodsFor: '*rtfExporting' stamp: 'jmv 4/12/2011 09:06'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"
	"We should also reference familyName, in the table with \f# where # is the number in the table..."

	aStream nextPutAll: '\fs'; nextPutAll: ((pointSize * Text pointSizeConversionFactor ) rounded * 2) asString; space.
	^0! !

!TextFontFamilyAndSize methodsFor: '*rtfExporting' stamp: 'jmv 4/7/2011 15:20'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop."
	aStream nextPutAll: '\fs0 '! !

!TextKern methodsFor: '*rtfExporting' stamp: 'jmv 4/7/2011 15:21'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"
	| quarterPoints twips |
	quarterPoints _ kern * 4.	"This maybe needs tweaking!!"
	twips _ kern * 12.
	aStream nextPutAll:
		'\kerning1\expnd', quarterPoints asString,
		'\expndtw', twips asString; space.
	^0! !

!TextKern methodsFor: '*rtfExporting' stamp: 'jmv 1/24/2011 11:52'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop."
	aStream nextPutAll: '\kerning1\expnd0\expndtw0 ' ! !
