'From Cuis 6.0 [latest update: #5353] on 5 July 2022 at 11:56:18 am'!
!classDefinition: #TextModel category: #'System-Text'!
ActiveModel subclass: #TextModel
	instanceVariableNames: 'actualContents undoRedoCommands lastEditTimeStamp '
	classVariableNames: 'UnicodeContentsHandling '
	poolDictionaries: ''
	category: 'System-Text'!

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 7/5/2022 11:43:15'!
useExtendedAsciiEditors
	"
	Text editors will work on instances of String and Character.
	The supported character set is Extended ASCII, this is ISO 8859-15 plus some selected Greek letters, Math symbols and assorted characters.
	See #initializeUnicodeCodePoints
	This is appropriate when using StrikeFonts, but can also be used with TrueType.
	This mode can be used both with ExtendedAscii files or Utf8Files.
	See #useExtendedAsciiFiles and #useUtf8Files
	See also #useUtf8Editors
	TextModel useExtendedAsciiEditors
	"
	UnicodeContentsHandling _ #useExtendedAsciiEditors! !

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 7/5/2022 11:51:51'!
useNonConvertingEditors
	"
	Text editors will work both on instances of String/Character or Utf8String/UnicodeCodePoints.
	No conversions will be done.
	See also #useExtendedAsciiEditors and #useUtf8Editors
	The use of Utf8String requires TrueType fonts and Utf8Files (if saving to files is desired).
	TextModel useNonConvertingEditors
	"
	UnicodeContentsHandling _ nil! !

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 7/5/2022 11:46:25'!
useUtf8Editors
	"
	Text editors will work on instances of Utf8String and UnicodeCodePoint.
	This supports the whole Unicode Code Point range
	Requires the use of TrueType fonts. StrikeFonts can not be used.
	Requires the use of Utf8Files. ExtendedAscii files can not be used.
	See #useUtf8Files and #useExtendedAsciiFiles.
	See also #useExtendedAsciiEditors
	TextModel useUtf8Editors
	"
	Feature require: 'VectorGraphics'.
	FileIOAccessor useUtf8Files.
	UnicodeContentsHandling _ #useUtf8Editors! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/27/2022 13:05:24'!
asByteStringOrText
	"Answer is either a String or a Text including a String"

	^self asByteString! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/25/2022 17:11:23'!
asUtf8StringOrText
	"Answer is either an Utf8String or a Text including an Utf8String"

	^self asUtf8String! !


!Text methodsFor: 'converting' stamp: 'jmv 6/27/2022 13:05:58'!
asByteStringOrText
	"Answer is either a String or a Text including a String"

	string isByteString ifTrue: [ ^self ].
	^Text string: string asByteString runs: runs! !

!Text methodsFor: 'converting' stamp: 'jmv 6/25/2022 17:11:26'!
asUtf8StringOrText
	"Answer is either an Utf8String or a Text including an Utf8String"

	string isUtf8String ifTrue: [ ^self ].
	^Text string: string asUtf8String runs: runs! !


!TextModel methodsFor: 'accessing' stamp: 'jmv 6/27/2022 15:07:21'!
basicActualContents: aTextOrString
	"Do not throw events."
	| prevContents |
	prevContents _ actualContents.
	actualContents _ aTextOrString asText.
	UnicodeContentsHandling = #useUtf8Editors ifTrue: [
		actualContents _ actualContents asUtf8StringOrText ]
		ifFalse: [
			UnicodeContentsHandling = #useExtendedAsciiEditors ifTrue: [
				actualContents _ actualContents asByteStringOrText ]].
	actualContents = prevContents ifFalse: [	"Compares only characters, not attributes"
		undoRedoCommands resetToStart.
		lastEditTimeStamp _ nil ]! !

!TextModel methodsFor: 'initialization' stamp: 'jmv 6/27/2022 14:57:13'!
initialize
	"Initialize the state of the receiver with its default contents."

	undoRedoCommands _ ReadWriteStream on: #().
	self basicActualContents: ''.! !


!FileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 7/5/2022 11:55:19'!
useExtendedAsciiFiles
	"
	Use Extended Ascii (ISO 8859-15 plus a few Greek and Math symbols) for files.
	Only support instances of String and Character.
	Disables UTF-8 text editing by default.
	This is the traditional, pre-Unicode way for Cuis.

	FileIOAccessor useExtendedAsciiFiles
	"
	TextModel useNonConvertingEditors.
	FileIOAccessor concreteStreamClass: StandardFileStream! !

!classDefinition: #TextModel category: #'System-Text'!
ActiveModel subclass: #TextModel
	instanceVariableNames: 'actualContents undoRedoCommands lastEditTimeStamp'
	classVariableNames: 'UnicodeContentsHandling'
	poolDictionaries: ''
	category: 'System-Text'!
