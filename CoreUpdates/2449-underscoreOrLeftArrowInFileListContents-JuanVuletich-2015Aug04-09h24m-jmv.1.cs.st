'From Cuis 4.2 of 25 July 2013 [latest update: #2448] on 4 August 2015 at 9:41:24.588777 am'!

!FileList methodsFor: 'private' stamp: 'jmv 8/4/2015 09:34'!
isFileSmalltalkCode
	^self fullName
		ifNil: [ false ]
		ifNotNil: [ :fullName | fullName endsWith: '.st' ]! !

!FileList methodsFor: 'private' stamp: 'jmv 8/4/2015 09:39'!
withUnderscoreGlyphsIfNotSmalltalkCode: aString
	^self isFileSmalltalkCode 
		ifTrue: [ aString ]
		ifFalse: [ aString asTextWithUnderscoreGlyphs ]! !


!String methodsFor: 'converting' stamp: 'jmv 8/4/2015 09:27'!
asTextWithUnderscoreGlyphs
	"Answer a Text whose string is the receiver, that uses underscore glyph
	(and not left arrow glyph)"

	^Text string: self attribute: TextEmphasis withUnderscoreGlyphs! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 8/4/2015 09:28'!
pattern

	^ (pattern ifNil: ['*']) asTextWithUnderscoreGlyphs! !

!FileList methodsFor: 'private' stamp: 'jmv 8/4/2015 09:28'!
defaultContents
	acceptedContentsCache _ (list
			ifNil: [String new]
			ifNotNil: [
				String streamContents: [ :s | 
					s nextPutAll: 'NO FILE SELECTED'; newLine.
					s nextPutAll: '  -- Folder Summary --'; newLine.
					list do: [ :item | s nextPutAll: item; newLine]]])
		asTextWithUnderscoreGlyphs.
	brevityState _ #FileList.
	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'jmv 8/4/2015 09:39'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| f fileSize first5000 |

	f := directory oldFileOrNoneNamed: self fullName.
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= 2000000]) ifTrue: [
		acceptedContentsCache _ self withUnderscoreGlyphsIfNotSmalltalkCode: f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	first5000 := f next: 5000.
	f close.
	acceptedContentsCache _ self withUnderscoreGlyphsIfNotSmalltalkCode:
('File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. fileSize. first5000}).
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !
