'From Cuis 4.1 of 12 December 2012 [latest update: #1603] on 20 February 2013 at 8:12:30 pm'!

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/20/2013 20:09'!
testFromUtf8
	"
	CharacterTest new testFromUtf8
	"
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: '24')) hex = '16r24'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'C2A2')) hex = '16rA2'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'E282AC')) hex = '16r20AC'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'F0A4ADA2')) hex = '16r24B62'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/20/2013 20:07'!
testSomeLatinCharsFromUtf8
	"
	CharacterTest new testSomeLatinCharsFromUtf8
	"
	| bytes string |
	bytes _ (ByteArray readHexFrom: 'C3A1C3A5C3A6C3B1C386C2A5C3BC') readStream.
	string _ String streamContents: [ :strm |
		[bytes atEnd ] whileFalse: [
			strm nextPut: (Character value: (Character nextUnicodeCodePointFromUtf8: bytes )) ]].
	self assert: string = 'áåæñÆ¥ü'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/20/2013 20:07'!
testSomeLatinCharsToUtf8
	"
	CharacterTest new testSomeLatinCharsToUtf8
	"
	| characters bytes |
	characters _ 'áåæñÆ¥ü' readStream.
	bytes _ ByteArray streamContents: [ :strm |
		[ characters atEnd ] whileFalse: [
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: characters next asciiValue ]].
	self assert: bytes hex = 'C3A1C3A5C3A6C3B1C386C2A5C3BC'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/20/2013 20:09'!
testToUtf8
	"
	CharacterTest new testToUtf8
	"
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r0024) hex =  '24'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r00A2) hex =  'C2A2'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r20AC) hex = 'E282AC'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r024B62) hex = 'F0A4ADA2'! !


!String methodsFor: 'converting' stamp: 'jmv 2/20/2013 20:06'!
asUtf8: convertEmbeddedNCRs
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
	Note: The conversion of NCRs is reversible. See #fromUtf8:hex:trimLastNull:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^ByteArray streamContents: [ :outStream | | inStream nextChar prevPos maybeUnicodeNCR ncrSize codePoint |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			(convertEmbeddedNCRs and: [ nextChar = $& ])
				ifTrue: [
					prevPos _ inStream position.
					maybeUnicodeNCR _ inStream next: 9.
					maybeUnicodeNCR first = $# ifTrue: [
						ncrSize _ maybeUnicodeNCR indexOf: $;.
						ncrSize = 0
							ifFalse: [
								codePoint _ maybeUnicodeNCR second = $x
									ifTrue: [ ('16r', (maybeUnicodeNCR copyFrom: 3 to: ncrSize) asUppercase) asNumber ]
									ifFalse: [ (maybeUnicodeNCR copyFrom: 2 to: ncrSize) asNumber ]]
							ifTrue: [
								"Not an NCR after all. Just add the $& and continue from there"
								codePoint _ nextChar unicodeCodePoint ].
						Character
							evaluate: [ :byte | outStream nextPut: byte ]
							withUtf8BytesOfUnicodeCodePoint: codePoint.
						inStream position: prevPos + ncrSize ]]
				ifFalse: [
					codePoint _ nextChar unicodeCodePoint.
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 2/20/2013 20:11'!
usefulExpressionsContents
	^ self class firstCommentAt: #usefulExpressionsContents

	"'Querying the system'.
Smalltalk aboutThisSystem.
SystemVersion current.
SystemDictionary copyright.
SystemOrganization categories.
SystemOrganization categoryOfElement: #Morph.
Editor organization categoryOfElement: #clearSelection.
Smalltalk allUnSentMessages.
Smalltalk browseAllUnSentMessages.


'find selectors
Remember you can also do World / Open / Method Names'.
Symbol selectorsContaining: 'rsCon'.
'Cuis uses iso885915 internally and UTF8 externally'.
Symbol selectorsContaining: 'iso8859'. 
Symbol selectorsContaining: 'utf8'. 


'Example for loading an external package; adapt to your needs'.
slash _ FileDirectory slash.
{
  'packages', slash, 'PetitParser', slash, 'PetitParser.pck' .
  'packages', slash, 'PetitParser', slash, 'PetitTests.pck' .
  'packages', slash, 'PetitParser', slash, 'PetitTutorial.pck'
} do: [ :fileName | CodeFileBrowser installPackage:
	(FileStream concreteStream readOnlyFileNamed: fileName) ].


'To create a *.pck.st file for each category with tests and  then to remove these categories'.
(SystemOrganization categoriesMatching: '*Test*')
	do: [ :cat | (CodePackage named: cat
				createIfAbsent: true
				registerIfNew: false) save.
			SystemOrganization removeSystemCategory: cat.
	].


'List of CodePackages which are not saved and save them in one go'.
'Save dirty packages:' print.
CodePackage installedPackages do: [ :each |
	each hasUnsavedChanges ifTrue: [
		each name print.
		each save ]].


'Globals'.
Cursor wait showWhile: [Sensor waitButton].
Smalltalk  inspectGlobals.


'Source code management'.
ChangeList browseRecentLog.


'Space and object count statistics'.
Smalltalk bytesLeft asStringWithCommas.
Symbol instanceCount.


'Anaylize memory usage by class'.
(String streamContents: [ :strm | SpaceTally new printSpaceAnalysis: 1 on: strm ]) edit.


'Remove a lot of stuff and reduce the image'.
Smalltalk reduceCuis.


'Performance measurement'.
Time millisecondsToRun: [Smalltalk allCallsOn: #asOop].
MessageTally spyOn: [Smalltalk allCallsOn: #asOop].


'Opening a Text editor'.
'Something' editLabel: 'Title'.


'Opening a workspace'.
Workspace new
		contents: '';
		openLabel: 'Workspace'.

'Morphic stuff'
LayoutMorph example1.				'Resize it'
LayoutMorph example11.			'Resize it. Play with separators.'
LayoutMorph example2.				'Resize it'
LayoutMorph example3.				'Resize it'
LayoutMorph example13.			'Resize it. Play with separators.'
LayoutMorph example6.				'Resize it'
LayoutMorph launcherExample.		'Have a Transcript open to use this'.

'Code authors - Potentially slow!!'.
Smalltalk knownInitialsAndNames do: [ :each | each print ].
Smalltalk allContributors do: [ :each | each print ].
Smalltalk contributionsOf: 'KenD'"! !

!methodRemoval: CharacterTest #testFromUTF8!
CharacterTest removeSelector: #testFromUTF8!
!methodRemoval: CharacterTest #testSomeLatinCharsFromUTF8!
CharacterTest removeSelector: #testSomeLatinCharsFromUTF8!
!methodRemoval: CharacterTest #testSomeLatinCharsToUTF8!
CharacterTest removeSelector: #testSomeLatinCharsToUTF8!
!methodRemoval: CharacterTest #testToUTF8!
CharacterTest removeSelector: #testToUTF8!
