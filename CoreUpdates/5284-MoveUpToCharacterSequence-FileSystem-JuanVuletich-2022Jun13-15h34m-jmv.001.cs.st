'From Cuis 6.0 [latest update: #5280] on 13 June 2022 at 4:28:36 pm'!

!CharacterSet methodsFor: 'testing' stamp: 'jmv 6/13/2022 15:55:38'!
isAscii
	"Answer true if we only contain ASCII characters
	(CharacterSet newFrom: { $a. $b }) isAscii.
	(CharacterSet newFrom: { $á. $ê }) isAscii.
	"

	^(String indexOfByte: 1 inString: map startingAt: 128) = 0! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 16:07:33'!
asFileName
	"Answer a String made up from the receiver that is an acceptable file name."

	| badChars |

	"Check if the file name contains any invalid characters"
	badChars _ `CharacterSet newFrom: #( $: $< $> $| $/ $\ $? $* $")`.
	(self indexOfAnyOf: badChars) = 0
		ifTrue: [^self ].
	^ self collect: [ :char |
			(badChars includes: char) 
				ifTrue: [$#] 
				ifFalse: [char]]! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 6/13/2022 16:12:45'!
asAbsolutePathName
	"See comment at #isAbsolutePathName"

	^ self species streamContents: [ :childPath |
		childPath nextPut: $/.
		(FileIOAccessor default absolutePathComponentsFor: self)
			do: [ :each | childPath nextPutAll: each]
			separatedBy: [childPath nextPut: $/]]! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 6/13/2022 16:18:29'!
asDirectoryEntry
	"See examples in #asFileEntry method comment"
	^DirectoryEntry withPathName: self! !

!CharacterSequence methodsFor: 'fileman-actions' stamp: 'mu 6/11/2006 22:12'!
fileContents
	^self asFileEntry fileContents! !

!CharacterSequence methodsFor: 'fileman-actions' stamp: 'mu 6/11/2006 22:19'!
fileContents: aString
	self asFileEntry fileContents: aString.
	^aString! !


!String methodsFor: 'accessing' stamp: 'jmv 6/13/2022 15:43:32'!
indexOfAnyOf: aCharacterSet
	"returns the index of the first character in the given set, starting from start"

	^ self indexOfAnyOf: aCharacterSet startingAt: 1 ifAbsent: 0! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/13/2022 16:03:08'!
indexOfAnyOf: aCharacterSet
	"returns the index of the first character in the given set, starting from start."

	"Fast lane."
	| index |
	(self isAscii or: [aCharacterSet isAscii]) ifTrue: [
		^ bytes indexOfAnyOf: aCharacterSet startingAt: 1 ifAbsent: 0 ].

	index _ 1.
	self do: [ :codePoint |
		(aCharacterSet includes: codePoint) ifTrue: [ ^index ].
		index _ index + 1].

	^0! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 6/13/2022 15:51:37'!
add: aCharacter
	map at: aCharacter iso8859s15Code+1 put: 1.! !

!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 6/13/2022 16:07:09'!
includes: aCharacter
	aCharacter isCharacter ifFalse: [ ^ false ].
	aCharacter iso8859s15Code ifNotNil: [ :index | ^(map at: index + 1) > 0 ].
	^false! !

!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 6/13/2022 15:52:07'!
remove: aCharacter
	map at: aCharacter iso8859s15Code+1 put: 0! !


!CharacterSequence methodsFor: 'converting' stamp: 'ls 10/28/2003 12:24'!
asSmalltalkComment
	"return this string, munged so that it can be treated as a comment in Smalltalk code.  Quote marks are added to the beginning and end of the string, and whenever a solitary quote mark appears within the string, it is doubled"

	^self species streamContents:  [ :str |
		| quoteCount first |

		str nextPut: $".
	
		quoteCount := 0.
		first := true.
		self do: [ :char |
			char = $"
				ifTrue: [
					first ifFalse: [
						str nextPut: char.
						quoteCount := quoteCount + 1 ] ]
				ifFalse: [
					quoteCount odd ifTrue: [
						"add a quote to even the number of quotes in a row"
						str nextPut: $" ].
					quoteCount := 0.
					str nextPut: char ].
			first := false ]. 

		quoteCount odd ifTrue: [
			"check at the end"
			str nextPut: $". ].

		str nextPut: $".
	].
	! !

!methodRemoval: FileIOAccessor #checkName:fixErrors: stamp: 'jmv 6/13/2022 16:08:09'!
FileIOAccessor removeSelector: #checkName:fixErrors:!
!methodRemoval: String #fileContents stamp: 'jmv 6/13/2022 16:20:44'!
String removeSelector: #fileContents!
!methodRemoval: String #fileContents: stamp: 'jmv 6/13/2022 16:21:54'!
String removeSelector: #fileContents:!
!methodRemoval: String #asFileName stamp: 'jmv 6/13/2022 16:08:03'!
String removeSelector: #asFileName!
!methodRemoval: String #asDirectoryEntry stamp: 'jmv 6/13/2022 16:18:29'!
String removeSelector: #asDirectoryEntry!
!methodRemoval: String #asAbsolutePathName stamp: 'jmv 6/13/2022 16:12:06'!
String removeSelector: #asAbsolutePathName!
!methodRemoval: CharacterSequence #indexOfAnyOf: stamp: 'jmv 6/13/2022 15:47:37'!
CharacterSequence removeSelector: #indexOfAnyOf:!

!CharacterSet reorganize!
('collection ops' add: do: includes: remove:)
('conversion' complement)
('testing' isAscii)
('comparison' = hash)
('private' byteArrayMap initialize)
!


!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findString:in:startingAt: findString:startingAt: findString:startingAt:caseSensitive: findStringIgnoreCase:in:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size string:lineIndicesDo:)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: compare: compare:caseSensitive: crc16 hash is:equalTo: is:substringAt: sameAs:)
('copying' , appendToString:)
('converting' asByteString asHex asLowercase asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asUtf8BytesOrByteString base64Decoded capitalized fixLineEndings string:withLineEndings: substrings substrings: surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('private' replaceFrom:to:with:startingAt:)
('system primitives' compare:with: compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty isRemote)
('paragraph support' encompassLine: encompassParagraph:)
('fileman-converting')
('fileman-actions')
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
!

