"
Superclass for String classes
"
Class {
	#name : 'CharacterSequence',
	#superclass : 'SequenceableCollection',
	#category : 'Kernel-Strings',
	#package : 'Kernel',
	#tag : 'Strings'
}

{ #category : 'instance creation' }
CharacterSequence class >> fromUtf8Bytes: aByteArray [
	"Answer will be an instance of String if aByteArray contains strictly ASCII characters.
	Answer will be an instance of UnicodeString if any byte is over 127, meaning UTF-8 encoded Code Points."

	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	^UnicodeString fromUtf8Bytes: aByteArray.
]

{ #category : 'instance creation' }
CharacterSequence class >> new [
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numElements
]

{ #category : 'instance creation' }
CharacterSequence class >> new: size withAll: element  [
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^ self streamContents: [ :strm |
		size timesRepeat: [ strm nextPut: element ]].
]

{ #category : 'instance creation' }
CharacterSequence class >> numElements [
	"Some subclasses create instances of a specific size, and a non-zero number"
	^0
]

{ #category : 'fileman-constants' }
CharacterSequence class >> pathSeparators [
	^'\/'
]

{ #category : 'instance creation' }
CharacterSequence class >> readFrom: inStream [
	"Answer a new String that is determined by reading the stream, inStream.  Embedded
	double quotes become the quote Character."

	^self streamContents: [ :outStream |
		| done |
		"go to first quote"
		inStream skipTo: $'.
		done := false.
		[done or: [inStream atEnd]]
			whileFalse: 
				[ |char|
				char := inStream next.
				char == $'
					ifTrue: 
						[char := inStream next.
						char == $'
							ifTrue: [outStream nextPut: char]
							ifFalse: [done := true]]
					ifFalse: [outStream nextPut: char]]].
]

{ #category : 'testing' }
CharacterSequence class >> representsSymbols [
	^false
]

{ #category : 'arithmetic' }
CharacterSequence >> * arg [

	self shouldNotImplement.
]

{ #category : 'arithmetic' }
CharacterSequence >> + arg [

	self shouldNotImplement.
]

{ #category : 'arithmetic' }
CharacterSequence >> - arg [

	self shouldNotImplement.
]

{ #category : 'arithmetic' }
CharacterSequence >> / arg [
	"If working with file paths, just use $/
	Or better yet, use DirectoryEntry protocol"

	self shouldNotImplement
]

{ #category : 'arithmetic' }
CharacterSequence >> // arg [

	self shouldNotImplement.
]

{ #category : 'comparing' }
CharacterSequence >> < another [
	"Answer whether the receiver sorts before another.
	The collation order is case sensitive.
	Note: Behavior the same for String, Symbol, UnicodeString, UnicodeSymbol and Text,
	both as receiver and as argument, if they hold same Characters."

	self subclassResponsibility.
]

{ #category : 'comparing' }
CharacterSequence >> <= another [
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."
	"Reduce all cases to #< and #= to simplify code."

	"Comparing for equality may is faster in most casts. Try it first."
	self = another ifTrue: [ ^true ].

	^self < another.
]

{ #category : 'comparing' }
CharacterSequence >> = another [
	"Note: Behavior the same for String, Symbol, UnicodeString, UnicodeSymbol and Text,
	both as receiver and as argument, if they hold same Characters."

	self subclassResponsibility
]

{ #category : 'comparing' }
CharacterSequence >> > another [
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."
	"Reduce all cases to #< and #= to simplify code."

	"Comparing for equality may is faster in most casts. Try it first."
	self = another ifTrue: [ ^false ].

	^(self < another) not.
]

{ #category : 'comparing' }
CharacterSequence >> >= another [
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."
	"Reduce all cases to #< and #= to simplify code."

	^(self < another) not.
]

{ #category : 'arithmetic' }
CharacterSequence >> \\ arg [

	self shouldNotImplement.
]

{ #category : 'converting' }
CharacterSequence >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with a number, convert me to a number."

	self shouldNotImplement.
]

{ #category : 'converting' }
CharacterSequence >> adaptToNumber: rcvr andSend: selector [
	"If I am involved in arithmetic with a number, convert me to a number."

	self shouldNotImplement.
]

{ #category : 'converting' }
CharacterSequence >> adaptToPoint: rcvr andSend: selector [
	"If I am involved in arithmetic with a point, convert me to a number."

	self shouldNotImplement.
]

{ #category : 'adding' }
CharacterSequence >> add: newObject [
	self shouldNotImplement
]

{ #category : 'comparing' }
CharacterSequence >> alike: aString [
	"Answer some indication of how alike the receiver is to the argument,  0 is no match, twice aString size is best score.  Case is ignored."

	| i j k minSize bonus |
	minSize := (j := self size) min: (k := aString size).
	bonus := (j - k) abs < 2 ifTrue: [ 1 ] ifFalse: [ 0 ].

	i := 1.
	[(i <= minSize) and: [(self at: i) asUppercase = (aString at: i) asUppercase]]
		whileTrue: [ i := i + 1 ].

	[(j > 0) and: [(k > 0) and:
		[(self at: j) asUppercase = (aString at: k) asUppercase]]]
			whileTrue: [ j := j - 1.  k := k - 1. ].

	^ i - 1 + self size - j + bonus.
]

{ #category : 'copying' }
CharacterSequence >> append: aStringOrText [

	^ aStringOrText appendToString: self
]

{ #category : 'copying' }
CharacterSequence >> appendToText: aText [

	^self asText appendToText: aText
]

{ #category : 'grammar' }
CharacterSequence >> article [
	| article first letter second |
	self isEmpty ifTrue: [^self].
	article := self first isVowel ifTrue: ['an'] ifFalse: ['a'].
	first := self first asLowercase.
	letter := self size = 1.
	second := letter ifFalse: [self second asLowercase].
	(first == $f and: [letter orNot: ['aeiloru' includes: second]])
		ifTrue: [^'an'].
	first == $u ifTrue: [
		(letter or: ['cks' includes: second]) ifTrue: [^'a'].
		second == $n
			ifTrue: [(self size = 2 or: [self third isVowel]) ifTrue: [^'a']]].
	(first == $e and: [second == $u]) ifTrue: [^'a'].
	^article
]

{ #category : 'converting' }
CharacterSequence >> asByteString [
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8Bytes: self asUtf8Bytes
]

{ #category : 'converting' }
CharacterSequence >> asCamelCase [
	"Answer a new String, without any whitespace, and with words capitalized (Except for the first one)
	' how do you do? ' asCamelCase
	"
	^ self species streamContents: [ :outStream | | capitalize wroteSome |
		wroteSome := false.
		capitalize := false.
		self do: [ :each | | c |
			c := each.
			c isSeparator
				ifTrue: [ capitalize := true ]
				ifFalse: [
					capitalize & wroteSome ifTrue: [ c := c asUppercase ].
					outStream nextPut: c.
					wroteSome := true.
					capitalize := false ]]].
]

{ #category : 'converting' }
CharacterSequence >> asDate [
	"Many allowed forms, see Date>>#readFrom:
		'2014/6/30' asDate.
		'70/12/30' asDate.
		'12/30/70' asDate.
		'30/12/70' asDate.
		'4/5/6' asDate.
		'15 April 1982' asDate.
	"

	^ Date fromString: self
]

{ #category : 'fileman-converting' }
CharacterSequence >> asDirectoryEntry [
	"Note: It is generally advisable to call one of the more specific messages:
		#/
		#withAbsolutePathName: 
		#withRelativePathName:to:"
	"See examples in #asFileEntry method comment"

	^ DirectoryEntry withPathName: self
]

{ #category : 'fileman-converting' }
CharacterSequence >> asDriveName [
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)
	
Windows
	'C:\' asDriveName 'C:'
	'NotAChance' asDriveName nil
	
Linux
	'/media/cdrom' asDriveName nil

macOS
    '/SanDisk32-NTFS' asDriveName nil
	
	"
	FileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].
	
	^ nil
]

{ #category : 'fileman-converting' }
CharacterSequence >> asFileEntry [
	"Note: It is generally advisable to call one of the more specific messages:
		#//
		#withAbsolutePathName: 
		#withRelativePathName:to:"
	"
	Windows	
		'C:\Windows' asFileEntry exists false
		'C:\Windows' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists false
		'C:\' asFileEntry exists false
		'C:\' asDirectoryEntry exists true
		('C:' asDirectoryEntry // 'Windows') exists false
		('C:' asDirectoryEntry / 'Windows') exists true
	Linux
		'/var' asFileEntry exists
		'/var' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists true
		'/media/cdrom' asFileEntry exists false
		'/media/cdrom' asDirectoryEntry exists true
		('/bin' asDirectoryEntry / 'more') exists false
		('/bin' asDirectoryEntry // 'more') exists true
	macOS
		'/var' asFileEntry exists false
		'/var' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists  true
		'/Volumes/SanDisk32-NTFS' asFileEntry exists false
		'/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
		'SanDisk32-NTFS' asFileEntry exists false
		'SanDisk32-NTFS' asDirectoryEntry exists false
	"

	self isRelativeMark ifTrue: [ ^self error: 'Maybe you need to call #asDirectoryEntry!' ].
	^ FileEntry withPathName: self
]

{ #category : 'converting' }
CharacterSequence >> asFileName [
	"Answer a String made up from the receiver that is an acceptable file name."

	^ self collect: [ :char |
		char isValidInFilenames
			ifFalse: [$#] 
			ifTrue: [char]].
]

{ #category : 'fileman-converting' }
CharacterSequence >> asFullDirectoryEntry [
	"Only for fully qualified path names"

	"See examples in #asFullFileEntry method comment"
	self isAbsolutePathName ifFalse: [ self error: 'Only for fully qualified (absolute) path names' ].
	^ DirectoryEntry withAbsolutePathName: self.
]

{ #category : 'fileman-converting' }
CharacterSequence >> asFullFileEntry [
	"Only for fully qualified path names"
	"
Windows	
	'C:\Windows' asFileEntry exists false
	'C:\Windows' asDirectoryEntry exists true
	'/' asFileEntry exists false
	'/' asDirectoryEntry exists false
	'C:\' asFileEntry exists false
	'C:\' asDirectoryEntry exists true
	('C:' asDirectoryEntry // 'Windows') exists false
	('C:' asDirectoryEntry / 'Windows') exists true
	
Linux
    '/var' asFileEntry exists
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists true
    '/media/cdrom' asFileEntry exists false
    '/media/cdrom' asDirectoryEntry exists true
    ('/bin' asDirectoryEntry / 'more') exists false
    ('/bin' asDirectoryEntry // 'more') exists true

macOS
    '/var' asFileEntry exists false
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists  true
    '/Volumes/SanDisk32-NTFS' asFileEntry exists false
    '/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
    'SanDisk32-NTFS' asFileEntry exists false
    'SanDisk32-NTFS' asDirectoryEntry exists false
	
	"
	self isAbsolutePathName ifFalse: [ self error: 'Only for fully qualified (absolute) path names' ].
	^ FileEntry withAbsolutePathName: self.
]

{ #category : 'converting' }
CharacterSequence >> asIdentifier: shouldBeCapitalized [
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString |
	aString := self select: [ :el | el isValidInIdentifiers ].
	(aString isEmpty or: [aString first isValidStartOfIdentifiers not])
		ifTrue:	 [aString := 'a', aString].
	^ shouldBeCapitalized ifTrue: [ aString capitalized ] ifFalse: [ aString uncapitalized ]

"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false
"
]

{ #category : 'converting' }
CharacterSequence >> asLowercase [
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self collect: [ :each | each asLowercase ].
]

{ #category : 'converting' }
CharacterSequence >> asMonth [
	"Many allowed forms, see Month>>#readFrom:
		'July 1998' asMonth.
		'1998/7' asMonth.
	"

	^ Month fromString: self
]

{ #category : 'Unicode normalization' }
CharacterSequence >> asNFC [
	^self
]

{ #category : 'Unicode normalization' }
CharacterSequence >> asNFD [
	^self
]

{ #category : 'converting' }
CharacterSequence >> asNumber  [
	"Answer the Number created by interpreting the receiver as the string 
	representation of a number."
	^Number readFrom: self readStream
]

{ #category : 'fileman-converting' }
CharacterSequence >> asPathComponents [

	| tokens |
	self isRelativePathName ifTrue: [self error: 'relative form is invaild!'].
	tokens := self asPathTokens.
	^tokens
]

{ #category : 'fileman-converting' }
CharacterSequence >> asPathTokens [

	^self beginsWithWindowsDriveName
		ifTrue: [
			(OrderedCollection with: (self copyFrom: 1 to: 2)), 
				((self copyFrom: 3 to: self size) findTokens: CharacterSequence pathSeparators) ]
		ifFalse: [
			self findTokens: CharacterSequence pathSeparators ]
]

{ #category : 'converting' }
CharacterSequence >> asPlainString [
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self
]

{ #category : 'grammar' }
CharacterSequence >> asPlural [
	| k trimmed plural n |
	k := self findFirst: [:ch | ch isSeparator not].
	k > 1
		ifTrue: [^(self copyFrom: 1 to: k - 1) , (self allButFirst: k - 1) asPlural].
	trimmed := self withBlanksTrimmed.
	trimmed isEmpty ifTrue: [^''].
	plural := trimmed asLowercase lowercasePlural.
	n := trimmed size min: plural size.
	1 to: n do: [:i |
		(trimmed at: i) isUppercase
			ifTrue: [plural at: i put: (plural at: i) asUppercase]].
	^plural
]

{ #category : 'converting' }
CharacterSequence >> asSmalltalkComment [
	"return this string, munged so that it can be treated as a comment in Smalltalk code.  Quote marks are added to the beginning and end of the string, and whenever a solitary quote mark appears within the string, it is doubled"

	^self species streamContents:  [ :str |
		| quoteCount first |

		str nextPut: $".
	
		quoteCount := 0.
		first := true.
		self do: [ :char |
			char == $"
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
	
]

{ #category : 'converting' }
CharacterSequence >> asString [
	"Answer the receiver itself."
	^self
]

{ #category : 'converting' }
CharacterSequence >> asSymbol [
	"Answer the unique symbol whose characters are the characters of the string."
	^Symbol intern: self
]

{ #category : 'converting' }
CharacterSequence >> asText [
	"Answer a Text whose string is the receiver."
	^Text fromString: self
]

{ #category : 'converting' }
CharacterSequence >> asUnHtml [
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in |
	in := self readStream.
	^self species streamContents: [ :out |
		[ in atEnd ] whileFalse: [
			in peek == $<
				ifTrue: [in unCommand] 	"Absorb <...><...>"
				ifFalse: [
					| char |
					(char := in next) == $&
					ifTrue: [
						| rest did |
						rest := in upTo: $;.
						did := out position.
						rest = 'lt' ifTrue: [out nextPut: $<].
						rest = 'gt' ifTrue: [out nextPut: $>].
						rest = 'amp' ifTrue: [out nextPut: $&].
						rest = 'deg' ifTrue: [out nextPut: $¡].
						rest = 'quot' ifTrue: [out nextPut: $"].
						rest first == $# ifTrue: [ out nextPut: (Character codePoint: rest findPositiveInteger) ].
						did = out position ifTrue: [
							out nextPut: $&; nextPutAll: rest.
							"self error: 'unknown encoded HTML char'."
							"Please add it to this method"]]
					ifFalse: [out nextPut: char]].
			]].
]

{ #category : 'converting' }
CharacterSequence >> asUnaccented [
	"
	'Hello' asUnaccented
	'áéîôóü' asUnaccented
	"
	^(self anySatisfy: [ :c | c isAccented])
		ifFalse: [ self ]
		ifTrue: [ self collect: [ :c | c asUnaccented ]]
]

{ #category : 'converting' }
CharacterSequence >> asUnicodeString [
	"Answer an instance of the UTF-8 encoded String class"

	| utf8Bytes |
	utf8Bytes := self asUtf8Bytes.
	^UnicodeString fromUtf8Bytes: utf8Bytes codePointCount: self size.
]

{ #category : 'converting' }
CharacterSequence >> asUnicodeStringOrText [
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	^self asUnicodeString
]

{ #category : 'converting' }
CharacterSequence >> asUppercase [
	"Answer a String made up from the receiver whose characters are all 
	uppercase."

	^ self collect: [ :each | each asUppercase ].
]

{ #category : 'converting' }
CharacterSequence >> asUtf8Bytes [
	^self subclassResponsibility
]

{ #category : 'converting' }
CharacterSequence >> asUtf8BytesOrByteString [
	"Similar to #asUtf8Bytes.
	May answer an instance of String as an optimization.
	See senders"
	^self asUtf8Bytes
]

{ #category : 'converting' }
CharacterSequence >> asUtf8BytesOrByteStringOrByteArray [
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self subclassResponsibility
]

{ #category : 'converting' }
CharacterSequence >> asValidInstanceVariableName [
	| answer |
	answer := self asIdentifier: false.
	(Scanner pseudoVariableNames includes: answer)
		ifTrue: [ answer := answer , 'x' ].
	^ answer

"
'234znak 43 ) 2' asValidInstanceVariableName
'234 xx\ Uml /ler42342380-4' asValidInstanceVariableName
'super' asValidInstanceVariableName
"
]

{ #category : 'converting' }
CharacterSequence >> asValidSelector [
	^ self asIdentifier: false

"
'234znak 43 ) 2' asValidSelector
"
]

{ #category : 'converting' }
CharacterSequence >> asWeek [
	"
		'2008-W52' asWeek.
		'2008-W53' asWeek. 'Invalid format!'.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
		'2008-W52' asWeek start.
		'2009-W01' asWeek start
		'2009-W02' asWeek start
		'2009-W53' asWeek start
		'2010-W01' asWeek start
		'2010-W02' asWeek start
	"

	^ Week fromString: self
]

{ #category : 'converting' }
CharacterSequence >> asYear [
	"
		'2008' asYear.
		'2008' asYear start.
	"

	^ Year fromString: self
]

{ #category : 'converting' }
CharacterSequence >> base64Decoded [
	"Decode the receiver from base 64"
	"
	'SGVsbG8gV29ybGQ=' base64Decoded
	'SGVsbG8gV29ybGQ=' base64Decoded asString
	"
	^(Base64MimeConverter mimeDecode: self)
]

{ #category : 'comparing' }
CharacterSequence >> beginsWith: prefix [
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."

	^self is: prefix substringAt: 1
]

{ #category : 'fileman-testing' }
CharacterSequence >> beginsWithPathSeparator [
	self ifEmpty: [^false].
	^self first isPathSeparator
]

{ #category : 'fileman-testing' }
CharacterSequence >> beginsWithWindowsDriveName [
	"Answer doesn't depend on running on Windows or not"
	self size < 2 ifTrue: [ ^false ].
	^self first isDriveLetter
		and: [ self second isDriveSeparator
			and: [ self size = 2 or: [ self third isPathSeparator ]]]
]

{ #category : 'text conversion color' }
CharacterSequence >> black [
	"Stuff like
	'Hello world' black edit
	"
	^self asText black
]

{ #category : 'text conversion color' }
CharacterSequence >> blue [
	"Stuff like
	'Hello world' blue edit
	"
	^self asText blue
]

{ #category : 'text conversion emphasis' }
CharacterSequence >> bold [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	^self asText bold
]

{ #category : 'Unicode normalization' }
CharacterSequence >> canonicalComposition [
	^self
]

{ #category : 'Unicode normalization' }
CharacterSequence >> canonicalDecomposition [
	^self
]

{ #category : 'converting' }
CharacterSequence >> capitalized [
	"Answer an object like the receiver but with first character downshifted if necesary
	'mElViN' capitalized
	'MElViN' capitalized
	('will', Random next mantissaPart printString) asSymbol capitalized
	('Will', Random next mantissaPart printString) asSymbol capitalized
	"
	self isEmpty ifTrue: [^ self].
	self first isUppercase ifTrue: [ ^self ].
	^self species streamContents: [ :strm | | isFirst corrected |
		isFirst := true.
		self do: [ :codePoint |
			isFirst
				ifTrue: [
					corrected := codePoint asUppercase.
					isFirst := false ]
				ifFalse: [corrected := codePoint].
			strm nextPut: corrected ]]
]

{ #category : 'text conversion attributes' }
CharacterSequence >> centered [
	"Stuff like
	('Hello world' centered ) edit
	"
	^self asText centered
]

{ #category : 'enumerating' }
CharacterSequence >> collect: aBlock [
	"Refer to the comment in Collection >> #collect:."
	| array |
	array := self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacter ]) ifFalse: [
		^array ].
	^self species newFrom: array
]

{ #category : 'enumerating' }
CharacterSequence >> collectInArray: aBlock [
	"Refer to the comment in Collection >> #collect:."
	| result i |
	result := Array new: self size.
	i := 1.
	self do: [ :codePoint |
		result at: i put: (aBlock value: codePoint).
		i := i + 1 ].
	^ result.
]

{ #category : 'text conversion color' }
CharacterSequence >> color: aColor [
	"Stuff like
	'Hello world' color: (Color fromHexString: '#941100') :: edit
	'Hello world' color: (Color fromHexString: '#5A9F2D') :: edit
	"
	^self asText color: aColor
]

{ #category : 'comparing' }
CharacterSequence >> commonPartWith: aString startAt: startIndex stopAt: stopIndexRequested applying: aBlock [
	"Return the size of the longest common subsequence with aString, only between startIndex and stopIndex.
	Apply aBlock to each character before comparing.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."
	| stopIndex |
	stopIndex := stopIndexRequested min: aString size.
	startIndex
		to: stopIndex
		do: [ :i |
			(aBlock value: (self at: i)) = (aBlock value: (aString at: i)) ifFalse: [ ^ i - 1 ]].
	^ stopIndex
]

{ #category : 'comparing' }
CharacterSequence >> commonPrefixWith: aString [
	"Return the size of the longest common prefix with aString.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly.
	'this is a string' commonPrefixWith: 'this is another'
	'this is a string' commonPrefixWith: ''
	'' commonPrefixWith: 'this is another'
	't' commonPrefixWith: 'this is another'
	'txxxxxxxxx' commonPrefixWith: 'this is another'
	"
	^ self commonPartWith: aString startAt: 1 stopAt: self size applying: [ :c | c ]
]

{ #category : 'converting' }
CharacterSequence >> contractTo: smallSize [
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize := smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	'A clear but rather long-winded summary' contractTo: 18
"
]

{ #category : 'copying' }
CharacterSequence >> copyReplaceTokens: oldSubstring with: newSubstring [
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"
	'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'
	"
]

{ #category : 'copying' }
CharacterSequence >> copyReplacing: rangesToNewStrings [

	^self species streamContents: [ :replacementStream |
		self copyReplacing: rangesToNewStrings into: replacementStream ].
]

{ #category : 'copying' }
CharacterSequence >> copyReplacing: rangesToNewStrings into: replacementStream [

	| lastFrom |

	lastFrom := rangesToNewStrings inject: 1 into: [ :from :aRangeToNewString |
		self
			newFromAfterAppending: aRangeToNewString value
			into: replacementStream
			keepingFrom: from
			skipping: aRangeToNewString key ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).

]

{ #category : 'copying' }
CharacterSequence >> copyReplacing: ranges with: newString [

	"Ranges must be in order, with first ranges first. If not, result is unexpected - Hernan"

	^ self species streamContents: [ :replacementStream |
		self copyReplacing: ranges with: newString into: replacementStream ]
		
]

{ #category : 'copying' }
CharacterSequence >> copyReplacing: ranges with: newString into: replacementStream [

	| lastFrom |

	lastFrom := ranges
		inject: 1
		into: [ :from :aRange |
			self newFromAfterAppending: newString into: replacementStream keepingFrom: from skipping: aRange ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).
]

{ #category : 'copying' }
CharacterSequence >> copyReplacing: ranges withAll: newStrings [

	"Ranges must be in order, with first ranges first. If not, result is unexpected
	newStrings must follow ranges order
	ranges and newString must have same size - Hernan"

	^ self species streamContents: [ :replacementStream |
		self copyReplacing: ranges withAll: newStrings into: replacementStream ]
		
]

{ #category : 'copying' }
CharacterSequence >> copyReplacing: ranges withAll: newStrings into: replacementStream [

	| from |

	from := 1.
	ranges 
		with: newStrings 
		do: [ :aRange :newString |
			from := self 
				newFromAfterAppending: newString 
				into: replacementStream 
				keepingFrom: from 
				skipping: aRange ].

	replacementStream nextPutAll: (self copyFrom: from to: self size).
]

{ #category : 'converting' }
CharacterSequence >> correctAgainst: wordList [
	"Correct the receiver: assume it is a misspelled word and return the (maximum of five) nearest words in the wordList.  Depends on the scoring scheme of alike:"
	| results |
	results := self correctAgainst: wordList continuedFrom: nil.
	results := self correctAgainst: nil continuedFrom: results.
	^ results
]

{ #category : 'converting' }
CharacterSequence >> correctAgainst: wordList continuedFrom: oldCollection [
	"Like correctAgainst:.  Use when you want to correct against several lists, give nil as the first oldCollection, and nil as the last wordList."

	^ wordList
		ifNil: [
			self correctAgainstEnumerator: nil continuedFrom: oldCollection ]
		ifNotNil: [
			self
				correctAgainstEnumerator: [ :action | wordList do: action without: nil]
				continuedFrom: oldCollection ]
]

{ #category : 'converting' }
CharacterSequence >> correctAgainstDictionary: wordDict continuedFrom: oldCollection [
	"Like correctAgainst:continuedFrom:.  Use when you want to correct against a dictionary."

	^ wordDict
		ifNil: [
			self correctAgainstEnumerator: nil continuedFrom: oldCollection ]
		ifNotNil: [
			self
				correctAgainstEnumerator: [ :action | wordDict keysDo: action ]
				continuedFrom: oldCollection ]
]

{ #category : 'private' }
CharacterSequence >> correctAgainstEnumerator: wordBlock continuedFrom: oldCollection [
	"The guts of correction, instead of a wordList, there is a block that should take another block and enumerate over some list with it."

	| choices scoreMin results maxChoices |
	scoreMin := self size // 2 min: 3.
	maxChoices := 10.
	choices := oldCollection
		ifNil: [ SortedCollection sortBlock: [ :x :y | x value > y value ] ].
	wordBlock
		ifNil: [
			results := OrderedCollection new.
			1 to: (maxChoices min: choices size) do: [ :i | results add: (choices at: i) key ] ]
		ifNotNil: [
			wordBlock value: [ :word |
				| score |
				(score := self alike: word) >= scoreMin ifTrue: [
					choices add: (Association key: word value: score).
						(choices size >= maxChoices) ifTrue: [ scoreMin := (choices at: maxChoices) value] ] ].
			results := choices ].
	^ results
]

{ #category : 'text conversion color' }
CharacterSequence >> cyan [
	"Stuff like
	'Hello world' cyan edit
	"
	^self asText cyan
]

{ #category : 'displaying' }
CharacterSequence >> displayAt: aPoint  [
	"Display the receiver as a DisplayText at aPoint on the display screen."

	self displayOn: Display at: aPoint.
	Display forceToScreen.
]

{ #category : 'displaying' }
CharacterSequence >> displayOn: aDisplayMedium [
	"Display the receiver on the given DisplayMedium.  5/16/96 sw"

	self displayOn: aDisplayMedium at: `0 @ 0`
]

{ #category : 'displaying' }
CharacterSequence >> displayOn: aDisplayMedium at: aPoint  [
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	^self displayOn: aDisplayMedium at: aPoint textColor: `Color black`
	"
	'Display' displayOn: Display at: 10@10. Display forceToScreen.
	"
]

{ #category : 'displaying' }
CharacterSequence >> displayOn: aDisplayMedium at: aPoint textColor: aColor [
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"

	^aDisplayMedium getCanvas drawString: self at: aPoint font: nil color: aColor
]

{ #category : 'displaying' }
CharacterSequence >> displayProgressAt: aPoint from: minVal to: maxVal during: workBlock [
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			barBlock value: x.]].
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock
]

{ #category : 'converting' }
CharacterSequence >> displayStringOrText [
	"To be used in the UI"
	"Answer this string."

	^ self

]

{ #category : 'user interface' }
CharacterSequence >> edit [

	self editLabel: self class name, ' Text Editor'
]

{ #category : 'user interface' }
CharacterSequence >> editLabel: labelString [

	(TextModel withText: self) openLabel: labelString
]

{ #category : 'paragraph support' }
CharacterSequence >> endOfParagraphBefore: aNumber [
	"Return the index of the last Character newLineCharacter before position aNumber, or zero if this is the first paragraph.
	'ddd' endOfParagraphBefore: 3
	'dd
	d' endOfParagraphBefore: 4
	"
	^ self lastIndexOf: Character newLineCharacter startingAt: aNumber - 1 ifAbsent: [ 0 ]
]

{ #category : 'comparing' }
CharacterSequence >> endsWith: suffix [
	"Answer whether the tail end of the receiver is the same as suffix.
	The comparison is case-sensitive."

	^self is: suffix substringAt: self size - suffix size + 1
"
  'Elvis' endsWith: 'vis'
"
]

{ #category : 'private' }
CharacterSequence >> evaluateExpression: aString parameters: aCollection  [
	"private - evaluate the expression aString with  
	aCollection as the parameters and answer the  
	evaluation result as an string"

	| index |
	index := ('0' , aString) asNumber.
	index isZero
		ifTrue: [^ '[invalid subscript: {1}]' format: {aString}].
	index > aCollection size
		ifTrue: [^ '[subscript is out of bounds: {1}]' format: {aString}].
	^ (aCollection at: index) asString
]

{ #category : 'grammar' }
CharacterSequence >> exceptionalPlural [
	| singular plural index |
	singular := #(
		'addendum' 'aircraft' 'alga' 'alumna' 'alumnus' 'amoeba' 'antenna' 'appendix' 'automaton'
		'bacillus' 'bacterium' 'barracks'
		'cactus' 'calculus' 'child' 'cicatrix' 'colossus' 'corpus' 'corrigendum' 'criterion' 'curriculum'
		'datum' 'deer' 'desideratum' 'dwarf'
		'echo' 'embargo' 'ephemeris' 'erratum' 'extremum'
		'fish' 'focus' 'foot' 'forum' 'fungus'
		'gallows' 'genus' 'goose'
		'hero'
		'index' 'infimum' 'is'
		'lacuna' 'larva' 'louse'
		'matrix' 'maximum' 'means' 'memorandum' 'minimum' 'mythos' 'money' 'mouse'
		'nucleus'
		'offspring' 'optimum' 'opus' 'ox'
		'person' 'phenomenon' 'phylum' 'potato' 'proof'
		'quantum'
		'roof'
		'series' 'sheep' 'species' 'spoof' 'stimulus' 'stratum' 'syllabus'
		'tomato' 'tooth' 'torpedo' 'trilby'
		'vertebra' 'vertex' 'veto'
		'was').
	plural := #(
		'addenda' 'aircraft' 'algae' 'alumnae' 'alumni' 'amoebae' 'antennae' 'appendices' 'automata'
		'bacilli' 'bacteria' 'barracks'
		'cacti' 'calculi' 'children' 'cicatrices' 'colossi' 'corpora' 'corrigenda' 'criteria' 'curricula'
		'data' 'deer' 'desiderata' 'dwarfs'
		'echoes' 'embargoes' 'ephemerides' 'errata' 'extrema'
		'fish' 'foci' 'feet' 'fora' 'fungi'
		'gallows' 'genera' 'geese'
		'heroes'
		'indices' 'infima' 'are'
		'lacunae' 'larvae' 'lice'
		'matrices' 'maxima' 'means' 'memoranda' 'minima' 'mythoi' 'moneys' 'mice'
		'nuclei'
		'offspring' 'optima' 'opera' 'oxen'
		'people' 'phenomena' 'phyla' 'potatoes' 'proofs'
		'quanta'
		'roofs'
		'series' 'sheep' 'species' 'spoofs' 'stimuli' 'strata' 'syllabi'
		'tomatoes' 'teeth' 'torpedoes' 'trilbys'
		'vertebrae' 'vertices' 'vetoes'
		'were').
	index := singular indexOf: self.
	^index > 0 ifTrue: [plural at: index]
]

{ #category : 'fileman-actions' }
CharacterSequence >> fileContents [
	^self asFileEntry fileContents
]

{ #category : 'fileman-actions' }
CharacterSequence >> fileContents: aString [
	self asFileEntry fileContents: aString.
	^aString
]

{ #category : 'accessing' }
CharacterSequence >> findAnySubStr: delimiters startingAt: start [
	"Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters.  delimiters is an Array of Strings (Characters are permitted also).  If the receiver does not contain any of the delimiters, answer size + 1."

	| min |
	min := self size + 1.
	delimiters do: [:delim |	"May be a char, a string of length 1, or a substring"
		| ind |
		ind := delim isCharacter
			ifTrue: [self indexOfSubCollection: (String with: delim) 
						startingAt: start ifAbsent: [min]]
			ifFalse: [self indexOfSubCollection: delim 
						startingAt: start ifAbsent: [min]].
		min := min min: ind].
	^ min
]

{ #category : 'accessing' }
CharacterSequence >> findBetweenSubStrs: delimiters [
	"Answer the collection of String tokens that result from parsing self.  Tokens are separated by 'delimiters', which can be a collection of Strings, or a collection of Characters.  Several delimiters in a row are considered as just one separation."

	| tokens keyStart keyStop |
	tokens := OrderedCollection new.
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipAnySubStr: delimiters startingAt: keyStop.
		keyStop := self findAnySubStr: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens
]

{ #category : 'accessing' }
CharacterSequence >> findDelimiters: delimiters startingAt: start [
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | delim = (self at: i) ifTrue: [^ i]]].
	^ self size + 1
]

{ #category : 'accessing' }
CharacterSequence >> findLastOccurrenceOfString: subString startingAt: start  [
	"Answer the index of the last occurrence of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0.  Case-sensitive match used."

	| last now |
	last := self findString: subString startingAt: start.
	last = 0 ifTrue: [^ 0].
	[last > 0] whileTrue:
		[now := last.
		last := self findString: subString startingAt: last + 1].

	^ now

]

{ #category : 'converting' }
CharacterSequence >> findPositiveInteger  [
	"Answer the Integer created by interpreting the receiver as the string representation of an integer.
	Answer nil if no digits, else find the first digit and then all consecutive digits after that"

	| startPosition tail endPosition |
	startPosition := self findFirst: [:ch | ch isDigit].
	startPosition = 0 ifTrue: [^ nil].
	tail := self copyFrom: startPosition to: self size.
	endPosition := tail findFirst: [:ch | ch isDigit not].
	endPosition = 0 ifTrue: [endPosition := tail size + 1].
	^ Number readFrom: (tail copyFrom: 1 to: endPosition - 1) readStream

"
'1796exportFixes-tkMX' findPositiveInteger
'1848recentLogFile-sw' findPositiveInteger
'donald' findPositiveInteger
'abc234def567' findPositiveInteger
"
]

{ #category : 'converting' }
CharacterSequence >> findSelector [
	"Revised to use scanner for better removal of extraneous stuff"
	| potentialSelector colonIndex |
	potentialSelector := self withBlanksTrimmed withoutEnclosing: $".
	colonIndex := potentialSelector indexOf: $:.
	"possible keyword selector"
	(colonIndex > 1 and: [ (potentialSelector at: colonIndex - 1) isValidInIdentifiers ]) ifTrue: [
		potentialSelector := [ Scanner findSelectorIn: potentialSelector ] on: Error do: [ :anError | 
			anError messageText = Scanner unmatechedCommentQuoteErrorDescription 
				ifTrue: [ 
					potentialSelector := potentialSelector copyWithout: $".
					anError retry ]
				ifFalse: [ anError return: '']]].
		
	potentialSelector isEmpty ifTrue: [ ^ nil ].
	Symbol
		hasInterned: potentialSelector
		ifTrue: [ :aSymbol | ^ aSymbol ].
		
	^ nil.
]

{ #category : 'accessing' }
CharacterSequence >> findString: subString [
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."
	^self findString: subString startingAt: 1.
]

{ #category : 'accessing' }
CharacterSequence >> findString: subString startingAt: start caseSensitive: caseSensitive [
	
	^caseSensitive
		ifTrue: [ self findString: subString startingAt: start ]
		ifFalse: [ self findStringCaseInsensitive: subString startingAt: start ]
]

{ #category : 'accessing' }
CharacterSequence >> findTokens: delimiters [
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens := OrderedCollection new.
	separators := delimiters isCharacter
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipDelimiters: separators startingAt: keyStop.
		keyStop := self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens
]

{ #category : 'accessing' }
CharacterSequence >> findTokens: delimiters keep: keepers [
	"Answer the collection of tokens that result from parsing self.  The tokens are seperated by delimiters, any of a string of characters.  If a delimiter is also in keepers, make a token for it.  (Very useful for carriage return.  A sole return ends a line, but is also saved as a token so you can see where the line breaks were.)"

	| tokens keyStart keyStop |
	tokens := OrderedCollection new.
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipDelimiters: delimiters startingAt: keyStop.
		keyStop to: keyStart-1 do: [:ii | 
			(keepers includes: (self at: ii)) ifTrue: [
				tokens add: (self copyFrom: ii to: ii)]].	"Make this keeper be a token"
		keyStop := self findDelimiters: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens
]

{ #category : 'converting' }
CharacterSequence >> firstNonSeparator [

	^self findFirst: [:aChar | aChar isSeparator not ]
]

{ #category : 'converting' }
CharacterSequence >> fixLineEndings [
	"Assume that CR, LF, and CRLF are all valid line endings. Replace each occurence with a single Lf.
	If possible, modify and answer the same instance. Create a new instance only if needed.
	Be quick and don't use extra memory if already using Character lf!
	('Hello ', String crlfString, 'World') fixLineEndings
	('Hello ', String crString, 'World') fixLineEndings
	('Hello ', String lfString, 'World') fixLineEndings
	"
	(self includes: `Character cr`) ifFalse: [ ^self ].
	^ self withLineEndings: `String newLineString`
]

{ #category : 'copying' }
CharacterSequence >> flattenTo: flattenedStream [

	flattenedStream nextPut: self
]

{ #category : 'formatting' }
CharacterSequence >> format: aCollection [
	"format the receiver with aCollection  
	 
	simplest example:
	'foo {1} bar' format: {Date today}.
	 
	complete example:
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}..
	"
	^self species streamContents: [ :out |
		| stream |
		stream := self readStream.
		[stream atEnd]
			whileFalse: [| currentChar | 
				currentChar := stream next.
				currentChar == ${
					ifTrue: [| expression | 
						expression := self getEnclosedExpressionFrom: stream.
						out nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
					ifFalse: [
						currentChar == $\
							ifTrue: [stream atEnd ifFalse: [out nextPut: stream next]]
							ifFalse: [out nextPut: currentChar]]]].
]

{ #category : 'private' }
CharacterSequence >> getEnclosedExpressionFrom: aStream [
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"

	| result currentChar |
	result := self species writeStream.
	[aStream atEnd 
		or: [(currentChar := aStream next) == $}]]
			whileFalse: [result nextPut: currentChar].
	^ result contents withBlanksTrimmed.
]

{ #category : 'text conversion color' }
CharacterSequence >> gray [
	"Stuff like
	'Hello world' gray edit
	"
	^self asText gray
]

{ #category : 'text conversion color' }
CharacterSequence >> green [
	"Stuff like
	'Hello world' green edit
	"
	^self asText green
]

{ #category : 'comparing' }
CharacterSequence >> hash [
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	self subclassResponsibility
]

{ #category : 'accessing' }
CharacterSequence >> includesSubString: subString [
	^ (self findString: subString startingAt: 1) > 0
]

{ #category : 'accessing' }
CharacterSequence >> includesSubstring: aString caseSensitive: caseSensitive [
	
	^ (self findString: aString startingAt: 1 caseSensitive: caseSensitive) > 0
]

{ #category : 'paragraph support' }
CharacterSequence >> indentationIfBlank: aBlock [
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator tab ch |
	tab := Character tab.
	reader := self readStream.
	leadingTabs := 0.
	[ reader atEnd not and: [ (ch := reader next) == tab ]] whileTrue: [
		leadingTabs := leadingTabs + 1 ].
	lastSeparator := leadingTabs + 1.
	[ reader atEnd not and: [
		ch isSeparator and: [ ch isLineSeparator not ]]] whileTrue: [
			lastSeparator := lastSeparator + 1.
			ch := reader next ].
	lastSeparator = self size | (ch notNil and: [ch isLineSeparator]) ifTrue: [
		^ aBlock value: leadingTabs ].
	^ leadingTabs
]

{ #category : 'accessing' }
CharacterSequence >> indexOf: aCharacter [

	^self indexOf: aCharacter startingAt: 1
]

{ #category : 'fileman-accessing' }
CharacterSequence >> indexOfFirstPathSeparator [
	self
		withIndexDo: [:each :idx | each isPathSeparator
				ifTrue: [^ idx]].
	^ 0
]

{ #category : 'fileman-accessing' }
CharacterSequence >> indexOfLastPathSeparator [
	"
	'pp.txt' indexOfLastPathSeparator
	'/pp.txt' indexOfLastPathSeparator
	'a/pp.txt' indexOfLastPathSeparator
	'b/a/pp.txt' indexOfLastPathSeparator
	'/b/a/pp.txt' indexOfLastPathSeparator
	"
	^self findLast: [ :char | char isPathSeparator ]
]

{ #category : 'accessing' }
CharacterSequence >> indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock [
	| index |
	subString isString ifFalse: [
		^super indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock ].
	index := self findString: subString startingAt: start.
	index = 0 ifTrue: [ ^exceptionBlock value ].
	^index
]

{ #category : 'converting' }
CharacterSequence >> initialIntegerOrNil [
	"Answer the integer represented by the leading digits of the receiver,
	or nil if the receiver does not begin with a digit"

	^self integerOrNilAt: 1.
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"

]

{ #category : 'converting' }
CharacterSequence >> integerOrNilAt: firstIndex [
	"Answer the integer represented by the leading digits of the receiver,
	or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self isEmpty or: [(self at: firstIndex) isDigit not]) ifTrue: [^ nil].
	firstNonDigit := self findFirst: [:m | m isDigit not] startingAt: firstIndex.
	firstNonDigit = 0 ifTrue: [firstNonDigit := self size + 1].
	^ (self copyFrom: firstIndex  to: (firstNonDigit - 1)) asNumber
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"

]

{ #category : 'comparing' }
CharacterSequence >> is: subString substringAt: index [
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	^self is: subString subcollectionAt: index
]

{ #category : 'fileman-testing' }
CharacterSequence >> isAbsolutePathName [
	"Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt'
	and even '/Users/Someone/file.txt' are considered an absolute pathName.
	This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice."
	
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].
	
	"tilde means user's home folder, if possible.
	https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	self first == $~ ifTrue: [^ true ].

	FileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	^ false
]

{ #category : 'testing' }
CharacterSequence >> isAlphaNumeric [
	"Answer true if the receiver contains only letters or digits."
	^ self allSatisfy: [:each| each isAlphaNumeric]
]

{ #category : 'fileman-testing' }
CharacterSequence >> isDriveName [
	FileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	^false
]

{ #category : 'testing' }
CharacterSequence >> isLineEndAt: anIndex [
	"Answer true if a line ends at position anIndex.
	Tolerates one position behind last character:
	'hello' isLineEndAt: 6
	'hello' isLineEndAt: 5
	'hello
	world' isLineEndAt: 6
	"
	anIndex = (self size + 1) ifTrue: [ ^true ].
	^(self at: anIndex) isLineSeparator
]

{ #category : 'fileman-testing' }
CharacterSequence >> isPathSeparator [
	^self size = 1 and: [ self first isPathSeparator ]
]

{ #category : 'fileman-testing' }
CharacterSequence >> isRelativeMark [

	self = '.' ifTrue: [^true].
	self = '..' ifTrue: [^true].

	^false
]

{ #category : 'fileman-testing' }
CharacterSequence >> isRelativePathName [
	self ifEmpty: [^false].
	self isRelativeMark ifTrue: [^true].
	^#('.' '..') includes: self upToFirstPathSeparator
]

{ #category : 'testing' }
CharacterSequence >> isRemote [
	^false
]

{ #category : 'testing' }
CharacterSequence >> isSeparators [
	"Answer true if the receiver contains only separators (i.e. whitespace)."
	^ self allSatisfy: [:each| each isSeparator]
]

{ #category : 'testing' }
CharacterSequence >> isString [
	^ true
]

{ #category : 'grammar' }
CharacterSequence >> isUninflictedNoun [
	| nouns |
	nouns := #(
		'bison' 'bream' 'breeches' 'britches'
		'carp' 'chassis' 'clippers' 'cod' 'contretemps' 'corps'
		'debris' 'diabetes' 'djinn'
		'eland' 'elk'
		'flounder'
		'gallows' 'graffiti'
		'headquarters' 'herpes' 'high-jinks' 'homework'
		'innings'
		'jackanapes'
		'mackerel' 'measles' 'mews' 'mumps'
		'news'
		'pincers' 'pliers' 'proceedings'
		'rabies'
		'salmon' 'scissors' 'sea-bass' 'series' 'shears' 'species' 'swine'
		'trout' 'tuna'
		'whiting' 'wildebeest').
	^nouns includes: self
]

{ #category : 'text conversion emphasis' }
CharacterSequence >> italic [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	^self asText italic
]

{ #category : 'copying' }
CharacterSequence >> join: aCollection  [
	"
	'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "

	^self species streamContents: [ :strm |
		aCollection
			do: [ :elem | strm nextPutAll: elem asString]
			separatedBy: [strm nextPutAll: self]].
]

{ #category : 'text conversion attributes' }
CharacterSequence >> justified [
	"Stuff like
	('Hello world' justified ) edit
	"
	^self asText justified
]

{ #category : 'converting' }
CharacterSequence >> keywords [
	"Answer an array of the keywords that compose the receiver."
	| keywords |
	keywords := Array streamContents: [ :kwds |
		| kwd isAllValidInIdentifiers |
		kwd := self species writeStream.
		isAllValidInIdentifiers := true.
		1
			to: self size
			do: [ :i |
				| char |
				char := self at: i.
				kwd nextPut: char.
				char == $: & isAllValidInIdentifiers
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllValidInIdentifiers := true ]
					ifFalse: [
						char isValidInIdentifiers ifFalse: [ isAllValidInIdentifiers := false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.
]

{ #category : 'converting' }
CharacterSequence >> lastNonSeparator [

	^ self findLast: [:aChar | aChar isSeparator not].
	
]

{ #category : 'text conversion attributes' }
CharacterSequence >> leftFlush [
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	^self asText leftFlush
]

{ #category : 'accessing' }
CharacterSequence >> lineCount [
	"Answer the number of lines represented by the receiver, where every cr adds one line.  5/10/96 sw"


	| i |
	i := 0.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i := i + 1 ].
	^i

"
'Fred
the
Bear' lineCount
"
]

{ #category : 'accessing' }
CharacterSequence >> lineNumber: anIndex [
	"Answer a string containing the characters in the given line number. "
	| i |
	i := 1.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i = anIndex ifTrue: [
			^ self copyFrom: start to: endWithoutDelimiters ].
		i := i + 1 ].
	^nil
	
"
'Fred
the
Bear' lineNumber: 3
".
]

{ #category : 'accessing' }
CharacterSequence >> lines [
	"Answer an array of lines composing this receiver without the line ending delimiters."

	^Array
		streamContents: [ :lines | self linesDo: [ :aLine | lines nextPut: aLine ]]
		estimatedSize: (self size // 60 max: 16)
]

{ #category : 'accessing' }
CharacterSequence >> linesDo: aBlock [
	"execute aBlock with each line in this string.  The terminating CR's are not included in what is passed to aBlock"
	
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		aBlock value: (self copyFrom: start  to: endWithoutDelimiters) ]
		
]

{ #category : 'grammar' }
CharacterSequence >> lowercasePlural [
	| last |
	self exceptionalPlural ifNotNil: [:pl | ^pl].
	self isUninflictedNoun ifTrue: [^self].
	last := self last.
	last == $y ifTrue: [
		#('ay' 'ey' 'oy' 'uy') do: [:t |
			(self endsWith: t) ifTrue: [^self , 's'].
			^self allButLast , 'ies']].
	#('zz' 'ch' 'sh') do: [:t | (self endsWith: t) ifTrue: [^self , 'es']].
	last == $s ifTrue: [
		self = 'its' ifTrue: [^'their'].
		#('bs' 'cs' 'ds' 'ks' 'ls' 'ms' 'rs' 'ts' 'ws')
			do: [:t | (self endsWith: t) ifTrue: [^self]].
		#('sis' 'xis')
			do: [:t | (self endsWith: t) ifTrue: [^(self allButLast: 2) , 'es']]].
	last == $z ifTrue: [^self , 'zes'].
	(last == $x or: [last == $s]) ifTrue: [^self , 'es'].
	(self endsWith: 'man') ifTrue: [^(self allButLast: 2) , 'en'].
	last == $f ifTrue: [^self allButLast , 'ves'].
	(self endsWith: 'fe') ifTrue: [^(self allButLast: 2) , 'ves'].
	^self , 's'
]

{ #category : 'text conversion color' }
CharacterSequence >> magenta [
	"Stuff like
	'Hello world' magenta edit
	"
	^self asText magenta
]

{ #category : 'comparing' }
CharacterSequence >> match: text [
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."

	^ self startingAt: 1 match: text startingAt: 1
"
	'*'			match: 'zort' true
	'*baz'		match: 'mobaz' true
	'*baz'		match: 'mobazo' false
	'*baz*'		match: 'mobazo' true
	'*baz*'		match: 'mozo' false
	'foo*'		match: 'foozo' true
	'foo*'		match: 'bozo' false
	'foo*baz'	match: 'foo23baz' true
	'foo*baz'	match: 'foobaz' true
	'foo*baz'	match: 'foo23bazo' false
	'foo'		match: 'Foo' true
	'foo*baz*zort' match: 'foobazort' false
	'foo*baz*zort' match: 'foobazzort' false
	'*foo#zort'	match: 'afoo3zortthenfoo3zort' true
	'*foo*zort'	match: 'afoodezortorfoo3zort' true
"
]

{ #category : 'copying' }
CharacterSequence >> newFromAfterAppending: aNewString into: replacementStream keepingFrom: from skipping: aRange [

	replacementStream
		nextPutAll: (self copyFrom: from to: aRange first - 1);
		nextPutAll: aNewString.

	^ aRange last + 1
]

{ #category : 'copying' }
CharacterSequence >> padded: leftRightOrCenter to: aLength with: aChar [

	"
	'hi' padded: #left to: 5 with: $. -->  '...hi' 
	'hi' padded: #right to: 5 with: $. --> 'hi...' 
	'hi' padded: #center to: 5 with: $. --> '.hi..' 
	"
	| toPad |
	
	toPad := aLength - self size max: 0.
	
	leftRightOrCenter = #left ifTrue: [ ^ (self species new: toPad withAll: aChar) , self].
	leftRightOrCenter = #right ifTrue: [ ^ self , (self species new: toPad withAll: aChar)].
	leftRightOrCenter = #center ifTrue: [ | paddedLeft | 
		paddedLeft := self padded: #left to: toPad//2 + self size with: aChar.
		^ paddedLeft padded: #right to: aLength with: aChar].
]

{ #category : 'fileman-accessing' }
CharacterSequence >> pathAndLocalName [
	| index path localName |
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^{''. self}].

	path := self copyFrom: 1 to: index-1.
	localName := self copyFrom: index+1 to: self size.

	^{path. localName}

	
]

{ #category : 'grammar' }
CharacterSequence >> pluralize: aBoolean [
	^aBoolean ifTrue: [self asPlural] ifFalse: [self]
]

{ #category : 'text conversion emphasis' }
CharacterSequence >> pointSize: pointSize [
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	^self asText pointSize: pointSize
]

{ #category : 'converting' }
CharacterSequence >> prefixAndSuffix: aCharacter [
	"Answer an array with the prefix up to the last occurrence of aCharacter, and the suffix after it.
	Answer nil if aCharacter is not in self
	'1164-ReferenceStreamFix-jmv.1.cs' prefixAndSuffix: $-
	"
	| i |
	i := self findLast: [ :c | c = aCharacter ].
	i = 0 ifTrue: [ ^ nil ].
	^ { self copyFrom: 1 to: i-1 . self copyFrom: i+1 to: self size }
]

{ #category : 'printing' }
CharacterSequence >> print [
	Transcript show: self; newLine
]

{ #category : 'printing' }
CharacterSequence >> printAsLiteralOn: aStream [
	"Print inside string quotes, doubling embedded quotes."
	self storeOn: aStream
]

{ #category : 'printing' }
CharacterSequence >> printOn: aStream  [
	"Print inside string quotes, doubling inbedded quotes."

	self storeOn: aStream
]

{ #category : 'text conversion color' }
CharacterSequence >> red [
	"Stuff like
	'Hello world' red edit
	"
	^self asText red
]

{ #category : 'text conversion attributes' }
CharacterSequence >> rightFlush [
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	^self asText rightFlush
]

{ #category : 'converting' }
CharacterSequence >> romanNumber [
	"
	2022 printStringRoman romanNumber
	"
	| value v1 v2 |
	value := v1 := v2 := 0.
	self reverseDo:
		[:each |
		v1 := #(1 5 10 50 100 500 1000) at: ('IVXLCDM' indexOf: each).
		v1 >= v2
			ifTrue: [value := value + v1]
			ifFalse: [value := value - v1].
		v2 := v1].
	^ value
]

{ #category : 'uCompletion' }
CharacterSequence >> separateKeywords [

	^self, ' '
]

{ #category : 'accessing' }
CharacterSequence >> skipAnySubStr: delimiters startingAt: start  [
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."
	| ii |
	ii := start-1.
	[(ii := ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		| any |
		any := false.
		delimiters do: [ :delim |
			delim isCharacter
				ifTrue: [ (self at: ii) = delim ifTrue: [any := true]]
				ifFalse: ["a substring"
					delim size > (self size - ii + 1) ifFalse: [
						| ind this |
						ind := 0.
						this := true.
						delim do: [ :dd | 
							dd = (self at: ii+ind) ifFalse: [this := false].
							ind := ind + 1].
						this ifTrue: [ii := ii + delim size - 1.  any := true]]
							ifTrue: [any := false] "if the delim is too big, it can't match"]].
		any ifFalse: [^ ii]].
	^ self size + 1
]

{ #category : 'accessing' }
CharacterSequence >> skipDelimiters: delimiters startingAt: start [
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1
]

{ #category : 'converting' }
CharacterSequence >> squeezedTo: n [
	"
Examples:
	Do nothing:
		'This one is a rather long phrase' squeezedTo: 32

	1-remove blanks (result can be shorter than asked):
		'This one is a rather long phrase' squeezedTo: 30

	2-remove necessary trailing vowels
		'This one is a rather long phrase' squeezedTo: 24

	3-truncate as needed (and add ellipsis)
		'This one is a rather long phrase' squeezedTo: 15

	4-avoid ellipsis
		'This one is a rather long phrase' squeezedTo: 5
	"
	| vowelCount read write i char allowedVowels str desiredSize postFix j |
	str := self.
	desiredSize := n.
	str size <= n ifTrue: [^str].
	str := str asCamelCase.
	str size <= n ifTrue: [^str].
	postFix := ''.
	desiredSize := n - postFix size.
	vowelCount := str
		inject: 0
		into: [:prev :each | each isVowel ifTrue: [prev + 1] ifFalse: [prev]].
	str size - vowelCount <= desiredSize
		ifTrue: [allowedVowels := vowelCount - (str size - desiredSize)]
		ifFalse: [
			allowedVowels := 0.
			postFix := '...'.
			n - postFix size < 5 ifTrue: [postFix := ''].
			desiredSize := n - postFix size].

	read := str readStream.
	write := self species writeStream.
	i := 0.
	j := 0.
	[read atEnd not and: [j < desiredSize]] whileTrue: [
		char := read next.
		(char isVowel not or: [i < allowedVowels]) ifTrue: [
			char isVowel ifTrue: [i := i + 1].
			write nextPut: char.
			j := j + 1]].
	str := write contents , postFix.
	^ str.
]

{ #category : 'comparing' }
CharacterSequence >> startingAt: keyStart match: text startingAt: textStart [
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."
	| anyMatch matchStart matchEnd i matchStr j ii jj |
	i := keyStart.
	j := textStart.

	"Check for any #'s"
	[i > self size ifTrue: [^ j > text size "Empty key matches only empty string"].
	(self at: i) == $#] whileTrue:
		["# consumes one char of key and one char of text"
		j > text size ifTrue: [^ false "no more text"].
		i := i+1.  j := j+1].

	"Then check for *"
	(self at: i) == $*
		ifTrue: [i = self size ifTrue:
					[^ true "Terminal * matches all"].
				"* means next match string can occur anywhere"
				anyMatch := true.
				matchStart := i + 1]
		ifFalse: ["Otherwise match string must occur immediately"
				anyMatch := false.
				matchStart := i].

	"Now determine the match string"
	matchEnd := self size.
	(ii := self indexOf: $* startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '** not valid -- use * instead'].
		matchEnd := ii-1].
	(ii := self indexOf: $# startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '*# not valid -- use #* instead'].
		matchEnd := matchEnd min: ii-1].
	matchStr := self copyFrom: matchStart to: matchEnd.

	"Now look for the match string"
	[jj := text findString: matchStr startingAt: j caseSensitive: false.
	anyMatch ifTrue: [jj > 0] ifFalse: [jj = j]]
		whileTrue:
		["Found matchStr at jj.  See if the rest matches..."
		(self startingAt: matchEnd+1 match: text startingAt: jj + matchStr size) ifTrue:
			[^ true "the rest matches -- success"].
		"The rest did not match."
		anyMatch ifFalse: [^ false].
		"Preceded by * -- try for a later match"
		j := j+1].
	^ false "Failed to find the match string"
]

{ #category : 'printing' }
CharacterSequence >> storeOn: aStream  [
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x := self at: i).
		x == $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'
]

{ #category : 'accessing' }
CharacterSequence >> string [
	"Answer the receiver itself.  This is for compatibility with other text classes."
	^self
]

{ #category : 'text conversion emphasis' }
CharacterSequence >> struck [
	^self asText struck
]

{ #category : 'text conversion emphasis' }
CharacterSequence >> sub [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	^self asText sub
]

{ #category : 'accessing' }
CharacterSequence >> subStrings: separators  [
	"Answer an array containing the substrings in the receiver separated 
	by any of the elements of separators.
	Answer will not include empty strings
	'Hello World' subStrings: ' '
	'1Hello12World1This1211Are31Words1' subStrings: '123'
	'1Hello12World1This1211Are31Words1' subStrings: #($1 $2 $3)
	"

	| delimiterCharacters |
	delimiterCharacters := separators asArray.
	^Array streamContents: [ :answer |
		| out |
		out := self species writeStream.
		self do: [ :char |
			(delimiterCharacters identityIncludes: char)
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					out isEmpty ifFalse: [
						answer nextPut: out contents.
						out := self species writeStream ]]].
		out isEmpty ifFalse: [
			answer nextPut: out contents ]].
]

{ #category : 'accessing' }
CharacterSequence >> substringsSeparatedBy: delimiterCharacter [
	"Find substrings delimited by aCharacter.
	Answer may include empty strings
	'0063;LATIN SMALL LETTER C;Ll;0;L;;;;;N;;;0043;;0043' substringsSeparatedBy: $;
	"

	^Array streamContents: [ :answer |
		| out |
		out := self species writeStream.
		self do: [ :char |
			char = delimiterCharacter
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					answer nextPut: out contents.
					out := self species writeStream ]].
		answer nextPut: out contents.
		out := nil ].
]

{ #category : 'text conversion emphasis' }
CharacterSequence >> super [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	^self asText super
]

{ #category : 'converting' }
CharacterSequence >> translated [
	"In Squeak, #translated does localization of Strings. This method helps with compatibility.
	Additionally, applications written in Cuis could do the same.
	This method should be redefined in a Localization package."
]

{ #category : 'converting' }
CharacterSequence >> truncateTo: smallSize [
	"return myself or a copy shortened to smallSize.  1/18/96 sw"

	^ self size <= smallSize
		ifTrue:
			[self]
		ifFalse:
			[self copyFrom: 1 to: smallSize]
]

{ #category : 'converting' }
CharacterSequence >> truncateWithElipsisTo: maxLength [
	"Return myself or a copy suitably shortened but with elipsis added"

	^ self size <= maxLength
		ifTrue:
			[self]
		ifFalse:
			[(self copyFrom: 1 to: (maxLength - 3)), '...']


	"'truncateWithElipsisTo:' truncateWithElipsisTo: 20"
]

{ #category : 'converting' }
CharacterSequence >> uncapitalized [
	"Answer an object like the receiver but with first character downshifted if necesary
	'MElViN' uncapitalized
	'mElViN' uncapitalized
	('Will', Random next mantissaPart printString) asSymbol uncapitalized
	('will', Random next mantissaPart printString) asSymbol uncapitalized
	"
	self isEmpty ifTrue: [^ self].
	self first isLowercase ifTrue: [ ^self ].
	^self species streamContents: [ :strm | | isFirst corrected |
		isFirst := true.
		self do: [ :codePoint |
			isFirst
				ifTrue: [
					corrected := codePoint asLowercase.
					isFirst := false ]
				ifFalse: [corrected := codePoint].
			strm nextPut: corrected ]]
]

{ #category : 'text conversion emphasis' }
CharacterSequence >> under [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	^self asText under
]

{ #category : 'fileman-accessing' }
CharacterSequence >> upToFirstPathSeparator [
	| index |
	self ifEmpty: [^self].
	index := self indexOfFirstPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
]

{ #category : 'fileman-accessing' }
CharacterSequence >> upToLastPathSeparator [
	| index |
	self ifEmpty: [^self].
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
	
]

{ #category : 'accessing' }
CharacterSequence >> utf8BytesSize [
	"Optimized where appropriate."
	^self asUtf8Bytes size
]

{ #category : 'grammar' }
CharacterSequence >> withArticle [
	^self article , ' ' , self
]

{ #category : 'converting' }
CharacterSequence >> withBlanksCondensed [
	"Return a copy of the receiver with leading/trailing blanks removed
	 and consecutive white spaces condensed as a single space."

	| trimmed |
	trimmed := self withBlanksTrimmed.
	^self species streamContents: [ :stream |
		| lastWasBlank |
		lastWasBlank := false.
		trimmed do: [ :c |
			c isSeparator
				ifTrue: [ lastWasBlank ifFalse: [ stream space ]]
				ifFalse: [ stream nextPut: c ].
			lastWasBlank := c isSeparator ]].

	"
	' abc  d   ' withBlanksCondensed
	' abc  d
	s
	as   zz 	q 			q' withBlanksCondensed
	"
]

{ #category : 'converting' }
CharacterSequence >> withBlanksTrimmed [
	"Return a copy of the receiver from which leading and trailing blanks have been trimmed."

	| first |
	
	first := self firstNonSeparator.
	first = 0 ifTrue: [^ ''].  "no non-separator character"
	
	^ self copyFrom: first to: self lastNonSeparator 

	" ' abc  d   ' withBlanksTrimmed"

]

{ #category : 'converting' }
CharacterSequence >> withCuisLineEndings [
	"Assume that CR, LF, and CRLF are all valid line endings.
	Answer a new instance, replacing each occurence with a single Lf.
	('aLine', String crlfString, 'anotherOne') withCuisLineEndings
	"
	^ self withLineEndings: `String newLineString`.
]

{ #category : 'converting' }
CharacterSequence >> withDescriptiveLineEndings [
	"
	('Hello', String newLineString, 'World') withDescriptiveLineEndings
	"
	^ self withLineEndings: '[enter]'.
]

{ #category : 'converting' }
CharacterSequence >> withDoubleColonsDoubled [
	"Return a copy of the receiver with any double colons duplicated.
	Useful for turning text into Smalltalk comments."

	^self species streamContents: [ :stream |
		self do: [ :c |
			stream nextPut: c.
			c == $"
				ifTrue: [ stream nextPut: c ]]].
]

{ #category : 'converting' }
CharacterSequence >> withDoubleColonsUndoubled [
	"Do the inverse of #withDoubleColonsDoubled"

	^self species streamContents: [ :stream |
		| lastWasDoubleColon |
		lastWasDoubleColon := false.
		self do: [ :c |
			c == $"
				ifTrue: [
					lastWasDoubleColon
						ifFalse: [
							stream nextPut: c.
							lastWasDoubleColon := true ]
						ifTrue: [
							lastWasDoubleColon := false]]
				ifFalse: [
					stream nextPut: c.
					lastWasDoubleColon := false ]]].
]

{ #category : 'converting' }
CharacterSequence >> withNewLines [
	"Return a copy of the receiver in which backslash (\) characters have been replaced with newLine (i.e. Lf)."

	^ self collect: [ :c | c == $\ ifTrue: [ Character newLineCharacter ] ifFalse: [ c ]].
]

{ #category : 'converting' }
CharacterSequence >> withoutEnclosing: aCharacter [
	"
	'*Hello*' withoutEnclosing: $*
	"
	| s |
	s := self size.
	s = 0 ifTrue: [ ^ self ].
	^((self at: 1) = aCharacter and: [ (self at: s) = aCharacter ])
		ifTrue: [ self copyFrom: 2 to: s-1 ]
		ifFalse: [ self ]
]

{ #category : 'converting' }
CharacterSequence >> withoutLeadingBlanks [
	
	"Return a copy of the receiver from which leading blanks have been trimmed."
	
	| first |
	
	first := self firstNonSeparator.
	first = 0 ifTrue: [^ ''].  
	first = 1 ifTrue: [^ self ].
	
	"no non-separator character"	
	^ self copyFrom: first to: self size
		
	" '    abc  d  ' withoutLeadingBlanks"

]

{ #category : 'converting' }
CharacterSequence >> withoutPossiblePrefix: aString [
	"
	'Elvis' withoutPossiblePrefix: 'El'
	'Elvis' withoutPossiblePrefix: 'Paul'
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: aString size+1 to: self size ]
		ifFalse: [self]
]

{ #category : 'converting' }
CharacterSequence >> withoutPossibleSuffix: aString [
	"
	'Elvis' withoutPossibleSuffix: 'vis'
	'Elvis' withoutPossibleSuffix: 'Paul'
	"
	^(self endsWith: aString)
		ifTrue: [ self copyFrom: 1 to: self size - aString size ]
		ifFalse: [self]
]

{ #category : 'converting' }
CharacterSequence >> withoutPrefix: aString [
	"
	'Elvis' withoutPrefix: 'El'
	'Elvis' withoutPrefix: 'Paul'
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: aString size+1 to: self size ]
]

{ #category : 'converting' }
CharacterSequence >> withoutSeparators [
	
	^self reject: [ :aCharacter | aCharacter isSeparator ]

	"
	' abc  d   ' withoutSeparators
	"
]

{ #category : 'converting' }
CharacterSequence >> withoutSuffix: aString [
	"
	'Elvis' withoutSuffix: 'vis'
	'Elvis' withoutSuffix: 'Paul'
	"
	^(self endsWith: aString)
		ifTrue: [ self copyFrom: 1 to: self size - aString size ]
]

{ #category : 'converting' }
CharacterSequence >> withoutTrailingBlanks [
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last := self lastNonSeparator.
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	"
	' abc  d   ' withoutTrailingBlanks
	"
]

{ #category : 'fileman-converting' }
CharacterSequence >> withoutWindowsDriveName [
	^self beginsWithWindowsDriveName 
		ifFalse: [ self ]
		ifTrue: [ self copyFrom: 3 to: self size ]
]

{ #category : 'as yet unclassified' }
CharacterSequence >> writeStream [
	"For the common idiom
		'' writeStream
	prefer a Unicode enabled writeStream that offers best performance if Unicode is ever involved,
	and no real slowdown if ASCII only."

	self isEmpty ifTrue: [ ^UnicodeString writeStream ].
	^super writeStream
]

{ #category : 'text conversion color' }
CharacterSequence >> yellow [
	"Stuff like
	'Hello world' yellow edit
	"
	^self asText yellow
]
