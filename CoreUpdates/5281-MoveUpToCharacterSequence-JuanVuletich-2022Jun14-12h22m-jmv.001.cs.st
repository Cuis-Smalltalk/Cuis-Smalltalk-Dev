'From Cuis 6.0 [latest update: #5279] on 14 June 2022 at 12:26:32 pm'!

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/14/2022 12:25:35'!
append: aStringOrText

	^ aStringOrText appendToString: self! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 6/23/2011 09:54'!
commonPrefixWith: aString
	"Return the size of the longest common prefix with aString.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly.
	'this is a string' commonPrefixWith: 'this is another'
	'this is a string' commonPrefixWith: ''
	'' commonPrefixWith: 'this is another'
	't' commonPrefixWith: 'this is another'
	'txxxxxxxxx' commonPrefixWith: 'this is another'

	'this is a string' commonPrefixWith: 'this is another' asUtf8String
	'this is a string' commonPrefixWith: '' asUtf8String
	'' commonPrefixWith: 'this is another' asUtf8String
	't' commonPrefixWith: 'this is another' asUtf8String
	'txxxxxxxxx' commonPrefixWith: 'this is another' asUtf8String

	'this is a string' asUtf8String commonPrefixWith: 'this is another'
	'this is a string' asUtf8String commonPrefixWith: ''
	'' asUtf8String commonPrefixWith: 'this is another'
	't' asUtf8String commonPrefixWith: 'this is another'
	'txxxxxxxxx' asUtf8String commonPrefixWith: 'this is another'

	'this is a string' asUtf8String commonPrefixWith: 'this is another' asUtf8String
	'this is a string' asUtf8String commonPrefixWith: '' asUtf8String
	'' asUtf8String commonPrefixWith: 'this is another' asUtf8String
	't' asUtf8String commonPrefixWith: 'this is another' asUtf8String
	'txxxxxxxxx' asUtf8String commonPrefixWith: 'this is another' asUtf8String
		"
	^ self commonPartWith: aString startAt: 1 stopAt: self size applying: [ :c | c ]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 10:54:41'!
asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString |
	aString _ self select: [ :el | el isValidInIdentifiers ].
	(aString isEmpty or: [aString first isValidStartOfIdentifiers not])
		ifTrue:	 [aString _ 'a', aString].
	^ shouldBeCapitalized ifTrue: [ aString capitalized ] ifFalse: [ aString uncapitalized ]

"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false

'234Fred987' asUtf8String asIdentifier: false
'235Fred987' asUtf8String asIdentifier: true
'' asUtf8String asIdentifier: true
'()87234' asUtf8String asIdentifier: false
'())z>=PPve889  U >' asUtf8String asIdentifier: false
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'ls 10/28/2003 12:24'!
asSmalltalkComment
	"return this string, munged so that it can be treated as a comment in Smalltalk code.  Quote marks are added to the beginning and end of the string, and whenever a solitary quote mark appears within the string, it is doubled"

	^String streamContents:  [ :str |
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

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 10:56:52'!
asValidInstanceVariableName
	| answer |
	answer _ self asIdentifier: false.
	(Scanner pseudoVariableNames includes: answer)
		ifTrue: [ answer _ answer , 'x' ].
	^ answer

"
'234znak 43 ) 2' asValidInstanceVariableName
'234 xx\ Uml /ler42342380-4' asValidInstanceVariableName
'super' asValidInstanceVariableName
'234znak 43 ) 2' asUtf8String asValidInstanceVariableName
'234 xx\ Uml /ler42342380-4' asUtf8String asValidInstanceVariableName
'super' asUtf8String asValidInstanceVariableName
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 10:55:40'!
asValidSelector
	^ self asIdentifier: false

"
'234znak 43 ) 2' asValidSelector
'234znak 43 ) 2' asUtf8String asValidSelector
"! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 6/13/2022 11:04:05'!
asDriveName
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)
	
Windows
	'C:\' asDriveName 'C:'
	'NotAChance' asDriveName nil
	
Linux
	'/media/cdrom' asDriveName nil

MacOsX
    '/SanDisk32-NTFS' asDriveName nil
	
	"
	FileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].
	
	^ nil! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'mu 3/4/2007 10:22'!
beginsWithPathSeparator
	self ifEmpty: [^false].
	^self first isPathSeparator! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'jmv 6/13/2022 10:47:58'!
isAbsolutePathName
	"Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt'
	and even '/Users/Someone/file.txt' are considered an absolute pathName.
	This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice."
	
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].

	FileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	^ false! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'jmv 6/13/2022 10:47:29'!
isDriveName
	FileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	^false! !

!CharacterSequence methodsFor: 'paragraph support' stamp: 'jmv 6/13/2022 10:49:26'!
endOfParagraphBefore: aNumber
	"Return the index of the last Character newLineCharacter before position aNumber, or zero if this is the first paragraph.
	'ddd' endOfParagraphBefore: 3
	'dd
	d' endOfParagraphBefore: 4
	"
	^ self lastIndexOf: Character newLineCharacter startingAt: aNumber - 1 ifAbsent: [ 0 ]! !

!CharacterSequence methodsFor: 'paragraph support' stamp: 'jmv 6/13/2022 10:48:57'!
indentationIfBlank: aBlock
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator tab ch |
	tab _ Character tab.
	reader _ self readStream.
	leadingTabs _ 0.
	[ reader atEnd not and: [ (ch _ reader next) == tab ]] whileTrue: [
		leadingTabs _ leadingTabs + 1 ].
	lastSeparator _ leadingTabs + 1.
	[ reader atEnd not and: [
		ch isSeparator and: [ ch isLineSeparator not ]]] whileTrue: [
			lastSeparator _ lastSeparator + 1.
			ch _ reader next ].
	lastSeparator = self size | (ch notNil and: [ch isLineSeparator]) ifTrue: [
		^ aBlock value: leadingTabs ].
	^ leadingTabs! !


!Text methodsFor: 'accessing' stamp: 'KenD 9/5/2016 15:59:40'!
append: stringOrText

	^ stringOrText appendToText: self! !

!methodRemoval: String #endOfParagraphBefore: stamp: 'jmv 6/14/2022 12:24:22'!
String removeSelector: #endOfParagraphBefore:!
!methodRemoval: String #indentationIfBlank: stamp: 'jmv 6/14/2022 12:24:22'!
String removeSelector: #indentationIfBlank:!
!methodRemoval: String #asIdentifier: stamp: 'jmv 6/14/2022 12:24:08'!
String removeSelector: #asIdentifier:!
!methodRemoval: String #asSmalltalkComment stamp: 'jmv 6/14/2022 12:22:55'!
String removeSelector: #asSmalltalkComment!
!methodRemoval: String #commonPrefixWith: stamp: 'jmv 6/14/2022 12:24:08'!
String removeSelector: #commonPrefixWith:!
!methodRemoval: String #asValidInstanceVariableName stamp: 'jmv 6/14/2022 12:24:35'!
String removeSelector: #asValidInstanceVariableName!
!methodRemoval: String #isDriveName stamp: 'jmv 6/14/2022 12:22:55'!
String removeSelector: #isDriveName!
!methodRemoval: String #asDriveName stamp: 'jmv 6/14/2022 12:22:55'!
String removeSelector: #asDriveName!
!methodRemoval: String #asValidSelector stamp: 'jmv 6/14/2022 12:24:35'!
String removeSelector: #asValidSelector!
!methodRemoval: String #append: stamp: 'jmv 6/14/2022 12:25:35'!
String removeSelector: #append:!
!methodRemoval: String #isAbsolutePathName stamp: 'jmv 6/14/2022 12:22:55'!
String removeSelector: #isAbsolutePathName!
!methodRemoval: String #beginsWithPathSeparator stamp: 'jmv 6/14/2022 12:22:55'!
String removeSelector: #beginsWithPathSeparator!
