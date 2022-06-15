'From Cuis 6.0 [latest update: #5267] on 12 June 2022 at 5:24:45 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/12/2022 17:10:16'!
isVowel
	"Answer whether the receiver is one of the vowels, AEIOU, in upper or 
	lower case, and with various diacritical marks."
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^ch isVowel ].
	^false! !


!CharacterSequence methodsFor: 'copying' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
copyReplaceTokens: oldSubstring with: newSubstring
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"
	'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'
	'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick' asUtf8String
	'File asFile Files File''s File' copyReplaceTokens: 'File' asUtf8String with: 'Snick'
	'File asFile Files File''s File' copyReplaceTokens: 'File' asUtf8String with: 'Snick' asUtf8String
	'File asFile Files File''s File' asUtf8String copyReplaceTokens: 'File' with: 'Snick'
	'File asFile Files File''s File' asUtf8String copyReplaceTokens: 'File' with: 'Snick' asUtf8String
	'File asFile Files File''s File' asUtf8String copyReplaceTokens: 'File' asUtf8String with: 'Snick'
	'File asFile Files File''s File' asUtf8String copyReplaceTokens: 'File' asUtf8String with: 'Snick' asUtf8String
	"! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/12/2022 16:22:58'!
flattenTo: flattenedStream

	flattenedStream nextPut: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:09:06'!
squeezedTo: n
	"
Examples:
	Do nothing:
		'This one is a rather long phrase' squeezedTo: 32
		'This one is a rather long phrase' asUtf8String squeezedTo: 32

	1-remove blanks (result can be shorter than asked):
		'This one is a rather long phrase' squeezedTo: 30
		'This one is a rather long phrase' asUtf8String squeezedTo: 30

	2-remove necessary trailing vowels
		'This one is a rather long phrase' squeezedTo: 24
		'This one is a rather long phrase' asUtf8String squeezedTo: 24

	3-truncate as needed (and add ellipsis)
		'This one is a rather long phrase' squeezedTo: 15
		'This one is a rather long phrase' asUtf8String squeezedTo: 15

	4-avoid ellipsis
		'This one is a rather long phrase' squeezedTo: 5
		'This one is a rather long phrase' asUtf8String squeezedTo: 5
	"
	| vowelCount read write i char allowedVowels str desiredSize postFix j |
	str _ self.
	desiredSize _ n.
	str size <= n ifTrue: [^str].
	str _ str asCamelCase.
	str size <= n ifTrue: [^str].
	postFix _ ''.
	desiredSize _ n - postFix size.
	vowelCount _ str
		inject: 0
		into: [:prev :each | each isVowel ifTrue: [prev + 1] ifFalse: [prev]].
	str size - vowelCount <= desiredSize
		ifTrue: [allowedVowels _ vowelCount - (str size - desiredSize)]
		ifFalse: [
			allowedVowels _ 0.
			postFix _ '...'.
			n - postFix size < 5 ifTrue: [postFix _ ''].
			desiredSize _ n - postFix size].

	read _ str readStream.
	write _ self class writeStream.
	i _ 0.
	j _ 0.
	[read atEnd not and: [j < desiredSize]] whileTrue: [
		char _ read next.
		(char isVowel not or: [i < allowedVowels]) ifTrue: [
			char isVowel ifTrue: [i := i + 1].
			write nextPut: char.
			j _ j + 1]].
	str _ write contents , postFix.
	^ str! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:17:56'!
withoutPrefix: aString
	"
	'Elvis' withoutPrefix: 'El'
	'Elvis' withoutPrefix: 'Paul'
	'Elvis' withoutPrefix: 'El' asUtf8String
	'Elvis' withoutPrefix: 'Paul' asUtf8String
	'Elvis' asUtf8String withoutPrefix: 'El'
	'Elvis' asUtf8String withoutPrefix: 'Paul'
	'Elvis' asUtf8String withoutPrefix: 'El' asUtf8String
	'Elvis' asUtf8String withoutPrefix: 'Paul' asUtf8String
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: aString size+1 to: self size ]! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/12/2022 16:32:43'!
caseInsensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^self asByteString caseInsensitiveLessOrEqual: aString ].
		(aString isUtf8String and: [ aString isAscii ]) ifTrue: [
			^self asByteString sameAs: aString asByteString ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #slowUnicode.
	^self asLowercase <= aString asLowercase! !


!FileList methodsFor: 'private' stamp: 'jmv 6/12/2022 16:54:38'!
sortBlock
	"Answer block to decide what order to display the directory entries."

	^ [ :xx :yy | | x y |
		(xx isDirectory == yy isDirectory) 
			ifTrue: [  
				sortAscending
					ifTrue: [ x _ xx. y _ yy ]
					ifFalse: [ x _ yy. y _ xx ].
				"sort by user-specified criterion"
				sortMode = #name 
					ifTrue: [x name <= y name]
					ifFalse: [ sortMode = #date
						ifTrue: [ x modificationTime = y modificationTime
								ifTrue: [ x name <= y name ]
								ifFalse: [ x modificationTime <= y modificationTime ] ]
						ifFalse: [ "size"
							x fileSize = y fileSize 
								ifTrue: [ x name <= y name ]
								ifFalse: [ x fileSize <= y fileSize ] ] ] ]
			ifFalse: [
				"directories always precede files"
				xx isDirectory ] ]! !

!methodRemoval: String #caseSensitiveLessOrEqual: stamp: 'jmv 6/12/2022 16:26:53'!
String removeSelector: #caseSensitiveLessOrEqual:!
!methodRemoval: String #withoutPrefix: stamp: 'jmv 6/12/2022 17:12:39'!
String removeSelector: #withoutPrefix:!
!methodRemoval: String #squeezedTo: stamp: 'jmv 6/12/2022 17:10:45'!
String removeSelector: #squeezedTo:!
!methodRemoval: String #flattenTo: stamp: 'jmv 6/12/2022 16:22:58'!
String removeSelector: #flattenTo:!
!methodRemoval: String #copyReplaceTokens:with: stamp: 'jmv 6/12/2022 16:57:56'!
String removeSelector: #copyReplaceTokens:with:!
