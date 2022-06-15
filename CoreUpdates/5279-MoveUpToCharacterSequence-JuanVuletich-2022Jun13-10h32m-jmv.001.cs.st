'From Cuis 6.0 [latest update: #5276] on 13 June 2022 at 10:44:57 am'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:08'!
lines
	"Answer an array of lines composing this receiver without the line ending delimiters."

	^Array
		streamContents: [ :lines | self linesDo: [ :aLine | lines nextPut: aLine ]]
		estimatedSize: (self size // 60 max: 16)! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 10:39:11'!
join: aCollection 
	"
	'*' join: #('WWWWW' 'W  EW' 'zzzz')
	'*' asUtf8String join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "

	^self species streamContents: [ :strm |
		aCollection
			do: [ :elem | strm nextPutAll: elem asString]
			separatedBy: [strm nextPutAll: self]].! !

!CharacterSequence methodsFor: 'converting' stamp: 'sw 9/2/1998 17:09'!
truncateWithElipsisTo: maxLength
	"Return myself or a copy suitably shortened but with elipsis added"

	^ self size <= maxLength
		ifTrue:
			[self]
		ifFalse:
			[(self copyFrom: 1 to: (maxLength - 3)), '...']


	"'truncateWithElipsisTo:' truncateWithElipsisTo: 20"! !

!CharacterSequence methodsFor: 'fileman-accessing' stamp: 'jmv 10/16/2015 14:21'!
indexOfLastPathSeparator
	"
	'pp.txt' indexOfLastPathSeparator
	'/pp.txt' indexOfLastPathSeparator
	'a/pp.txt' indexOfLastPathSeparator
	'b/a/pp.txt' indexOfLastPathSeparator
	'/b/a/pp.txt' indexOfLastPathSeparator
	"
	^self findLast: [ :char | char isPathSeparator ]! !

!CharacterSequence methodsFor: 'fileman-accessing' stamp: 'mu 5/7/2007 15:55'!
pathAndLocalName
	| index path localName |
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^{''. self}].

	path := self copyFrom: 1 to: index-1.
	localName := self copyFrom: index+1 to: self size.

	^{path. localName}

	! !

!CharacterSequence methodsFor: 'fileman-accessing' stamp: 'mu 5/7/2007 15:49'!
upToLastPathSeparator
	| index |
	self ifEmpty: [^self].
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
	! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 6/13/2022 10:34:24'!
surroundedBySingleQuotes
	"Answer the receiver with leading and trailing quotes.  "

	^ `(Utf8String with: $')`, self, `(Utf8String with: $')`! !

!methodRemoval: String #truncateWithElipsisTo: stamp: 'jmv 6/13/2022 10:35:19'!
String removeSelector: #truncateWithElipsisTo:!
!methodRemoval: String #join: stamp: 'jmv 6/13/2022 10:38:37'!
String removeSelector: #join:!
!methodRemoval: String #indexOfLastPathSeparator stamp: 'jmv 6/13/2022 10:36:30'!
String removeSelector: #indexOfLastPathSeparator!
!methodRemoval: String #lines stamp: 'jmv 6/13/2022 10:43:32'!
String removeSelector: #lines!
!methodRemoval: String #upToLastPathSeparator stamp: 'jmv 6/13/2022 10:36:21'!
String removeSelector: #upToLastPathSeparator!
!methodRemoval: String #pathAndLocalName stamp: 'jmv 6/13/2022 10:38:09'!
String removeSelector: #pathAndLocalName!
