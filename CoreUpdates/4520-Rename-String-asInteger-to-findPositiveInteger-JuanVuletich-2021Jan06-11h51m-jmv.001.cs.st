'From Cuis 5.0 [latest update: #4519] on 6 January 2021 at 12:01:26 pm'!

!String methodsFor: 'converting' stamp: 'jmv 1/6/2021 11:51:13'!
findPositiveInteger 
	"Answer the Integer created by interpreting the receiver as the string representation of an integer.
	Answer nil if no digits, else find the first digit and then all consecutive digits after that"

	| startPosition tail endPosition |
	startPosition _ self findFirst: [:ch | ch isDigit].
	startPosition = 0 ifTrue: [^ nil].
	tail _ self copyFrom: startPosition to: self size.
	endPosition _ tail findFirst: [:ch | ch isDigit not].
	endPosition = 0 ifTrue: [endPosition _ tail size + 1].
	^ Number readFrom: (tail copyFrom: 1 to: endPosition - 1) readStream

"
'1796exportFixes-tkMX' findPositiveInteger
'1848recentLogFile-sw'  findPositiveInteger
'donald' findPositiveInteger
'abc234def567' findPositiveInteger
"! !

!methodRemoval: String #asInteger stamp: 'jmv 1/6/2021 11:57:59'!
String removeSelector: #asInteger!
