'From Cuis 5.0 [latest update: #4940] on 25 October 2021 at 11:19:49 am'!

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 10/25/2021 11:18:50'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"Answer a subcollection from the current access position to the 
	occurrence of delimiter in the receiver. 

	If delimiterIsTerminator is false, delimiter is considered a separator:
		- Skip delimiter, but don't include it in the answer.
		- If delimiter is not found, answer the entire rest of the receiver.

	If delimiterIsTerminator is true, answer will end with delimeter:
		- Read delimiter, include it in the answer.
		- If delimiter is not found, answer nil and don't advance receiver at all.
	This is especially useful if we are appended new stuff while simultaneusly being read."

	| prevPosition element answer |
	prevPosition _ self position.
	answer _ self collectionSpecies streamContents: [ :newStream |
		[self atEnd or: [(element _ self next) = delimiter]]
			whileFalse: [newStream nextPut: element].
		delimiterIsTerminator ifTrue: [
			element = delimiter
				ifTrue: [newStream nextPut: element]
				ifFalse: [
					self position: prevPosition.
					^ nil ]]].
	^answer.! !


!ReadStream methodsFor: 'accessing' stamp: 'jmv 10/25/2021 09:58:29'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"See comment at PositionableStream.
	Fast version using indexOf:"

	| start end |

	start _ position+1.
	end _ collection indexOf: delimiter startingAt: start ifAbsent: [0].

	"not present"	
	end = 0 ifTrue: [
		^ delimiterIsTerminator
			ifTrue: [
				self position: start-1.
				nil ]
			ifFalse: [self upToEnd]].

	"skip to the end and return the data passed over"
	position _ end.
	^collection copyFrom: start to: (delimiterIsTerminator ifTrue: [end] ifFalse: [end-1])! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/25/2021 10:00:18'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"See comment at PositionableStream.
	Fast version to speed up nextChunk"

	| pos buffer count skipSeparator tryAgain |
	skipSeparator _ delimiterIsTerminator ifTrue: [0] ifFalse: [1].
	collection ifNotNil: [
		(position < readLimit and: [
			(pos _ collection indexOf: delimiter startingAt: position + 1) <= readLimit and: [
				pos > 0 ] ]) ifTrue: [
					^ collection copyFrom: position + 1 to: (position _ pos) - skipSeparator ] ].

	pos _ self position.
	buffer _ self next: 2000.
	(count _ buffer indexOf: delimiter) > 0 ifTrue: [
		"Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - skipSeparator].

	self atEnd ifTrue: [
		"Never found it, and hit end of file"
		^ delimiterIsTerminator ifTrue: [self position: pos. nil] ifFalse: [buffer]].

	"Never found it, but there's more..."
	tryAgain _ self upTo: delimiter delimiterIsTerminator: delimiterIsTerminator.
	tryAgain ifNil: [
		self position: pos.
		^ nil ].
	^ buffer, tryAgain.! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 10/24/2021 20:57:46'!
upTo: delimiter 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of delimiter in the receiver. If 
	delimiter is not in the collection, answer the entire rest of the receiver."

	^self upTo: delimiter delimiterIsTerminator: false.! !

!methodRemoval: StandardFileStream #upTo: stamp: 'jmv 10/25/2021 10:05:36'!
StandardFileStream removeSelector: #upTo:!
!methodRemoval: ReadStream #upTo: stamp: 'jmv 10/25/2021 10:05:36'!
ReadStream removeSelector: #upTo:!
