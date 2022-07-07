'From Cuis 6.0 [latest update: #5362] on 7 July 2022 at 12:05:35 pm'!

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:09:54'!
atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	"
	^self primAtEnd: fileID
	"
	^self position = self size! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 10:53:07'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"See comment at PositionableStream.
	Fast version."

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


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:09:47'!
atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	"
	^self primAtEnd: fileID
	"
	^self position = self size! !

!methodRemoval: UniFileStream #upTo:delimiterIsTerminator: stamp: 'jmv 7/7/2022 12:05:07'!
UniFileStream removeSelector: #upTo:delimiterIsTerminator:!
