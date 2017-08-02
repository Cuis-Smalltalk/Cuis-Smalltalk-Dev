'From Cuis 5.0 of 7 November 2016 [latest update: #3131] on 2 August 2017 at 12:59:24 pm'!

!Scanner methodsFor: 'expression types' stamp: 'jmv 8/2/2017 12:59:08'!
scanLitByteVec
	"Also accept Floats besides bytes!!
	#[1 2 3 255]
	#[1.0 0.2 1.0]
	#[1.0 -0.2e-23 1.0e4]
	"
	| stream |
	stream _ nil.
	[ tokenType == #rightBracket or: [ tokenType == #doIt ] ] whileFalse: [
		(token == #- and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: [
			self scanToken.
			token _ token negated ].
		((token isInteger and: [ token between: 0 and: 255 ]) or: [token isFloat])
			ifFalse: [ ^ self offEnd: '8-bit integer, floating point number, or right bracket expected' ].
		stream ifNil: [
			stream _ ((token isFloat ifTrue: [Float64Array] ifFalse: [ByteArray]) new: 16) writeStream ].
		stream nextPut: token.
		self scanToken ].
	token _ stream
		ifNotNil: [ stream contents ]
		ifNil: [
			"For back compatibility, if empty, assume ByteArray"
			ByteArray new ]! !

