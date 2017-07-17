'From Cuis 5.0 of 7 November 2016 [latest update: #3121] on 12 July 2017 at 1:50:53 pm'!

!Float64Array methodsFor: 'testing' stamp: 'jmv 7/11/2017 14:04:20'!
isLiteral
	"so that 
	#(1 #[1.0 2 3] 5)
	prints itself"
	^self class == Float64Array! !

!Float64Array methodsFor: 'printing' stamp: 'jmv 7/11/2017 14:10:44'!
printOn: aStream

	self storeOn: aStream! !

!Float64Array methodsFor: 'printing' stamp: 'jmv 7/11/2017 14:10:41'!
storeOn: aStream

	aStream nextPutAll: '#['.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]! !


!ByteArray methodsFor: 'printing' stamp: 'jmv 7/11/2017 14:09:10'!
printOn: aStream
	self storeOn: aStream! !


!Scanner methodsFor: 'expression types' stamp: 'jmv 7/12/2017 13:50:30'!
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
	token _ stream contents! !

