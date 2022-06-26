'From Cuis 6.0 [latest update: #5321] on 26 June 2022 at 3:52:33 pm'!

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:50:44'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	String string: bytes lineIndicesDo: [ :startByte :endByteWithoutDelimiters :endByte |
		| start end endWithoutDelimiters |
		start _ self codePointIndexAt: startByte.
		end _ (self codePointIndexAt: endByte+1)-1.
		endWithoutDelimiters _ endByteWithoutDelimiters = endByte
			ifTrue: [end]
			ifFalse: [(self codePointIndexAt: endByteWithoutDelimiters+1)-1].
		aBlock
			value: start
			value: endWithoutDelimiters
			value: end
		]! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/26/2022 15:49:41'!
codePointIndexAt: byteIndex
	| codePointIndex byteCursor |
	byteIndex < 2 ifTrue: [ ^byteIndex ]. "For 0 and 1"
	self isAscii ifTrue: [ ^ byteIndex ].
	byteIndex = (bytes size+1) ifTrue: [
		"The index of the code point starting at a hypotetical extra byte.
		Useful to find the code point index for the last byte in a sequence. See senders."
		^self size+1 ].
	codePointIndex _ byteCursor _ 1.
	strideIndex ifNil: [ self computeCountAndIndex ].
	strideIndex ifNotEmpty: [  
		strideIndex 
			findBinaryIndex: [ :x | byteIndex - x ] 
			do: [ :found | ^ (found * self stride) + 1 ] 
			ifNone: [ :lower :upper |
				lower = 0 
			 		ifFalse: [ 
						byteCursor _ strideIndex at: lower. 
						codePointIndex _ (lower * self stride) + 1 ] ] ].
	[ byteCursor <= bytes size ] whileTrue: [
		byteCursor = byteIndex 
			ifTrue: [ ^ codePointIndex ].
		byteCursor _ byteCursor + (self encodedByteCountAt: byteCursor).
		codePointIndex _ codePointIndex + 1 ].
	^ 0! !

