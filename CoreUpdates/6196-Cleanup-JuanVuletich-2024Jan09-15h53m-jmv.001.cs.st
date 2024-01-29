'From Cuis6.3 [latest update: #6188] on 9 January 2024 at 3:56:35 pm'!

!Base64MimeConverter class methodsFor: 'additional services' stamp: 'jmv 1/9/2024 15:54:19'!
mimeDecodeToBytes: aStream 
	"Return a ReadWriteStream of the original ByteArray.  aStream has only 65 innocuous character values.  aStream is not binary.  (See class comment). 4 bytes in aStream goes to 3 bytes in output."

	| me |
	aStream position: 0.
	me := self new mimeStream: aStream.
	me dataStream: (ReadWriteStream on: (ByteArray new: aStream size * 3 // 4)).
	me mimeDecodeToByteArray.
	me dataStream position: 0.
	^ me dataStream! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 1/9/2024 15:53:54'!
nextLine
	"Answer next line (may be empty), or nil if at end"

"	self atEnd ifTrue: [^nil].
	^self upTo: Character cr
"
	^self crLfNextLine! !

!methodRemoval: ReferenceStream class #on: stamp: 'jmv 1/9/2024 15:56:06'!
ReferenceStream class removeSelector: #on:!
