'From Cuis7.5 [latest update: #7768] on 18 December 2025 at 10:21:17 am'!

!CharacterSequence methodsFor: 'as yet unclassified' stamp: 'jmv 18/Dec/2025 10:21:06'!
writeStream
	"For the common idiom
		'' writeStream
	prefer a Unicode enabled writeStream that offers best performance if Unicode is ever involved,
	and no real slowdown if ASCII only."

	self isEmpty ifTrue: [ ^UnicodeString writeStream ].
	^super writeStream! !


!String class methodsFor: 'stream creation' stamp: 'jmv 18/Dec/2025 10:16:35'!
writeStream
	"Generally prefer
		UnicodeString writeStream
	that offers better performance in most cases.
	Only use this form if:
	- Zero or close to zero chance of any Unicode Character (code point > 255)
	- You know what you are doing
	"
	^ WriteStream on: (self new: 100)! !


!UnicodeString class methodsFor: 'stream creation' stamp: 'jmv 18/Dec/2025 10:14:34'!
writeStream
	"Answer a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, UnicodeStrings.
	Generally prefer this. It offers best overall performance."

	| stream |	
	stream := Utf8EncodedWriteStream on: (ByteArray new: 100).
	^stream! !

