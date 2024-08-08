'From Cuis7.1 [latest update: #6574] on 8 August 2024 at 5:24:53 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 8/8/2024 16:48:51'!
utf8BytesSize
	"Optimized where appropriate."
	^self asUtf8Bytes size! !


!String methodsFor: 'accessing' stamp: 'jmv 8/8/2024 16:54:52'!
utf8BytesSize
	"Avoid creating the actual byte array."

	| byteCount |
	"Fast lane."
	self isAscii ifTrue: [ ^self size ].

	"Need to do UTF-8 encoding."
	byteCount := 0.
	self do: [ :c |
		Character
			evaluate: [ :byte | byteCount := byteCount + 1 ]
			withUtf8BytesOf: c codePoint ].
	^byteCount! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 8/8/2024 16:56:37'!
utf8BytesSize
	"We are already UTF-8 encoded."

	^bytes size! !

