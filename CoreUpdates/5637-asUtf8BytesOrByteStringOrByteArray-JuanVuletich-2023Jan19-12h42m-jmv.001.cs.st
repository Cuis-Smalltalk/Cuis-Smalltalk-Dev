'From Cuis 6.0 [latest update: #5636] on 19 January 2023 at 1:08:37 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 1/19/2023 13:05:54'!
asUtf8BytesOrByteStringOrByteArray
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self subclassResponsibility! !


!String methodsFor: 'converting' stamp: 'jmv 1/19/2023 13:04:56'!
asUtf8BytesOrByteStringOrByteArray
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 1/19/2023 13:04:35'!
asUtf8BytesOrByteStringOrByteArray
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self asUtf8Bytes! !


!ByteArray methodsFor: 'converting' stamp: 'jmv 1/19/2023 13:02:22'!
asUtf8BytesOrByteStringOrByteArray
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self! !

