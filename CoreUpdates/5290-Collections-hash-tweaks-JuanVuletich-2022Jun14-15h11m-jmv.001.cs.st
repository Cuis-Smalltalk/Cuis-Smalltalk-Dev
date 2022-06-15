'From Cuis 6.0 [latest update: #5284] on 14 June 2022 at 3:14:50 pm'!

!SequenceableCollection methodsFor: 'comparing' stamp: 'jmv 6/14/2022 14:50:11'!
hashQuick: initialHash
	| hash size step |

	size _ self size.
	hash _ (initialHash + size hash) hashMultiply.
	step _ size < 64 ifTrue: [1] ifFalse: [size//64].
	1 to: size by: step do: [ :i | | elem |
		elem _ self at: i.
		elem == self ifFalse: [
			hash _ (hash + elem hash) hashMultiply]].
	^hash! !


!ByteArray class methodsFor: 'byte based hash' stamp: 'jmv 6/14/2022 14:45:55'!
minSizeForHashQuick
	"Answer the size above which we prefer using the #hashQuick method"

	^256! !


!FloatArray class methodsFor: 'byte based hash' stamp: 'jmv 6/14/2022 14:47:18'!
minSizeForHashQuick
	"Answer the size above which we prefer using the #hashQuick method"

	^256! !


!SequenceableCollection methodsFor: 'comparing' stamp: 'jmv 6/14/2022 14:50:31'!
hashQuick
	^self hashQuick: self species hash! !


!String methodsFor: 'comparing' stamp: 'jmv 6/14/2022 14:51:19'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, Utf8String, Utf8Symbol, Utf32String and Text
	if they hold same Characters."

	self size >  ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: String hash ].
	^ByteArray
		hashBytes: self
		startingWith: String hash! !


!ByteArray methodsFor: 'comparing' stamp: 'jmv 6/14/2022 14:52:04'!
hash
	"#hash is implemented, because #= is implemented"
	self size > ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: ByteArray hash ].
	^self class
		hashBytes: self
		startingWith: ByteArray hash! !


!FloatArray methodsFor: 'comparing' stamp: 'jmv 6/14/2022 14:47:35'!
= another 
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	self size > FloatArray minSizeForHashQuick ifTrue: [
		self hashQuick = another hashQuick ifFalse: [ ^false ]].
	^self primitiveEqual: another! !

!FloatArray methodsFor: 'comparing' stamp: 'jmv 6/14/2022 14:47:42'!
hash
	self size > FloatArray minSizeForHashQuick ifTrue: [ ^ self hashQuick ].
	^ self hashFull! !

