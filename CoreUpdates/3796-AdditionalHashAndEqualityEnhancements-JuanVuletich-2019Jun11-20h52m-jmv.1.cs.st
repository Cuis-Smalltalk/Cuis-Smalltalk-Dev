'From Cuis 5.0 of 7 November 2016 [latest update: #3795] on 11 June 2019 at 8:54:49 pm'!

!SequenceableCollection methodsFor: 'comparing' stamp: 'jmv 6/11/2019 18:53:56'!
hashQuick
	| hash size step |

	size _ self size.
	hash _ (self species hash + size hash) hashMultiply.
	step _ size < 64 ifTrue: [1] ifFalse: [size//64].
	1 to: size by: step do: [ :i | | elem |
		elem _ self at: i.
		elem == self ifFalse: [
			hash _ (hash + elem hash) hashMultiply]].
	^hash! !


!FloatArray methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:12:42'!
hashFull
	| hash |
	<primitive:'primitiveHashArray' module: 'FloatArrayPlugin'>
	hash _ (self species hash + self size hash) hashMultiply.
	1 to: self size do: [:i | hash _ (hash + (self basicAt: i)) hashMultiply].
	^hash! !


!Association methodsFor: 'comparing' stamp: 'jmv 6/11/2019 18:22:00'!
hash
	"Hash is reimplemented because = is implemented."

	value == self ifTrue: [ ^ key hash ].
	^key hash bitXor: value hash.! !


!Collection methodsFor: 'comparing' stamp: 'jmv 6/11/2019 19:07:39'!
hash
	"A default hash function for any collection. Note that this method only considers a few elements so you might want to refine this behavior."

	| size hash count |
	size _ self size.
	hash _ self species hash bitXor: size hash.
	count _ 0.
	self do: [ :elem |
		elem == self ifFalse: [
			hash _ hash bitXor: elem hash.
			count _ count + 1.
			count =64 ifTrue: [
				^ hash]]].
	^ hash! !


!SequenceableCollection methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:08:53'!
hash
	"Subclasses might use other methods.
	However #hashQuick is suggested for very large collections."
	^ self hashQuick! !


!String methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:23:20'!
= aString 
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a String or Symbol, maybe it is a Text?"
	aString species == String ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].
	
	self size > 256 ifTrue: [
		self hashQuick = aString hashQuick ifFalse: [ ^false ]].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 2! !

!String methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:13:53'!
hash
	"#hash is implemented, because #= is implemented"
	self size > 256 ifTrue: [ ^ self hashQuick ].
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !


!ByteArray methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:09:49'!
hash
	"#hash is implemented, because #= is implemented"
	self size > 256 ifTrue: [ ^ self hashQuick ].
	^self class
		hashBytes: self
		startingWith: self species hash! !


!FloatArray methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:22:31'!
= another 
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	self size > 256 ifTrue: [
		self hashQuick = another hashQuick ifFalse: [ ^false ]].
	^self primitiveEqual: another! !

!FloatArray methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:10:44'!
hash
	self size > 256 ifTrue: [ ^ self hashQuick ].
	^ self hashFull! !


!Set methodsFor: 'testing' stamp: 'jmv 6/11/2019 18:46:53'!
= aSet

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	self species == aSet species ifFalse: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true! !


!Dictionary methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:13'!
is: aSymbol
	^aSymbol == #Dictionary or: [ super is: aSymbol ]! !

!Dictionary methodsFor: 'comparing' stamp: 'jmv 6/11/2019 18:47:15'!
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value".

	self == aDictionary ifTrue: [^ true].	"Any object is equal to itself"
	self species == aDictionary species ifFalse: [^ false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true

! !

