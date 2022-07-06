'From Cuis 6.0 [latest update: #5356] on 5 July 2022 at 8:53:49 pm'!

!Utf8Symbol methodsFor: 'object serialization' stamp: 'jmv 7/5/2022 20:52:50'!
comeFullyUpOnReload: smartRefStream
	"
	(Object unStream: (ReferenceStream streamedRepresentationOf: #hello)) = #hello
	"
	^self asString asSymbol! !

