'From Cuis7.5 [latest update: #7621] on 27 September 2025 at 10:26:17 am'!

!Morph methodsFor: 'printing' stamp: 'hlsf 9/27/2025 10:24:54'!
printOn: aStream 
	"Add the identity of the receiver to a stream"
	aStream isText
		ifTrue: [
			aStream
				withAttribute: (TextAnchor new anchoredFormOrMorph: (owner ifNil: [self] ifNotNil: [self imageForm: 32@32 depth: 32]))
				do: [ aStream nextPut: $* ].
			^ self].
	super printOn: aStream. "a(n) className"
	aStream 
		nextPut: $(;
		print: self identityHash;
		nextPut: $).
	self valueOfProperty: #morphName ifPresentDo: [ :x | 
		aStream nextPutAll: ' :: ';
			nextPutAll: x asString]! !

