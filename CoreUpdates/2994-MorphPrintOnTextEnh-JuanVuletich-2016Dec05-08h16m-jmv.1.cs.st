'From Cuis 5.0 of 7 November 2016 [latest update: #2993] on 5 December 2016 at 8:17:22 am'!

!Morph methodsFor: 'printing' stamp: 'jmv 12/5/2016 08:16:19'!
printOn: aStream 
	"Add the identity of the receiver to a stream"
	aStream isText
		ifTrue: [
			aStream
				withAttribute: (TextAnchor new anchoredFormOrMorph: (owner ifNil: [self] ifNotNil: [self imageForm:32]))
				do: [ aStream nextPut: $* ].
			^ self].
	super printOn: aStream. "a(n) className"
	aStream 
		nextPut: $(;
		print: self identityHash;
		nextPut: $).
	self valueOfProperty: #morphName ifPresentDo: [ :x | aStream nextPutAll: x asString]! !

