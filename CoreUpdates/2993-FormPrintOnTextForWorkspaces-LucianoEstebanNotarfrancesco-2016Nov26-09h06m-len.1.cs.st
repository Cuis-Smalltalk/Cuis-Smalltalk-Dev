'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 26 November 2016 at 9:08:39 am'!

!Form methodsFor: 'fileIn/Out' stamp: 'len 8/1/2016 08:13'!
printOn: aStream
	aStream isText
		ifTrue:
			[aStream withAttribute: (TextAnchor new anchoredFormOrMorph: self) do: [aStream nextPut: $*].
			^ self].
	aStream
		nextPutAll: self class name;
		nextPut: $(; print: width;
		nextPut: $x; print: height;
		nextPut: $x; print: depth;
		nextPut: $)! !

