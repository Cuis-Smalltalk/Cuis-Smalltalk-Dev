'From Cuis 6.0 [latest update: #6065] on 30 October 2023 at 2:32:38 pm'!

!Dictionary methodsFor: 'printing' stamp: 'jmv 10/30/2023 14:27:00'!
printElementsOn: aStream
	aStream nextPut: $(.
	self keysDo: [ :key |
		aStream print: key; nextPutAll: '->'; print: (self at: key); space ].
	aStream nextPut: $)! !

!Dictionary methodsFor: 'printing' stamp: 'jmv 10/30/2023 14:27:47'!
storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self keysDo: [ :key | 
			noneYet
				ifTrue: [noneYet := false]
				ifFalse: [aStream nextPut: $;].
			aStream nextPutAll: ' add: '.
			aStream store: (self associationAt: key)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

