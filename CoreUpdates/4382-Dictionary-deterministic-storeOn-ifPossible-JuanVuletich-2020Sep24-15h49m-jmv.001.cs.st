'From Cuis 5.0 [latest update: #4381] on 24 September 2020 at 3:51:28 pm'!

!Dictionary methodsFor: 'printing' stamp: 'jmv 9/24/2020 15:49:52'!
storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self keysSortedSafely do: [ :key | 
			noneYet
				ifTrue: [noneYet _ false]
				ifFalse: [aStream nextPut: $;].
			aStream nextPutAll: ' add: '.
			aStream store: (self associationAt: key)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

