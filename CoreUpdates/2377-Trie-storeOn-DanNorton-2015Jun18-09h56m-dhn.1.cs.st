'From Cuis 4.2 of 25 July 2013 [latest update: #2376] on 18 June 2015 at 10:00:09.365473 am'!

!Trie methodsFor: 'printing' stamp: 'dhn 6/16/2015 17:03'!
storeOn: aStream
	"Refer to the comment in Object|storeOn:."
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self keysAndValuesDo: 
			[:each :val | 
			noneYet
				ifTrue: [noneYet _ false]
				ifFalse: [aStream nextPut: $;].
			aStream 
				nextPutAll: ' at: ';
				store: each;
				nextPutAll: ' put: ';
				store: val].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

