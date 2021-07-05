'From Cuis 5.0 [latest update: #4651] on 5 July 2021 at 2:38:05 pm'!

!ArrayedCollection methodsFor: 'printing' stamp: 'jmv 7/5/2021 13:26:23'!
printNameOn: aStream

	aStream
		nextPutAll: self class name withArticle;
		nextPut: $:;
		print: self size! !


!Collection methodsFor: 'printing' stamp: 'jmv 7/5/2021 13:24:54'!
printNameOn: aStream

	aStream
		nextPutAll: self class name withArticle! !

