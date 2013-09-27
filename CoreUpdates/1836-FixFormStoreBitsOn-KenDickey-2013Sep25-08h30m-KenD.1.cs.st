'From Cuis 4.2 of 25 July 2013 [latest update: #1835] on 25 September 2013 at 8:30:55.555506 am'!

!Form methodsFor: 'fileIn/Out' stamp: 'KenD 9/22/2013 18:45'!
storeBitsOn: aStream base: anInteger
	bits do: [:word | 
		anInteger = 10
			ifTrue: [aStream space]
			ifFalse: [aStream newLineTab: 2].
		anInteger = 2
			ifTrue: [ 
				aStream nextPut: $2. 
				aStream nextPut: $r. 
				word printOn: aStream base: 2 length: 16 padded: true
			]
			ifFalse: [
				word printOn: aStream base: anInteger
			].
	].! !

