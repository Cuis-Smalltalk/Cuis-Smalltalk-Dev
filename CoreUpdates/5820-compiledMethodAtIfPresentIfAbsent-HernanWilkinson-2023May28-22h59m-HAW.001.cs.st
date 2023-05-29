'From Cuis 6.0 [latest update: #5819] on 28 May 2023 at 11:00:40 pm'!

!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 5/28/2023 22:59:44'!
compiledMethodAt: aSelector ifPresent: aFoundBlock ifAbsent: anAbsentBlock 
	
	^self methodDict at: aSelector ifPresent: aFoundBlock ifAbsent: anAbsentBlock.! !

