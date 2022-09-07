'From Cuis 6.0 [latest update: #5473] on 7 September 2022 at 9:26:37 am'!

!UserInputEvent methodsFor: 'printing' stamp: 'jmv 9/7/2022 09:17:45'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		print: buttons;
		nextPut: $)! !

