'From Cuis 6.0 [latest update: #5645] on 22 January 2023 at 2:00:43 pm'!

!MethodContext methodsFor: 'private' stamp: 'jar 1/21/2023 16:23:41'!
endpc
	^closureOrNil
		ifNil:	[self method endPC]
		ifNotNil: [closureOrNil endpc]! !

