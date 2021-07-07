'From Cuis 5.0 [latest update: #4656] on 6 July 2021 at 5:21:09 pm'!

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 7/6/2021 17:18:31'!
mouseFocus

	mouseFocus ifNotNil: [
		(mouseFocus isWorldMorph or: [mouseFocus isInWorld not])
			ifTrue: [ mouseFocus _ nil ]].
	^mouseFocus! !

