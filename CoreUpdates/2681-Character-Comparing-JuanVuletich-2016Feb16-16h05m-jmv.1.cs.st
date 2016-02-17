'From Cuis 4.2 of 25 July 2013 [latest update: #2679] on 16 February 2016 at 4:07:10.269041 pm'!

!Character methodsFor: 'comparing' stamp: 'jmv 2/16/2016 16:06'!
<= aCharacter 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aCharacter) not! !

!Character methodsFor: 'comparing' stamp: 'jmv 2/16/2016 16:06'!
>= aCharacter 
	"Answer whether the receiver is greater than or equal to the argument."

	^aCharacter <= self! !

