'From Cuis 4.2 of 25 July 2013 [latest update: #2794] on 7 June 2016 at 5:53:06.765019 pm'!

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/30/2016 21:32'!
doesNotExist
	"
	Character doesNotExist
	"
	^ self value: 16r83! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/30/2016 21:33'!
exists
	"
	Character exists
	"
	^ self value: 16r82! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/7/2016 00:49'!
times
	"
	Character times
	"
	^ self value: 16rD7! !

!methodRemoval: Character class #thereExists!
Character class removeSelector: #thereExists!
!methodRemoval: Character class #thereDoesNotExists!
Character class removeSelector: #thereDoesNotExists!
