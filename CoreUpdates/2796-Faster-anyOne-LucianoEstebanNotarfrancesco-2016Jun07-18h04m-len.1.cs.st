'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 6:06:24.879342 pm'!

!Collection methodsFor: 'accessing' stamp: 'len 2/23/2016 23:02'!
anyOne
	"Answer any element in the receiver."

	self do: [:each | ^ each].
	self errorEmptyCollection! !

