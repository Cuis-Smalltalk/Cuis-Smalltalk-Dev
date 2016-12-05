'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 26 November 2016 at 9:02:48 am'!

!SystemDictionary methodsFor: 'browsing' stamp: 'len 6/9/2016 23:23'!
browseAllPrimitives
	self browseAllSelect: [:each| each primitive ~= 0 and: [(each primitive between: 256 and: 291) not]]
! !

