'From Cuis7.3 [latest update: #6899] on 9 December 2024 at 1:07:59 pm'!

!Interval methodsFor: 'testing' stamp: 'HAW 11/2/2024 10:50:17'!
intersectsRange: rightRange
 
	^(self includes: rightRange first) 
		or: [ (self includes: rightRange last)
		or: [ rightRange includes: self first ]]! !

