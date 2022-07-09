'From Cuis 6.0 [latest update: #5382] on 9 July 2022 at 4:49:34 pm'!

!Process methodsFor: 'accessing' stamp: 'jmv 7/9/2022 16:48:40'!
priority: anInteger 
	"Set the receiver's priority to anInteger."
	priority _ anInteger min: Processor highestPriority max: Processor lowestPriority.! !

