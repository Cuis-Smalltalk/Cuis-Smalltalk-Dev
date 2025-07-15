'From Cuis7.5 [latest update: #7353] on 15 July 2025 at 10:15:30 am'!

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 7/15/2025 09:11:32'!
initSortBlocks

	stepList := Heap sortBlock: self stepListSortBlock.
	alarms := Heap sortBlock: self alarmSortBlock.! !

