'From Cuis 4.1 of 12 December 2012 [latest update: #1513] on 17 December 2012 at 1:00:50 pm'!

!Taskbar methodsFor: 'testing' stamp: 'jmv 12/17/2012 13:00'!
isSticky
	"answer whether the receiver is Sticky"
	^true! !


!Taskbar reorganize!
('change reporting' delete)
('commands' minimize: restore: restoreAll)
('initialization' defaultColor)
('stepping' step stepTime wantsSteps)
('testing' isSticky)
!

