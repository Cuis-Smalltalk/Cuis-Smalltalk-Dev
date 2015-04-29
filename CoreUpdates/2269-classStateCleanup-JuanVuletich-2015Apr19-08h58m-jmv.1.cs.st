'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 8:59:43.899912 am'!

!Clipboard class methodsFor: 'cached state access' stamp: 'jmv 4/19/2015 08:59'!
releaseClassCachedState
	Default _ nil! !


!Clipboard class reorganize!
('accessing' chooseRecentClipping default default:)
('default clipboard' retrieveMorph retrieveObject retrieveStringOrText storeObject:)
('cached state access' releaseClassCachedState)
!

