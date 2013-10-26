'From Cuis 4.0 of 3 April 2012 [latest update: #1246] on 5 April 2012 at 9:52:36 am'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 09:52'!
highestUpdate
	highestUpdate ifNil: [
		highestUpdate _ updates
			ifEmpty: [ 0 ]
			ifNotEmpty: [
				updates detectMax: [ :updateName | updateName initialIntegerOrNil ]]].
	^ highestUpdate initialIntegerOrNil! !

