'From Cuis 4.0 of 3 April 2012 [latest update: #1251] on 5 April 2012 at 12:03:48 pm'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 12:03'!
highestUpdate
	highestUpdate ifNil: [
		highestUpdate _ updates
			ifEmpty: [ 0 ]
			ifNotEmpty: [
				(updates detectMax: [ :updateName | updateName initialIntegerOrNil ])
					initialIntegerOrNil]].
	^ highestUpdate! !

