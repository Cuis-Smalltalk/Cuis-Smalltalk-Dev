'From Cuis 4.0 of 3 April 2012 [latest update: #1252] on 5 April 2012 at 12:40:11 pm'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 12:40'!
highestUpdate
	highestUpdate ifNil: [
		highestUpdate _ updates
			ifEmpty: [ 0 ]
			ifNotEmpty: [
				(updates detectMax: [ :updateName | updateName initialIntegerOrNil ifNil: [0]])
					initialIntegerOrNil ifNil: [0]]].
	^ highestUpdate! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 12:39'!
registerUpdate: changeSetOrPackageName

	changeSetOrPackageName initialIntegerOrNil ifNotNil: [ :number |
		highestUpdate _ self highestUpdate max: number ].
	updates add: changeSetOrPackageName! !

