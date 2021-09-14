'From Cuis 5.0 [latest update: #4856] on 13 September 2021 at 3:51:30 pm'!

!ScrollBar methodsFor: 'events' stamp: 'jmv 9/13/2021 15:51:20'!
mouseStillDown

	nextPageDirection notNil ifTrue: [
		self scrollByPage ]! !

