'From Cuis7.7 [latest update: #7955] on 21 May 2026 at 4:07:03 pm'!

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 21/May/2026 16:06:39'!
cancel
	response := ''.
	self delete.
	cancelBlock ifNotNil: [cancelBlock value].! !

