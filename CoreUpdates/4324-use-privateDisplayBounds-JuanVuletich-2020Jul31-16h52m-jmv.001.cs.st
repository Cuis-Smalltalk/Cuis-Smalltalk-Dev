'From Cuis 5.0 [latest update: #4322] on 31 July 2020 at 4:53:32 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/31/2020 16:53:06'!
displayBounds
	"At time of last draw. nil if unknown."

	self visible ifFalse: [ ^nil ].
	^ privateDisplayBounds! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/31/2020 16:53:12'!
displayBounds: aRectangle

	self displayBounds ~= aRectangle ifTrue: [
		privateDisplayBounds _ aRectangle ].! !

