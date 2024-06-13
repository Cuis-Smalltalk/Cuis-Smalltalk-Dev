'From Cuis7.1 [latest update: #6466] on 13 June 2024 at 12:00:21 pm'!

!Class methodsFor: 'accessing class hierarchy' stamp: 'jmv 6/13/2024 11:57:18'!
superclass: aClass 
	"Change the receiver's superclass to be aClass.
	Keep the subclass / superclass invariant.
	Not to be used lightly!!
	Usually used only to create subclasses of nil."

	| oldSuperclass |
	oldSuperclass := superclass.
	super superclass: aClass.
	(oldSuperclass notNil and: [ oldSuperclass ~~ superclass ]) ifTrue: [
		oldSuperclass removeSubclass: self ].! !

