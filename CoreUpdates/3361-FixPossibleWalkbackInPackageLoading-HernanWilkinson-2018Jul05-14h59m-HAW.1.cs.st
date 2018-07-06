'From Cuis 5.0 of 7 November 2016 [latest update: #3345] on 5 July 2018 at 2:59:23 pm'!

!CodePackage methodsFor: 'accessing' stamp: 'HAW 7/5/2018 14:59:10'!
hasUnsavedChanges

	"Might be nil and breaks when a code package window is open and loading packages - Hernan
	This is not a lazy initialization, the variable is set to non nil value only when certain."
	^hasUnsavedChanges = true! !

