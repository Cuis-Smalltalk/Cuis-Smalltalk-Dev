'From Cuis 4.2 of 25 July 2013 [latest update: #2464] on 30 August 2015 at 8:19:41.112488 pm'!

!Object class methodsFor: 'package support' stamp: 'jmv 8/30/2015 20:18'!
package
	"
	Object package
	AtomMorph package
	
	AtomMorph package ifNotNil: [ :pck | pck fullFileName ]
	"
	^ CodePackage packageOfClass: self ifNone: nil! !

