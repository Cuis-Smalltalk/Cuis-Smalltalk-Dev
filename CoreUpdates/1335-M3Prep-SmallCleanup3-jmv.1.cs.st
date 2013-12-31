'From Cuis 4.0 of 3 April 2012 [latest update: #4211] on 12 April 2012 at 9:15:45 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 4/12/2012 09:11'!
align: aPoint1 with: aPoint2
	"Translate by aPoint2 - aPoint1."

	^ self morphPositionInOwner: self morphPositionInOwner + (aPoint2 - aPoint1)! !

!Morph methodsFor: 'geometry' stamp: 'jmv 4/12/2012 09:12'!
bounds: newBounds
	| oldExtent newExtent |

	"remove senders and implementors"
	self flag: #jmvVer2.
		
	oldExtent _ bounds extent.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPositionInWorld: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPositionInWorld: newBounds topLeft; morphExtent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self morphExtent: newExtent; morphPositionInWorld: newBounds topLeft.
	].! !

!Morph methodsFor: 'geometry eToy' stamp: 'jmv 4/12/2012 09:13'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	^bounds center! !

!Morph methodsFor: 'geometry eToy' stamp: 'jmv 4/12/2012 09:13'!
referencePosition: aPoint
	"a rather ugly way to say #center: . Just for consistency with #referencePosition"
	"remove some day"
	self flag: #jmvVer2.
	self morphPositionInWorld: aPoint - (bounds extent // 2)! !


!SketchMorph methodsFor: 'accessing' stamp: 'jmv 4/12/2012 09:14'!
form: aForm
	"Set the receiver's form"

	| oldForm |
	oldForm _ originalForm.
	originalForm _ aForm.
	self basicExtent: originalForm extent.

	oldForm ifNotNil: [ self morphPositionInOwner: self morphPositionInOwner + (oldForm extent - aForm extent // 2) ]! !


!StarMorph methodsFor: 'editing' stamp: 'jmv 4/12/2012 09:14'!
dragVertex: label event: evt fromHandle: handle
	| ext oldR pt center |
	label == #center ifTrue: [
		self morphPositionInOwner: self morphPositionInOwner + (evt eventPosition - handle referencePosition)].

	label == #outside ifTrue: [
		center _ handles first referencePosition.
		pt _ center - evt eventPosition.
		ext _ pt r.
		oldR _ ext.
		vertices _ (0 to: 359 by: (360//vertices size)) collect: [ :angle |
			(Point r: (oldR _ oldR = ext ifTrue: [ext*5//12] ifFalse: [ext])
					degrees: angle + pt degrees)
				+ center].
		handle align: handle referencePosition with: evt eventPosition ].

	self computeBounds.
! !

