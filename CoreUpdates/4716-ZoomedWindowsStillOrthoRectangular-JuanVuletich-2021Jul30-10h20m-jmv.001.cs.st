'From Cuis 5.0 [latest update: #4715] on 30 July 2021 at 10:29:05 am'!

!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 7/30/2021 10:24:34'!
doesNotRotate
	"Return true if the receiver specifies no rotation."
	^false! !


!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/30/2021 10:25:56'!
isOrthoRectangularMorph
	"True if we are of rectangular shape, aligned with local coordinate axes.
	Doesn't take into account possible rotation of our or some owner's local coordinates.
	Note: SystemWindow answers true, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^location doesNotRotate and: [ owner isNil or: [ owner isOrthoRectangularMorph ]].! !

