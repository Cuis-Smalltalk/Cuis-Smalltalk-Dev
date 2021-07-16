'From Cuis 5.0 [latest update: #4673] on 16 July 2021 at 7:47:04 pm'!

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/16/2021 19:44:54'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If false, we occupy a Rectangle in local coordinates. Allows for many optimizations to be done.
	Note: If answer is false, but some owner requiresVectorCanvas, then we also do. Canvas takes care of this.
	Note: SystemWindow answers false, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^location isPureTranslation not or: [ owner notNil and: [ owner requiresVectorCanvas ]].! !

