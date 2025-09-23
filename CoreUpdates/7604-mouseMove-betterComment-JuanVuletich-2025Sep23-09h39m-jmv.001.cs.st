'From Cuis7.5 [latest update: #7602] on 23 September 2025 at 9:56:39 am'!

!Morph methodsFor: 'events' stamp: 'jmv 9/23/2025 09:43:39'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event.
	We can query aMouseMoveEvent to know about pressed mouse buttons.
	Many submorphs will only do stuff like
		aMouseMoveEvent anyButtonPressed ifTrue: []."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseMove:localPosition: 
		ifPresentDo: [ :handler | handler value: aMouseMoveEvent value: localEventPosition ]! !

