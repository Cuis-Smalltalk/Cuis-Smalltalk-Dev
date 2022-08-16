'From Cuis 6.0 [latest update: #5450] on 16 August 2022 at 11:57:10 am'!

!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 8/16/2022 11:50:48'!
clickedAt: localEventPosition maybeDeselect: allowDeselecting
	"The mouse came up within the list; take appropriate action"
	"No change if model is locked"

	| row |
	row _ self rowAtLocation: localEventPosition.
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [ ^ self ]].
	(autoDeselect == false and:  [row = 0 ]) ifTrue: [ ^ self ].
	(autoDeselect and: [ row == self visualSelectionIndex ])
		ifTrue: [
			allowDeselecting ifTrue: [
				self setSelectionIndex: 0 ]]
		ifFalse: [ self setSelectionIndex: row ]! !

!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 8/16/2022 11:51:54'!
invalidDrop: aDropEvent
	"A drag and drop operation was requested, but it was not accepted.
	Usually the user just meant to click, and the small drag was unintended."

	| droppedMorph localEventPosition |
	droppedMorph _ aDropEvent contents.
	((droppedMorph is: #DraggingGuideMorph) and: [
		(droppedMorph valueOfProperty: #dragSource) == self]) ifTrue: [
			localEventPosition _ self internalizeFromWorld: aDropEvent eventPosition.
			self clickedAt: localEventPosition maybeDeselect: true.
			aDropEvent wasHandled: true ].! !


!InnerListMorph methodsFor: 'events-processing' stamp: 'jmv 8/16/2022 11:52:56'!
invalidDrop: aDropEvent
	"A drag and drop operation was requested, but it was not accepted.
	Usually the user just meant to click, and the small drag was unintended."

	owner invalidDrop: aDropEvent.! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/16/2022 11:51:04'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	self clickedAt: localEventPosition maybeDeselect: aMouseButtonEvent mouseButton1Changed! !

