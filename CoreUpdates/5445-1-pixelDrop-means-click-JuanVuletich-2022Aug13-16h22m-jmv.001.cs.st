'From Cuis 6.0 [latest update: #5443] on 13 August 2022 at 4:50:18 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/13/2022 16:34:35'!
invalidDrop: aDropEvent
	"A drag and drop operation was requested, but it was not accepted.
	Some morphs may want to do somthing in this case. Let them."! !


!InnerListMorph methodsFor: 'events-processing' stamp: 'jmv 8/13/2022 16:49:08'!
invalidDrop: aDropEvent
	"A drag and drop operation was requested, but it was not accepted.
	Usually the user just meant to click, and the small drag was unintended."

	| droppedMorph localPosition row |
	droppedMorph _ aDropEvent contents.
	((droppedMorph is: #DraggingGuideMorph) and: [
		(droppedMorph valueOfProperty: #dragSource) == owner]) ifTrue: [
			localPosition _ self internalizeFromWorld: aDropEvent eventPosition.
			row _ self rowAtLocation: localPosition ifNone: [].
			self selectedRow: row.
			aDropEvent wasHandled: true ].! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/13/2022 16:39:04'!
dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."
	| dropped |

	"Try to get out quickly"
	(aMorph fullIncludesPixel: position)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [
			^self ]].

	(aMorph rejectsEvent: self) ifFalse: [
		aMorph allowsMorphDrop ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [ ^ self sendEventTo: aMorph ]].
		aMorph invalidDrop: self.
		wasHandled ifTrue: [
			self contents justDroppedInto: aMorph event: self.
			^self].
	].
	^#rejected! !


!InnerListMorph reorganize!
('initialization' initialize)
('list management' drawBoundsForRow: drawYForRow: highlightedRow: listChanged rowAtLocation: rowAtLocation:ifNone: selectedRow selectedRow:)
('drawing' bottomVisibleRowForCanvas: draw:atRow:on: drawBackgroundForMulti:on: drawHighlightOn: drawOn: drawSelectionOn: font font: topVisibleRowForCanvas:)
('list access' getListItem: getListSize item:)
('scroll range' desiredWidth widthToDisplayItem:)
('private' noSelection)
('geometry' adjustExtent fontPreferenceChanged)
('events-processing' invalidDrop:)
!

