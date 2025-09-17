'From Cuis7.5 [latest update: #7583] on 17 September 2025 at 5:46:10 pm'!

!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/17/2025 17:33:30'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| topUnlockedMorph handlerMorph dropped |
	topUnlockedMorph := aWorldMorph topUnlockedMorphAt: position.
	handlerMorph := topUnlockedMorph.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			handlerMorph allowsMorphDrop ifTrue: [
				"Do a symmetric check if both morphs like each other"
				dropped := self contents.
				((handlerMorph wantsDroppedMorph: dropped event: self)		"I want her"
					and: [dropped wantsToBeDroppedInto: handlerMorph])		"she wants me"
						ifTrue: [ self sendEventTo: handlerMorph ]].
		].
		handlerMorph := handlerMorph owner.
	].
	wasHandled
		ifFalse: [
			topUnlockedMorph invalidDrop: self.
			(self contents is: #DraggingGuideMorph) ifTrue: [
				self wasHandled: true.
				self contents delete ]
			].! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 9/17/2025 17:30:07'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, 
	 which is being dropped by a hand in response to the given event. 
	Note that for a successful drop operation both parties need to agree. 
	The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.
	 Individual Morpks may override by setting the corresponding property
	 to an appropriate two argument closure."

	"DraggingGuideMorph is a bit special. It is usually used to D&D from one list to another.
	Any Morph willing to deal with them should redefine this method."
	(aMorph is: #DraggingGuideMorph) ifTrue: [
		^false ].

	^self valueOfProperty: #wantsDroppedMorph:event:
		 ifPresentDo: [ :wantsMorphEvt | wantsMorphEvt value: aMorph value: evt ]
		 ifAbsent: [ true ]! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/17/2025 17:41:21'!
dragEvent: aMouseEvent localPosition: localEventPosition
	| row listItem label draggingGuide |
	scroller highlightedRow: nil.
	row := self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem := self getListItem: row.
	
	label := LabelMorph contents: listItem.
	draggingGuide := DraggingGuideMorph new.
	draggingGuide addMorph: label.
	draggingGuide morphExtent: label morphExtent.
	draggingGuide setProperty: #dragSource toValue: self.
	draggingGuide setProperty: #dropSelectorArgument toValue: listItem.
	draggingGuide setProperty: #dragLocalPos toValue: localEventPosition.
	
	aMouseEvent hand attachMorphBeside: draggingGuide! !

!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 9/17/2025 17:42:30'!
invalidDrop: aDropEvent
	"A drag and drop operation was requested, but it was not accepted.
	Usually the user just meant to click, and the small drag was unintended."

	| droppedMorph localEventPosition |
	droppedMorph := aDropEvent contents.
	((droppedMorph is: #DraggingGuideMorph) and: [
		(droppedMorph valueOfProperty: #dragSource) == self]) ifTrue: [
			localEventPosition := droppedMorph valueOfProperty: #dragLocalPos.
			self clickedAt: localEventPosition maybeDeselect: true.
			aDropEvent wasHandled: true ].! !

