'From Cuis7.5 [latest update: #7489] on 2 September 2025 at 5:42:14 pm'!

!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 17:41:42'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| handlerMorph dropped |
	handlerMorph := aWorldMorph morphVisibleAt: position.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph includesPixel: position]) ifTrue: [
			
			handlerMorph allowsMorphDrop ifTrue: [
				"Do a symmetric check if both morphs like each other"
				dropped := self contents.
				((handlerMorph wantsDroppedMorph: dropped event: self)		"I want her"
					and: [dropped wantsToBeDroppedInto: handlerMorph])		"she wants me"
						ifTrue: [ self sendEventTo: handlerMorph ]].
			wasHandled
				ifFalse: [
					handlerMorph invalidDrop: self.
					(self contents is: #DraggingGuideMorph) ifTrue: [
						self wasHandled: true.
						self contents delete ]
					]
			
		].
		handlerMorph := handlerMorph owner.
	].! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 17:35:40'!
startDropEventDispatch: aDropEvent

	aDropEvent dispatchIn: owner.
	self mouseOverHandler processMouseOver: lastMouseEvent! !


!DraggingGuideMorph methodsFor: 'drawing' stamp: 'jmv 9/2/2025 17:11:06'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self localBounds
		color: (Theme current listHighlightFocused: true).! !

