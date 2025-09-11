'From Cuis7.5 [latest update: #7556] on 11 September 2025 at 1:03:33 pm'!

!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/11/2025 13:03:31'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| handlerMorph dropped |
	handlerMorph := aWorldMorph topUnlockedMorphAt: position.
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

