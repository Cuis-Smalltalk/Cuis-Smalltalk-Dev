'From Cuis7.5 [latest update: #7597] on 22 September 2025 at 9:46:05 am'!

!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/22/2025 09:46:00'!
dispatchIn: aWorldMorph
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| topUnlockedMorph handlerMorph dropped |
	topUnlockedMorph := aWorldMorph topUnlockedMorphAt: position.
	handlerMorph := topUnlockedMorph.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
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
			topUnlockedMorph ifNotNil: [ topUnlockedMorph invalidDrop: self ].
			(self contents is: #DraggingGuideMorph) ifTrue: [
				self wasHandled: true.
				self contents delete ]].! !

