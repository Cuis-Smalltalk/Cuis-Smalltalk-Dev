'From Cuis7.5 [latest update: #7570] on 15 September 2025 at 1:17:49 pm'!

!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 9/15/2025 13:17:03'!
dispatchIn: aWorldMorph
	"Host window events do not have a position and are only dispatched to the World"

	self wasHandled not ifTrue: [
		(aWorldMorph wantsWindowEvent: self) ifTrue: [
			self sendEventTo: aWorldMorph ]].! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/15/2025 13:17:19'!
processWindowEvent: aWindowEvent
	"Handle an event concerning our host OS window"

	aWindowEvent wasHandled: true.
	self windowEvent: aWindowEvent.
! !

