'From Cuis 5.0 of 7 November 2016 [latest update: #3765] on 24 May 2019 at 7:25:25 pm'!

!PluggableListMorph methodsFor: 'events' stamp: 'len 5/24/2019 19:23:59'!
mouseLeave: event
	super mouseLeave: event.
	self listMorph highlightedRow: nil! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'len 5/24/2019 19:10:11'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	self listMorph highlightedRow: (self rowAtLocation: localEventPosition ifNone: []).
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !

