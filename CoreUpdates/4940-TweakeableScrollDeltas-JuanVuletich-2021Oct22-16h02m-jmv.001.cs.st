'From Cuis 5.0 [latest update: #4939] on 22 October 2021 at 4:06:15 pm'!

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/22/2021 16:05:10'!
scrollDeltaHeight: anInteger
	"Set the increment in pixels which this pane should be scrolled."
	self setProperty: #scrollDeltaHeight toValue: anInteger.
	self vSetScrollDelta.! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/22/2021 16:05:00'!
scrollDeltaWidth: anInteger
	"Set the increment in pixels which this pane should be scrolled."
	self setProperty: #scrollDeltaWidth toValue: anInteger.
	self hSetScrollDelta.! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/22/2021 16:02:19'!
scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."
	^ self valueOfProperty: #scrollDeltaHeight ifAbsent: [10]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/22/2021 16:02:36'!
scrollDeltaWidth
	"Return the increment in pixels which this pane should be scrolled."
	^ self valueOfProperty: #scrollDeltaWidth ifAbsent: [10]! !

