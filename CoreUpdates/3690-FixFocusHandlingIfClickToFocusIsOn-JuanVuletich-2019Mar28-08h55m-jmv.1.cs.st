'From Cuis 5.0 of 7 November 2016 [latest update: #3689] on 28 March 2019 at 8:56:04 am'!

!Morph methodsFor: 'events' stamp: 'jmv 3/28/2019 08:54:29'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."
	Preferences focusFollowsMouse
		ifTrue: [evt hand releaseKeyboardFocus: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: evt ].! !

