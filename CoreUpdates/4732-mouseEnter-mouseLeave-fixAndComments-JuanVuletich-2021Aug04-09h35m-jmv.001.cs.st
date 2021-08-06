'From Cuis 5.0 [latest update: #4731] on 4 August 2021 at 9:37:48 am'!

!Morph methodsFor: 'events' stamp: 'jmv 8/4/2021 09:37:23'!
mouseEnter: evt
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseEnter: 
		ifPresentDo: [ :handler | handler value: evt ]! !

!Morph methodsFor: 'events' stamp: 'jmv 8/4/2021 09:37:07'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	Preferences focusFollowsMouse
		ifTrue: [evt hand releaseKeyboardFocus: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: evt ].! !


!IndentingListItemMorph methodsFor: 'events' stamp: 'jmv 8/4/2021 09:35:18'!
mouseLeave: event
	isHighlighted _ false.
	self redrawNeeded.
	^super mouseLeave: event! !

