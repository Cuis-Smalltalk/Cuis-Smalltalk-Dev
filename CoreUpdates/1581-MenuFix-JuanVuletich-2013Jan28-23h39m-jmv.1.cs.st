'From Cuis 4.1 of 12 December 2012 [latest update: #1580] on 28 January 2013 at 11:41:27 pm'!

!MenuMorph methodsFor: 'events' stamp: 'jmv 1/27/2013 00:54'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		"mejor en un lugar que no dependa de esto... es una porqueria"
		aMouseButtonEvent hand newMouseFocus: self ]! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 1/27/2013 00:50'!
mouseButton2Activity
	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]! !


!PluggableScrollPane methodsFor: 'scroll bar events' stamp: 'jmv 1/27/2013 00:51'!
mouseButton2Activity
	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]! !


!TranscriptMorph methodsFor: 'menus' stamp: 'jmv 1/27/2013 00:51'!
mouseButton2Activity

	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]! !

