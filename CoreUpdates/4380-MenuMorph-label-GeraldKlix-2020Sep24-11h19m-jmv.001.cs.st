'From Cuis 5.0 [latest update: #4379] on 24 September 2020 at 11:22:55 am'!

!MenuMorph methodsFor: 'accessing' stamp: 'KLG 9/22/2020 20:59:54'!
label
	"Answer a nice label.
	
	Pinned menus in the taskbar are easier to identify on big screens."

	titleMorph ifNil: [ ^ super label ].
	titleMorph submorphsDo: [ :stringMorph |
		"Be careful"
		[ ^ stringMorph contents ] onDNU: #contents do: [] ].
	^ super label ":] One never knows"! !

