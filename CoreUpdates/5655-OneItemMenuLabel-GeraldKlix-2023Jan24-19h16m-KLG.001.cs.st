'From Cuis 6.0 [latest update: #5654] on 24 January 2023 at 7:18:54 pm'!

!MenuItemMorph methodsFor: 'accessing' stamp: 'KLG 1/24/2023 19:18:06'!
label
	"Answer the name to show in a list of windows-and-morphs to represent the receiver"

	^contents! !


!MenuMorph methodsFor: 'accessing' stamp: 'KLG 1/24/2023 13:25:13'!
label
	"Answer a nice label.
	
	Pinned menus in the taskbar are easier to identify on big screens."

	titleMorph ifNil: [
		"One item menu, copied by shift right click, use the item's content:"
		submorphs size = 1 ifFalse: [ ^ super label ].
		^ submorphs anyOne label ].
	titleMorph submorphsDo: [ :labelMorph |
		"Be careful"
		[ ^ labelMorph contents ] onDNU: #contents do: [] ].
	^ super label ":] One never knows"! !

