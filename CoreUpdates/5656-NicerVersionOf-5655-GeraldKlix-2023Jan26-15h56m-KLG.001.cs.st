'From Cuis 6.0 [latest update: #5655] on 26 January 2023 at 4:03:29 pm'!

!LabelMorph methodsFor: 'accessing' stamp: 'KLG 1/26/2023 15:56:42'!
label
	"Answer the name to show in a list of windows-and-morphs to represent the receiver"

	^contents! !


!MenuMorph methodsFor: 'accessing' stamp: 'KLG 1/26/2023 16:02:55'!
label
	"Answer a nice label.
	
	Pinned menus in the taskbar are easier to identify on big screens."

	titleMorph ifNotNil: [
		titleMorph submorphsDo: [ :labelMorph |
			^ labelMorph contents ].
		^super label ].
	"One item menu, copied by shift right click, use the item's content:"
	submorphs size = 1 ifTrue: [
		^ submorphs first label ].
	^ super label! !

!methodRemoval: MenuItemMorph #label stamp: 'KLG 1/26/2023 15:56:49'!
MenuItemMorph removeSelector: #label!
