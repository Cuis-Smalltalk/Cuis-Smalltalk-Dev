'From Cuis7.5 [latest update: #7530] on 9 September 2025 at 8:25:40 am'!

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/8/2025 15:47:53'!
mouseEnter: aMouseEnterEvent
	"The mouse entered the receiver"
	super mouseEnter: aMouseEnterEvent.
	owner ifNotNil: [
		owner selectItem: self ]! !


!IndentingListItemMorph methodsFor: 'events' stamp: 'jmv 9/8/2025 15:48:07'!
mouseEnter: aMouseEnterEvent
	isHighlighted := true.
	self redrawNeeded.
	super mouseEnter: aMouseEnterEvent! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/8/2025 15:48:10'!
mouseEnter: aMouseEnterEvent
	"The mouse entered the receiver"
	mouseIsOver := true.
	self redrawNeeded.
	super mouseEnter: aMouseEnterEvent! !


!HoverableButtonMorph methodsFor: 'events' stamp: 'jmv 9/8/2025 15:48:13'!
mouseEnter: aMouseEnterEvent
	
	mouseEnterSelector ifNotNil: [ model perform: mouseEnterSelector ].
	super mouseEnter: aMouseEnterEvent! !

