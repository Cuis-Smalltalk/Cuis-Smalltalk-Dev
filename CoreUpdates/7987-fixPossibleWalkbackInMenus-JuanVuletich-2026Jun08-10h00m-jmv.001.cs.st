'From Cuis7.9 [latest update: #7986] on 8 June 2026 at 10:01:18 am'!

!MenuItemMorph methodsFor: 'event handling' stamp: 'jmv 8/Jun/2026 10:00:17'!
forgiveDwellExpired
	"Dwell timer fired: cursor stayed on this item long enough to clearly mean it.
	Clear the parent menu's forgive triangle and select this item normally."
	owner ifNotNil: [
		owner isInWorld ifTrue: [
			owner clearForgive.
			owner selectItem: self ]].! !

