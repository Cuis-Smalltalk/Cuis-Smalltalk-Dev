'From Cuis 5.0 [latest update: #4275] on 22 July 2020 at 3:53:04 pm'!

!Morph methodsFor: 'updating' stamp: 'jmv 7/22/2020 15:51:59'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
"BTW, no method should call #redrawNeeded twice. Just once, before any updates."
	"these properties... better store flags in 'id' "
	self setProperty: #needsRedraw toValue: true.
	self allOwnersDo: [ :m | m setProperty: #submorphNeedsRedraw toValue: true ].
	self displayFullBounds
		ifNil: [
			"Dummy, so #updateIsNeeded answers true if some morph dirty"
			"Check if this is really needed. If so, find a cleaner way."
			self invalidateDisplayRect: (10@10 extent: 2@2) from: nil ]
		ifNotNil: [ :r |
			self invalidateDisplayRect: r from: nil ].

"hacer esto en todos los casos? o solo si invalido r?"
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !

!methodRemoval: Morph #isRedrawNeeded stamp: 'jmv 7/22/2020 15:52:55'!
Morph removeSelector: #isRedrawNeeded!
