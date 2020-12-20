'From Cuis 5.0 [latest update: #4490] on 20 December 2020 at 11:30:12 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'KLG 12/19/2020 13:17:06'!
setBalloonText: stringTextOrSymbol
	"Set receiver's balloon help text. Pass nil to remove the help."

	stringTextOrSymbol
		ifNil: [ self removeProperty: #balloonText ]
		ifNotNil: [
			self
				setProperty: #balloonText
				toValue: stringTextOrSymbol ].! !

