'From Cuis 5.0 [latest update: #3838] on 6 August 2019 at 8:37:30 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/6/2019 08:36:15'!
setBalloonText: stringTextOrSymbol
	"Set receiver's balloon help text. Pass nil to remove the help."

	stringTextOrSymbol
		ifNil: [ self removeProperty: #balloonText ]
		ifNotNil: [
			self
				setProperty: #balloonText
				toValue: stringTextOrSymbol string ].! !

