'From Cuis7.1 [latest update: #6543] on 28 July 2024 at 7:18:11 pm'!

!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 12/30/2021 13:11:33'!
addedMorph: aMorph
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]! !

