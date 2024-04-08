'From Cuis6.3 [latest update: #6313] on 8 April 2024 at 9:52:34 am'!

!Sample04Pacman methodsFor: 'updating' stamp: 'jmv 4/8/2024 09:50:51'!
redrawNeeded
	"for experimenting with collision detection"

	self isOwnedByHand ifTrue: [
		self world ifNotNil: [ :w |
			w submorphsDo: [ :m |
				"In #redrawNeeded, we can't call #delete on a dirty SystemWindow,
				as the confirmation menu can't be opened."
				(((m is: #SystemWindow) not or: [ m canDiscardEdits ]) and: [
						self collides: m]) ifTrue: [
					m class == self class
						ifTrue: [ m colorize ]
						ifFalse: [ m delete ]]]]].
	super redrawNeeded.! !

