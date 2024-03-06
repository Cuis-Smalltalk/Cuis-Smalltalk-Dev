'From Cuis6.3 [latest update: #6253] on 6 March 2024 at 11:08:43 am'!

!Sample04Pacman methodsFor: 'updating' stamp: 'jmv 3/6/2024 11:08:05'!
redrawNeeded
	"for experimenting with collision detection"

	self isOwnedByHand ifTrue: [
		self world ifNotNil: [ :w |
			w submorphsDo: [ :m |
				((m is: #SystemWindow) not and: [self collides: m]) ifTrue: [
					m class == self class
						ifTrue: [ m colorize ]
						ifFalse: [ m delete ]]]]].
	super redrawNeeded.! !

