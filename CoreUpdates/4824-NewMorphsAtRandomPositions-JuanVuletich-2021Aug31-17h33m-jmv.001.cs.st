'From Cuis 5.0 [latest update: #4822] on 31 August 2021 at 5:36:52 pm'!

!MovableMorph methodsFor: 'initialization' stamp: 'jmv 8/31/2021 17:11:29'!
openInWorld: aWorld
	"Add this morph to the requested World."
	location isIdentity
		ifTrue: [
			aWorld
				addMorph: self
				position: (Display width*8//10) atRandom@(Display height*8//10) atRandom + (Display extent //10) ]
		ifFalse: [ aWorld addMorph: self ]! !

