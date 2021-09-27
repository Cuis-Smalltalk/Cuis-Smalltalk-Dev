'From Cuis 5.0 [latest update: #4887] on 27 September 2021 at 9:55:42 am'!

!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 9/27/2021 09:52:51'!
isPureMirroring
	"Return true if the receiver specifies no translation, rotation or scaling, and just mirrors the Y axis."

	^ false! !


!AffineTransformation methodsFor: 'testing' stamp: 'jmv 9/27/2021 09:52:19'!
isPureMirroring
	"Return true if the receiver specifies no translation, rotation or scaling, and just mirrors the Y axis."

	^self a11 = 1.0 and: [ self a12 = 0.0 and: [
	  self a21 = 0.0 and: [ self a22 = -1.0 and: [
	  self a13 = 0.0 and: [ self a23 = 0.0 ]]]]]! !


!AffineTransformation methodsFor: 'testing' stamp: 'jmv 9/27/2021 09:51:11'!
isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."

	<primitive: 'primitiveIsPureTranslation' module: 'Matrix2x3Plugin'>
	^self a11 = 1.0 and: [ self a12 = 0.0 and: [
	  self a21 = 0.0 and: [ self a22 = 1.0 ]]]! !


!MovableMorph methodsFor: 'initialization' stamp: 'jmv 9/27/2021 09:53:27'!
openInWorld: aWorld
	"Add this morph to the requested World."

	location isIdentity
		ifTrue: [						"Identity means default location on creation. Not an actual position to honor."
			aWorld
				addMorph: self
				position: (Display width*7//10) atRandom@(Display height*8//10) atRandom ]
		ifFalse: [
			location isPureMirroring
				ifTrue: [				"But not the identity. Default location on creation if #yAxisPointsUp"
					aWorld
						addMorph: self
						position: (Display width*7//10) atRandom@(Display height*6//10) atRandom + ((Display width //4)@ Display height //3) ]
				ifFalse: [
					aWorld addMorph: self ]]! !

