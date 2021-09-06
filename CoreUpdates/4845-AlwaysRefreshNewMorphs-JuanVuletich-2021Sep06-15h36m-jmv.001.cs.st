'From Cuis 5.0 [latest update: #4844] on 6 September 2021 at 3:36:59 pm'!

!Morph methodsFor: 'initialization' stamp: 'jmv 9/6/2021 15:21:29'!
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."

	aWorld ifNil: [ ^self ].
	self needsRedraw: true.
	self wantsSteps ifTrue: [ self startStepping ].
	self submorphsDo: [ :m | m intoWorld: aWorld ].! !

