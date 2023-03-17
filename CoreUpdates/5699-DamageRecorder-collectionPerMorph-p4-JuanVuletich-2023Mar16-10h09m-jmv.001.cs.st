'From Cuis 6.0 [latest update: #5696] on 16 March 2023 at 10:19:25 am'!

!DamageRecorder methodsFor: 'private' stamp: 'jmv 3/16/2023 10:18:42'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."

	aRootMorph ifNotNil: [
		(damageByRoot at: aRootMorph
			ifPresent: [ :c | |r|
				r := requestedRect copy. "asIntegerRectangle? encompassing?"
				1 to: c size do: [ :i | |old|
					old := c at: i.
					(r intersects: old) ifTrue: [
						r updateMerging: old.
						c at: i put: nil ]].
				c removeAllSuchThat: [:e | e isNil ].
				c add: r ]
			ifAbsent: [ damageByRoot at: aRootMorph put: (OrderedCollection with: requestedRect copy) ]) ]
		ifNil: [otherDamage add: requestedRect copy].! !

