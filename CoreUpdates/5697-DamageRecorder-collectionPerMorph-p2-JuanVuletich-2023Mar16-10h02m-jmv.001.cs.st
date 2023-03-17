'From Cuis 6.0 [latest update: #5696] on 16 March 2023 at 10:06:38 am'!

!DamageRecorder methodsFor: 'private' stamp: 'jmv 3/16/2023 10:02:42'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."

	aRootMorph ifNotNil: [
		(damageByRoot at: aRootMorph
			ifPresent: [ :c |
				c isCollection
					ifTrue: [c add: requestedRect copy]
					ifFalse: [damageByRoot at: aRootMorph put: 
						(OrderedCollection with: c with: requestedRect copy)]]
			ifAbsent: [ damageByRoot at: aRootMorph put: (OrderedCollection with: requestedRect copy) ]) ]
		ifNil: [otherDamage add: requestedRect copy].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
self runningWorld resetCanvas.!

