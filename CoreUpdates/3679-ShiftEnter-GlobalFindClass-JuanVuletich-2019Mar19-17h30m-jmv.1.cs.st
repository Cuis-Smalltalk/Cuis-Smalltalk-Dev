'From Cuis 5.0 of 7 November 2016 [latest update: #3678] on 19 March 2019 at 5:34:42 pm'!

!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 3/19/2019 17:34:09'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		(self shiftPressed and: [self keyValue = 13 ]) ifTrue: [
			^BrowserWindow findClass].
		(self commandAltKeyPressed or: [ self controlKeyPressed ])
			ifTrue: [
				self keyValue = $w numericValue ifTrue: [
					aMorph owningWindow ifNotNil: [ :w |
						(w morphContainsPoint: (w internalizeFromWorld: self eventPosition)) ifTrue: [
							w delete. 
							^self ]]]].
		^ aMorph
			processKeystroke: self
			localPosition: positionInAMorph ].
	type == #keyDown ifTrue: [
		^ aMorph
			processKeyDown: self
			localPosition: positionInAMorph ].
	type == #keyUp ifTrue: [ 
		^ aMorph
			processKeyUp: self
			localPosition: positionInAMorph ].
	^ super
		sentTo: aMorph
		localPosition: positionInAMorph.! !

