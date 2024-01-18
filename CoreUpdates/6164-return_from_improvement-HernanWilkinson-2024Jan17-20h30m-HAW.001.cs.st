'From Cuis6.3 [latest update: #6163] on 17 January 2024 at 8:31:33 pm'!

!ContextPart methodsFor: 'instruction decoding' stamp: 'HAW 1/17/2024 20:30:55'!
return: value from: aSender 
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop |
	newTop := aSender sender.
	(aSender isDead or: [newTop isNil or: [newTop isDead]]) ifTrue: [
		^ self pc: nil; send: #cannotReturn: to: self with: {value} super: false].
	(self findNextUnwindContextUpTo: newTop) ifNotNil: [
		^ self send: #aboutToReturn:through: to: self with: {value. nil} super: false].
	self releaseTo: newTop.
	newTop ifNotNil: [newTop push: value].
	^ newTop! !

