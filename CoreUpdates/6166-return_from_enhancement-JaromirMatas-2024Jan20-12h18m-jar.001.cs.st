'From Cuis6.3 [latest update: #6165] on 20 January 2024 at 12:19:48 pm'!

!ContextPart methodsFor: 'instruction decoding' stamp: 'jar 1/18/2024 17:52:20'!
return: value from: aSender 
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop |
	newTop := aSender sender.
	(aSender isDead or: [newTop isNil or: [newTop isDead]]) ifTrue: [
		^ self pc: nil; send: #cannotReturn: to: self with: {value} super: false].
	(self findNextUnwindContextUpTo: newTop) ifNotNil: [
		^ self send: #aboutToReturn:through: to: self with: {value. nil} super: false].
	self releaseTo: newTop.
	^ newTop push: value! !

