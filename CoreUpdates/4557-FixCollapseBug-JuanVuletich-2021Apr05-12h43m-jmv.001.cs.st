'From Cuis 5.0 [latest update: #4552] on 5 April 2021 at 12:47:10 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 4/5/2021 12:46:44'!
maybeCollapse: event with: aHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle containsGlobalPoint: event eventPosition)
		ifTrue: [
			target collapse ].
	self delete.! !

