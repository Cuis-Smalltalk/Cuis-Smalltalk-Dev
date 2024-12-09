'From Cuis7.3 [latest update: #6899] on 9 December 2024 at 1:05:57 pm'!

!WeakMessageSend methodsFor: 'private' stamp: 'HAW 10/30/2024 08:25:06'!
isAnyArgumentGarbage
	"Make sure that my arguments haven't gone away"
	
	^arguments notNil and: [ arguments with: shouldBeNil anySatisfy: [ :arg :flag | flag not and: [arg isNil]]].
	
! !

