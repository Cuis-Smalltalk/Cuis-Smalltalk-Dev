'From Cuis7.3 [latest update: #6962] on 12 January 2025 at 9:01:29 pm'!

!Random methodsFor: 'accessing' stamp: 'eem 1/12/2025 19:26:00'!
next: numberOfElements
	| elements |
	elements := Array new: numberOfElements.
	1 to: numberOfElements do:
		[:i| elements at: i put: self next].
	^elements! !

!Character class methodsFor: 'instance creation' stamp: 'eem 2/5/2014 12:49'!
value: anInteger
	"Answer the Character whose value is anInteger."
	<primitive: 170>
	^self primitiveFailed! !
