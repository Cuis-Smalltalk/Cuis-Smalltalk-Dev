'From Cuis7.3 [latest update: #6922] on 18 December 2024 at 8:18:48 pm'!

!DebuggerWindow class methodsFor: 'services' stamp: 'eem 12/18/2024 11:12:07'!
closeAllDebuggersSuchThat: aBlock
	"DebuggerWindow closeAllDebuggers"

	UISupervisor ui ifNotNil:
		[ :world |
		world submorphsDo:
			[ :morph |
			((morph class == DebuggerWindow or: [morph class == PreDebugWindow ])
			 and: [aBlock value: morph])
				ifTrue: [morph delete ]]]! !


!DebuggerWindow class methodsFor: 'services' stamp: 'eem 12/18/2024 11:12:22'!
closeAllDebuggers
	"DebuggerWindow closeAllDebuggers"

	self closeAllDebuggersSuchThat: [:ign| true]! !

