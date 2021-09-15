'From Cuis 5.0 [latest update: #4384] on 30 December 2020 at 7:32:37 pm'!

!ContextPart methodsFor: 'closure support' stamp: 'HAW 12/30/2020 19:31:45'!
contextTag
	"Context tags may be used for referring to contexts instead of contexts themselves as they can be copied and will continue to work in other processes (continuations). By default, we use the context itself to as its tag."
	^self! !

!methodRemoval: MethodContext #contextTag stamp: 'HAW 12/30/2020 19:31:45'!
MethodContext removeSelector: #contextTag!
