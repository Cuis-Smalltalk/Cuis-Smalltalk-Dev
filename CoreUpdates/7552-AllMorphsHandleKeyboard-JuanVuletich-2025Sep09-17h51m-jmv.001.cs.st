'From Cuis7.5 [latest update: #7538] on 9 September 2025 at 5:55:20 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/9/2025 17:51:56'!
processKeyDown: aKeyboardEvent
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyDown: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/9/2025 17:51:49'!
processKeyUp: aKeyboardEvent
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyUp: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/9/2025 17:51:39'!
processKeystroke: aKeyboardEvent
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyStroke: aKeyboardEvent.
	self 
		valueOfProperty: #'keyStroke:'
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].! !

!methodRemoval: InnerTextMorph #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:26'!
InnerTextMorph removeSelector: #handlesKeyboard!
!methodRemoval: WorldMorph #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:24'!
WorldMorph removeSelector: #handlesKeyboard!
!methodRemoval: PluggableListMorph #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:23'!
PluggableListMorph removeSelector: #handlesKeyboard!
!methodRemoval: HierarchicalListMorph #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:22'!
HierarchicalListMorph removeSelector: #handlesKeyboard!
!methodRemoval: EntryFieldMorph #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:21'!
EntryFieldMorph removeSelector: #handlesKeyboard!
!methodRemoval: MenuMorph #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:20'!
MenuMorph removeSelector: #handlesKeyboard!
!methodRemoval: FindInSourceDialog #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:19'!
FindInSourceDialog removeSelector: #handlesKeyboard!
!methodRemoval: Morph #handlesKeyboard stamp: 'jmv 9/9/2025 17:52:10'!
Morph removeSelector: #handlesKeyboard!
