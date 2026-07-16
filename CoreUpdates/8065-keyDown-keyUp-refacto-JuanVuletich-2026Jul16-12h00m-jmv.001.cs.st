'From Cuis7.9 [latest update: #8065] on 16 July 2026 at 12:00:28 pm'!

!Morph methodsFor: 'events' stamp: 'jmv 16/Jul/2026, 2:45:11 pm (UTC)'!
keyDown: aKeyboardEvent
	"Handle a key down event. The default response is to do nothing."! !

!Morph methodsFor: 'events' stamp: 'jmv 16/Jul/2026, 2:45:58 pm (UTC)'!
keyUp: aKeyboardEvent
	"Handle a key up event. The default response is to do nothing."! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 16/Jul/2026, 2:44:58 pm (UTC)'!
processKeyDown: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyDown: aKeyboardEvent.
	self 
		valueOfProperty: #'keyDown:' 
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 16/Jul/2026, 2:45:51 pm (UTC)'!
processKeyUp: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyUp: aKeyboardEvent.
	self 
		valueOfProperty: #'keyUp:'
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].! !

