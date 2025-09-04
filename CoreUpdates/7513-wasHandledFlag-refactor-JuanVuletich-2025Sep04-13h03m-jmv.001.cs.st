'From Cuis7.5 [latest update: #7507] on 4 September 2025 at 1:07:56 pm'!

!Morph methodsFor: 'events' stamp: 'jmv 9/4/2025 13:04:49'!
keyDown: aKeyboardEvent
	"Handle a key down event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyDown:' 
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ]! !

!Morph methodsFor: 'events' stamp: 'jmv 9/4/2025 13:05:01'!
keyUp: aKeyboardEvent
	"Handle a key up event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyUp:'
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:06:33'!
processKeyDown: aKeyboardEvent
	"System level event handling."

	self handlesKeyboard ifFalse: [^self].
	"#keyDown: must set the event as #wasHandled: true if it effectively was."
	self keyDown: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:06:39'!
processKeyUp: aKeyboardEvent
	"System level event handling."

	self handlesKeyboard ifFalse: [^self].
	"#keyUp: must set the event as #wasHandled: true if it effectively was."
	self keyUp: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:06:22'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	self handlesKeyboard ifTrue: [
		"#keyStroke: must set the event as #wasHandled: true if it effectively was."
		self keyStroke: aKeyboardEvent.
		self 
			valueOfProperty: #'keyStroke:'
			ifPresentDo: [ :handler |
				handler value: aKeyboardEvent.
				aKeyboardEvent wasHandled: true ]].! !

