Class {
	#name : 'UserInputEvent',
	#superclass : 'MorphicEvent',
	#instVars : [
		'buttons',
		'position',
		'wasHandled'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'modifier state' }
UserInputEvent >> anyModifierKeyPressed [
	"ignore, however, the shift keys 'cause that's not REALLY a command key "

	^ self buttons anyMask: InputSensor anyModifierKey
]

{ #category : 'accessing' }
UserInputEvent >> buttons [
	"Return the a word encoding the mouse and modifier buttons for this event."

	^ buttons
]

{ #category : 'modifier state' }
UserInputEvent >> commandAltKeyPressed [
	"Answer true if the command (Mac) / alt(Windows) key on the keyboard was being held down when this event occurred."

	^ buttons anyMask: InputSensor commandAltKey
]

{ #category : 'modifier state' }
UserInputEvent >> controlKeyPressed [
	"Answer true if the control/ctrl key on the keyboard was being held down when this event occurred."

	^ buttons anyMask: InputSensor controlKey
]

{ #category : 'dispatching' }
UserInputEvent >> dispatchIn: aMorph [
	"Dispatch me.
	Different subclasses will use a specific strategy to determine who will handle each event.
	See argument name in subclasses for the specific morph passed in."

	self subclassResponsibility
]

{ #category : 'accessing' }
UserInputEvent >> eventPosition [
	^position
]

{ #category : 'modifier state' }
UserInputEvent >> macOptionKeyPressed [
	"Answer whether the option key on the Macintosh keyboard was being held down when this event occurred. Macintosh specific."

	self notify: 'Portability note:
MorphicEvent>>macOptionKeyPressed is not portable.
Please use MorphicEvent>>mouseButton2Pressed instead!'.

	^ self rawMacOptionKeyPressed
]

{ #category : 'printing' }
UserInputEvent >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		print: buttons;
		nextPut: $)
]

{ #category : 'modifier state' }
UserInputEvent >> rawMacOptionKeyPressed [
	"Answer whether the option/alt key on the Macintosh keyboard was being held down when this event occurred. Macintosh specific. Please do not confuse with the alt key on Windows and Linux, that is called 'command' on the mac. See #commandAltKeyPressed"

	^ buttons anyMask: InputSensor macOptionKey
]

{ #category : 'modifier state' }
UserInputEvent >> shiftPressed [
	"Answer true if the shift key on the keyboard was being held down when this event occurred."

	^ buttons anyMask: InputSensor shiftKey
]

{ #category : 'accessing' }
UserInputEvent >> wasHandled [
	^wasHandled
]

{ #category : 'accessing' }
UserInputEvent >> wasHandled: aBool [

	wasHandled := aBool
]
