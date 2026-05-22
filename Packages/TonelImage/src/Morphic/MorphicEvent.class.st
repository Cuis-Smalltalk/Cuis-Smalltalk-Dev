"
This class represents the base for all Morphic events.

Instance variables:
	stamp	<Integer>	The millisecond clock time stamp (based on Time millisecondClock)
	source	<Hand | nil>	If non-nil the hand that generated the event.
"
Class {
	#name : 'MorphicEvent',
	#superclass : 'Object',
	#instVars : [
		'timeStamp',
		'source'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'dispatching' }
MorphicEvent >> dispatchIn: aMorph [
	"Dispatch me.
	Different subclasses will use a specific strategy to determine who will handle each event.
	Argument is usually the WorldMorph, but not always. See subclasses."

	self subclassResponsibility
]

{ #category : 'accessing' }
MorphicEvent >> eventPosition [
	self subclassResponsibility
]

{ #category : 'accessing' }
MorphicEvent >> hand [
	"Return the source that generated the event"
	^source
]

{ #category : 'testing' }
MorphicEvent >> is: aSymbol [
	^ aSymbol == #MorphicEvent or: [ super is: aSymbol ]
]

{ #category : 'testing' }
MorphicEvent >> isDropEvent [
	^false
]

{ #category : 'testing' }
MorphicEvent >> isKeyboard [
	^false
]

{ #category : 'testing' }
MorphicEvent >> isKeystroke [
	^false
]

{ #category : 'testing' }
MorphicEvent >> isMouse [
	^false
]

{ #category : 'testing' }
MorphicEvent >> isMouseMove [
	^false
]

{ #category : 'testing' }
MorphicEvent >> isWindowEvent [
	^false
]

{ #category : 'dispatching' }
MorphicEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processUnknownEvent: self
]

{ #category : 'private' }
MorphicEvent >> setTimeStamp: stamp [
	timeStamp := stamp.
]

{ #category : 'dispatching' }
MorphicEvent >> startDispatchFromHand: aHand [
	"double dispatch the event dispatch"

	aHand startGenericEventDispatch: self.
]

{ #category : 'accessing' }
MorphicEvent >> timeStamp [
	"Return the millisecond clock value at which the event was generated"
	^timeStamp ifNil:[timeStamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
]

{ #category : 'accessing' }
MorphicEvent >> wasHandled [
	"Return true if this event was handled. May be ignored for some types of events."
	^false
]

{ #category : 'accessing' }
MorphicEvent >> wasHandled: aBool [
	"Determine if this event was handled. May be ignored for some types of events."
]
