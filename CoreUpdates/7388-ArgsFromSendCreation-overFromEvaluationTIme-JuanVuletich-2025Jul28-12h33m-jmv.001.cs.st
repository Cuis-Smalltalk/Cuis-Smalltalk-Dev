'From Cuis7.5 [latest update: #7385] on 28 July 2025 at 12:36:44 pm'!

!MessageSend methodsFor: 'private' stamp: 'jmv 7/28/2025 11:01:22'!
collectArguments: anArgArray
	"Arguments set when builing the receiver (i.e. set when registering for an event)
	take precedence over anArgArray (i.e. set when triggering the event).
	If you want to use arguments set at trigger event time, set to an empty array, or set individual elements to nil."

	| staticArgs answer |
	staticArgs := self arguments.
	answer := Array new: selector numArgs.
	1 to: answer size do: [ :i | | each |
		each := staticArgs at: i ifAbsent: nil.
		each isNil ifTrue: [
			each := anArgArray at: i ifAbsent: nil ].
		answer at: i put: each ].
	^answer! !


!MessageSend class methodsFor: 'instance creation' stamp: 'jmv 7/28/2025 11:10:37'!
receiver: anObject selector: aSymbol arguments: anArray
	"Note: Arguments set here will take preceence over those set when doing #valueWithArguments: and #triggerEvent:withArguments:. If you want them to take precedence, pass here #() or individual nil elements in anArray. See #collectArguments:"

	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray! !


!WeakMessageSend methodsFor: 'private' stamp: 'jmv 7/28/2025 11:01:41'!
collectArguments: anArgArray
	"Arguments set when builing the receiver (i.e. set when registering for an event)
	take precedence over anArgArray (i.e. set when triggering the event).
	If you want to use arguments set at trigger event time, set to an empty array, or set individual elements to nil."

	| staticArgs answer |
	staticArgs := self arguments.
	answer := Array new: selector numArgs.
	1 to: answer size do: [ :i | | each |
		each := staticArgs at: i ifAbsent: nil.
		each isNil ifTrue: [
			each := anArgArray at: i ifAbsent: nil ].
		answer at: i put: each ].
	^answer! !


!WeakMessageSend class methodsFor: 'instance creation' stamp: 'jmv 7/28/2025 11:11:17'!
receiver: anObject selector: aSymbol arguments: anArray
	"Note: Arguments set here will take preceence over those set when doing #valueWithArguments: and #triggerEvent:withArguments:. If you want them to take precedence, pass here #() or individual nil elements in anArray. See #collectArguments:"

	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray! !

