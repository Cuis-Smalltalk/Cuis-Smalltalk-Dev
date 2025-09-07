'From Cuis7.5 [latest update: #7486] on 2 September 2025 at 4:54:52 pm'!

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:35:27'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	aMorph processUnknownEvent: self! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:35:32'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	aMorph processDropMorph: self! !


!DropFilesEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:35:36'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	aMorph processDropFiles: self! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 16:25:21'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		self isFindClassShortcut ifTrue: [ 
			(Preferences at: #classFinder) value.
			^self ].
		self isCloseWindowShortcut ifTrue: [
			self closeCurrentWindowOf: aMorph.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	type == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	type == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].
	super sendEventTo: aMorph.! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:36:38'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph.
	See #asMouseEnter, #asMouseLeave, #asMouseMove, #asMouseOver."

	type == #mouseOver ifTrue: [
		aMorph processMouseOver: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
	type == #mouseEnter ifTrue: [
		aMorph processMouseEnter: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
	type == #mouseLeave ifTrue: [
		aMorph processMouseLeave: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Backstop. Just in case."
	super sendEventTo: aMorph.! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:36:55'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseDown ifTrue: [
		aMorph processMouseDown: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
	type == #mouseUp ifTrue: [
		aMorph processMouseUp: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Because any MouseEvent can be of the kinds handled by super."
	super sendEventTo: aMorph.! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:37:05'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseMove ifTrue: [
		aMorph processMouseMove: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Because any MouseEvent can be of the kinds handled by super."
	super sendEventTo: aMorph.! !


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:37:15'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseScroll ifTrue: [
		aMorph processMouseScroll: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Because any MouseEvent can be of the kinds handled by super."
	super sendEventTo: aMorph.! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 9/2/2025 15:37:21'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	aMorph processWindowEvent: self.! !

