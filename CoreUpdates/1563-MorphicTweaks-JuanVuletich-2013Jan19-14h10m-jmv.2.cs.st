'From Cuis 4.1 of 12 December 2012 [latest update: #1562] on 19 January 2013 at 2:26:55 pm'!

!Morph methodsFor: 'event handling testing' stamp: 'jmv 1/18/2013 15:50'!
allowsMorphDrop
"pensar un cacho mas"
	"Answer whether we accept dropping morphs. By default answer false."

	^(self valueOfProperty: #dropEnabled) ifNil: [ false ]! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 1/18/2013 15:50'!
allowsSubmorphDrag

"pensar un cacho mas"
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^(self valueOfProperty: #dragEnabled) ifNil: [ false ]! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/18/2013 13:41'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^(self valueOfProperty: #dropEnabled) ifNil: [ true ]! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/18/2013 13:42'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^(self valueOfProperty: #dragEnabled) ifNil: [ true ]! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 1/18/2013 17:39'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside eventPositionInChild |

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Give aMorph a chance to repel the dropping morph"
	(aMorph repelsMorph: self contents event: self) ifTrue: [
		self wasHandled: true.
		self contents rejectDropMorphEvent: self.
		^self ].

	"Go looking if any of our submorphs wants it"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				inside _ true
			]]].

	inside ifFalse: [ inside _ aMorph containsPoint: positionInAMorph event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph].
	^#rejected! !


!Morph methodsFor: 'accessing' stamp: 'jmv 1/18/2013 11:46'!
isLocked
	"Answer whether the receiver is Locked.
	The idea is that a locked morph behaves as if it wasn't a separate object,
	but just part of the #drawOn: method in the owner. Locked morphs receive no events."

	self hasExtension
		ifFalse: [^ false].
	^ self extension locked! !

!Morph methodsFor: 'accessing' stamp: 'jmv 1/18/2013 13:24'!
isSticky
	"answer whether the receiver is Sticky.
	A morph that is made sticky can not be easily grabbed with the hand."
	self hasExtension
		ifFalse: [^ false].
	^ self extension sticky! !

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 14:00'!
separateDragAndDrop
	"Conversion only. Separate the old #dragNDropEnabled into #dragEnabled and #dropEnabled and remove the old property."
	| dnd |
	(self hasProperty: #dragNDropEnabled) ifFalse:[^self].
	dnd _ (self valueOfProperty: #dragNDropEnabled) == true.
	self enableDrag: dnd.
	self enableDrop: dnd.
	self removeProperty: #dragNDropEnabled.
! !

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/18/2013 15:48'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self."
"ver"
	^self allowsMorphDrop! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 1/18/2013 17:34'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseButton messages ?
	- #mouseButton1Down:localPosition:
	- #mouseButton1Up:localPosition:
	- #mouseButton2Down:localPosition:
	- #mouseButton2Up:localPosition:
	- #mouseButton3Down:localPosition:
	- #mouseButton3Up:localPosition:
	- #mouseMove:localPosition:
	- #mouseButton2Activity
	
	NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true.
	
	Implementors could query the argument, and only answer true for (for example) button 2 up only."

	^ false! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/18/2013 17:46'!
processDropMorph: aDropEvent localPosition: localEventPosition
	"Handle a dropping morph."
	| aMorph posInWorld |
	
	aDropEvent wasHandled ifTrue: [ ^self ]. "Do it just once, for one drop destination"
	
	aMorph _ aDropEvent contents.
	"Do a symmetric check if both morphs like each other"
	((self wantsDroppedMorph: aMorph event: aDropEvent)	"I want her"
		and: [aMorph wantsToBeDroppedInto: self])		"she wants me"
			ifFalse: [
				^ self].
	aDropEvent wasHandled: true.
	posInWorld _ aMorph referencePosition.
	aMorph referencePosition: posInWorld.
	self acceptDroppingMorph: aMorph event: aDropEvent.
	aMorph justDroppedInto: self event: aDropEvent! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 13:59'!
morphicDirectoryTreePane

	^(HierarchicalListMorph
		model: model
		listGetter: #initialDirectoryList
		indexGetter: #currentDirectorySelected
		indexSetter: #setSelectedDirectoryTo:
		mainView: self
		menuGetter: #volumeMenu
		keystrokeAction: nil)
			autoDeselect: false;
			enableDrag: false;
			enableDrop: true not;
			yourself! !

!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 13:59'!
morphicFileListPane

	^(PluggableListMorph
		model: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: 
		mainView: self
		menuGetter: #fileListMenu
		keystrokeAction: nil)
			enableDrag: true not;
			enableDrop: false;
			yourself! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 1/19/2013 14:22'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner eventPositionInChild |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition) ifFalse: [
		^#rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sentTo: aMorph localPosition: positionInAMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand grabMorph: aMorph.
					self wasHandled: true.
					^self ]]].

	handledByInner ifTrue: [ ^self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/18/2013 15:48'!
repelsMorph: aMorph event: ev
	(aMorph wantsToBeDroppedInto: self) ifFalse: [^ false].
	self allowsMorphDrop ifFalse: [^ true].
	(self wantsDroppedMorph: aMorph event: ev) ifFalse: [^ true].
	^ super repelsMorph: aMorph event: ev "consults #repelling flag"! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/18/2013 15:48'!
wantsDroppedMorph: aMorph event: evt
	self isWorldMorph ifTrue:[^true]. "always"
	self visible ifFalse: [^ false].  "will be a call to #hidden again very soon"
	self allowsMorphDrop ifFalse: [^ false].
	^ true! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 1/18/2013 12:38'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!methodRemoval: PasteUpMorph #morphToGrab:!
PasteUpMorph removeSelector: #morphToGrab:!
!methodRemoval: FillInTheBlankMorph #handlesMouseDown:!
FillInTheBlankMorph removeSelector: #handlesMouseDown:!
!methodRemoval: FillInTheBlankMorph #mouseButton1Down:localPosition:!
FillInTheBlankMorph removeSelector: #mouseButton1Down:localPosition:!

!FillInTheBlankMorph reorganize!
('accessing' response response: selectionInterval)
('initialization' autoCompleterClass createAcceptButton createCancelButton createQueryTextMorph: createTextPaneExtent:acceptBoolean: defaultColor delete initialize responseUponCancel: setQuery:initialAnswer:answerExtent:acceptOnCR:)
('invoking' getUserResponse morphicLayerNumber)
('menu' acceptClicked cancelClicked)
('drawing' drawOn:)
!

!methodRemoval: Morph #allowsGrabWithHand!
Morph removeSelector: #allowsGrabWithHand!
!methodRemoval: Morph #dragEnabled:!
Morph removeSelector: #dragEnabled:!
!methodRemoval: Morph #dropEnabled:!
Morph removeSelector: #dropEnabled:!
!methodRemoval: Morph #rejectDropEvent:!
Morph removeSelector: #rejectDropEvent:!
