Understanding Drag and Drop Mechanics
=====================================

Event driven code allows multiple objects to interact without direct contact.

One example of this is **Drag and Drop**.
One might, say "pick up a color" from a color palette,
and drop it on an area which may or may not be sensitive to colors.

How does this work?

Part of the difficulty in understanding event driven code is that in many
cases, small bits of work are split among a number of different classes.

Following is a brief discussion, with much code,
showing how events are used to pick up some value
and drop onto some target morph.
We are going to look at the main beads of the thread and skip
a number of details.

You can take a look at the code via the **UI-Tools** package and use
of a code or message names browser and look at **senders**.


````Smalltalk
  Feature require: 'UI-Tools'.
````

## DRAG

First, let's look at picking up a value.
What do we mean by this?
Basically, we move the cursor/hand over a Morph,
press the mouse and keeping the mouse "down" drag
a Morph denoting the thing being dragged.
Letting up on the mouse button "drops" the value,
which may be accepted or rejected by the Morph it
is dropped upon.

This is an "opt in" for a Morph.
Just as with mouse events, a Morph class 
can override method ````allowsSubmorphDrag```` to answer **true**
or an individual Morph can set the property with that name.

````Smalltalk
Morph>>allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, 
         instead of requiring the use of the halo.
	 By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsSubmorphDrag'
````

As with mouse events, the HandMorph does most of the interacting starting
with  the processing of the
event queue (````HandMorph>>processEventQueue````),
but we don't need to understand the details of this to get the
gist of what is going on.

At some point, a Morph which allowsSubMorphDrag
gets to handle the mouse down event.
For example, in the MetaProperty package:

````Smalltalk
VisualPropertyMenuItem>>processMouseDown: evt localPosition: localEventPosition
	"Do nothing upon mouse-down except inform the hand 
	 to watch for a click; wait until an ensuing #click: 
	 message gets dispatched"

	evt wasHandled: true.
	evt hand waitForClicksOrDrag: self
				event: evt
				dragSel: #dragEvent:localPosition:
				clkSel: #mouseButton1Down:localPosition:
````

The VisualPropertyMenuItem sets the drag
selector to ````#dragEvent:localPosition:````.
Upon a drag action,
the drag event causes the HandMorph to "pick up" something.

````Smalltalk
Morph>>dragEvent: aMouseEvent localPosition: aPoint

	aMouseEvent hand halo: nil.
	aMouseEvent hand grabMorph: self
````

The "something" which is picked up is a Morph
which becomes a submorph of the HandMorph.

````Smalltalk
HandMorph>>grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand 
	 and remove it from its current owner) 
	 without changing its position. 
	 This is used to pick up a morph under the 
	 hand's current position, versus attachMorph: 
	 which is used to pick up a morph that 
	 may not be near this hand."

	^self grabMorph: aMorph moveUnderHand: false
````

````Smalltalk
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph 
	(i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, 
	move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	self hideHardwareCursor.
	self redrawNeeded.
"...some code elided..."
````

The grabbed Morph which is ````aboutToBeGrabbedBy:```` gets
to answer the actual Morph which is given to the HandMorph.

For example, picking up a Color from a color palette "grabs" a
DropColorMorph, which gives a copy of itself to the HandMorph
to carry around.

````Smalltalk
DropColorMorph>>aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return
	the actual morph that should be added to the hand.
	Answer nil to reject the drag."
	"This message is sent to the dragged morph, not to the owner."

	^ self class fromColor: self color "Grab a new sibling of me"
````


## DROP

OK. The HandMorph is carrying around a submorph and the mouse is down.
What happens when we drop a morph representing a value somewhere
by letting up on the mouse?

````Smalltalk
HandMorph>>dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent _ DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	owner dispatchEvent: dropEvent.
	dropEvent wasHandled
		  ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.
	self mouseOverHandler processMouseOver: aMouseEvent
````
Upon a mouse up event, the HandMorph creates a DropEvent and asks
it to dispatch to some Morph under the hand, then checks if
the event was handled, or if the drop was rejected.

The actual dispatch contains the cleverness which allows the dropped
Morph and the Morph which is the target of the drop to mutually
decide if they like each other enough to cooperate.

````Smalltalk
DropEvent>>dispatchWith: aMorph
	"Drop is done on the innermost target that accepts it."
	| dropped |

	"Try to get out quickly"
	(aMorph fullIncludesPixel: position)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		(eachChild dispatchEvent: self) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop
	  and: [ (aMorph rejectsEvent: self) not
	    and: [aMorph fullIncludesPixel: position] ])
		ifTrue: [
		"Do a symmetric check if both morphs like each other"
		dropped _ self contents.
		((aMorph wantsDroppedMorph: dropped event: self)  "I want her"
		    and: [dropped wantsToBeDroppedInto: aMorph]) "she wants me"
			ifTrue: [ ^ self sendEventTo: aMorph ]].
	^#rejected
````

Deciding to cooperate is up to each of the dropped Morph and drop target.

````Smalltalk
Morph>>wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, 
	 which is being dropped by a hand in response to the given event. 
	 Note that for a successful drop operation both parties need to agree. 
	 The symmetric check is done automatically 
	 via aMorph wantsToBeDroppedInto: self.
	 Individual Morpks may override by setting the corresponding property
	 to an appropriate two argument closure."

    ^self valueOfProperty: #wantsDroppedMorph:event:
       ifPresentDo: [ :wantsMorphEvt | wantsMorphEvt value: aMorph value: evt ]
       ifAbsent: [ true ]
````

For example, a VisualPropertyMenuItem has a MetaProperty with a test, so
it overrides ````wantsDroppedMorph:event:````.

````Smalltalk
VisualPropertyMenuItem>>wantsDroppedMorph: aMorph event: evt

	^ self allowsValue: aMorph valueWhenDropped
````

As another example, a MorphEditLens drop target checks
that a least one visual
property is willing to accept the dropped value.

````Smalltalk
MorphEditLens>>wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph"

	^ (aMorph hasProperty: #DropActionMorph)
	or: [(Smalltalk includesKey: #MetaProperty)
		and: [((MetaProperty metaPropsForMorph: 
					self targetMorph)
				detect: [ :metaProp | 
						metaProp accepts: 
						  aMorph valueWhenDropped] 
				ifNone: [^false]).
			^true
		]
	]
````

So much for the Morph which is the taget of the drop..
On the other side of the action, the dropping Morph also
gets to answer if it wants to be dropped onto the
target morph.

For example, a DropColorMorph checks for a MorphEditLens, a #dropAction
property, or a VisualPropertyMenuItem.

The guards around the ````isKindOf:```` test
make sure that the class code is loaded before being asked.

````Smalltalk
DropColorMorph>>wantsToBeDroppedInto: aMorph
	"Who do I wish to be dropped onto?"

	((Smalltalk includesKey: #MorphEditLens) "May not be present"
	 and: [aMorph isKindOf: (Smalltalk at: #MorphEditLens)])
		ifTrue: [ ^true ].
		
	(aMorph hasProperty: #dropAction)
		ifTrue: [ ^true ].
		
	((Smalltalk includesKey: #VisualPropertyMenuItem)
	 and: [ aMorph isKindOf: (Smalltalk at: #VisualPropertyMenuItem) ])
		ifTrue: [ ^true ].
				
	^ false 
````

Eventually, if everyone agrees to the "dating before marriage", 
the target of the drop finally gets
an introduction to the dropping morph.

````Smalltalk
DropEvent>>sendEventTo: aMorph
	"Dispatch the receiver into aMorph"

	^aMorph processDropMorph: self
````

The drop target then gets to query the DropEvent for its
desired drop Morph and gets to take some action for the drop.
This allows toolkits to cooperate with a minimum of required detail.

````Smalltalk
DropColorMorph>>processDropMorph: aDropEvent
	"I have already expressed a desire for the drop. Just do it."
	
	| dropedMorph dropAction |
	dropedMorph := aDropEvent contents.
	dropAction := self valueOfProperty: #dropAction ifAbsent: [ nil ]. 
	aDropEvent wasHandled: (dropAction notNil).
	dropAction ifNotNil: [ :doIt |
		doIt value: dropedMorph value:  dropedMorph valueWhenDropped.
	 ]
````

Again, it is easy to have individual Morphs participate in Drag and Drop
by setting property values.

An example is the Color Editor, which makes many uses of events (e.g. to
update display areas when the ColorEditModel changes color --
note ````ColorEditModel>>setColor:````).

In this case, the areas where a color can be dropped are made sensitive.

````Smalltalk
ColorEditorPanel>>colorSwatchesBeDroppable

	{alphaSwatch. colorPane. colorSwatch.} do: [ :dropTarget | 
	  dropTarget
	    setProperty: #'allowsMorphDrop' toValue: true;
	    setProperty: #wantsDroppedMorph:event: 
		toValue: [ :dropMorph :evt |
			   dropMorph valueWhenDropped isKindOf: Color] ;
	    setProperty: #dropAction 
		toValue: [ :dropMorph :colorValue |
			self setColor: colorValue. 
			dropMorph showAcceptAndDeleteSelf.
		]
	].
````

It is a good idea to let the user know when something is complete.
The ````showAcceptAndDeleteSelf````, ````showReject````, and friends
give visual feedback that a drop was accepted or rejected.


````Smalltalk
SignMorph>>rejectDropMorphEvent: dropEvent
	"The receiver has been rejected.  Disappear"
	
	self showReject; hide; delete.
	self world ifNotNil: [ :w | w activeHand removeMorph: self ].
````

Finally, when a drop succeeds, the dropped Morph gets
notification that all went well and the action is complete.
The dropped Morph only gets this after a successful drop.

````Smalltalk
SignMorph>>justDroppedInto: newOwnerMorph event: anEvent 
	"This message is sent to a dropped morph 
	 after it has been dropped on -- 
	 and been accepted by -- a drop-sensitive morph"

	self showAcceptAndDeleteSelf 
````

Whew!  You can now see that a long evolution of making Drag and Drop
activity work for a large variety of use-cases has led to a sophisticated
protocol.  But when you need it..

OK.  One last example: **Introspection -- looking within**.

Smalltalk knows about itself and can be asked about itself.
This lets us query internals and put up a choice list.

Here is the code used by a SignMorph to give choices when
dropped upon a MorphEditLens drop target.

When dropped on something that has only a simple action, the
strategy above is followed, but in this case we want to
find all the _possible_ actions and give the user a choice.

Note that this only happens if MetaProperties are loaded.

````Smalltalk
SignMorph>>dropAction: aDropTargetMorph
	"Find accepting MetaProperties of target morph
	 and allow user to choose action."
	
	| metaPropsForMyValue myValue choices selection |
	(Smalltalk includesKey: #MetaProperty) ifFalse: [^nil ].
	
	myValue := self valueWhenDropped.
	metaPropsForMyValue := 
		(MetaProperty metaPropsForMorph:  aDropTargetMorph targetMorph)
			select: [ :metaProp | metaProp accepts: myValue ]. 
	(metaPropsForMyValue size isZero) ifTrue: [^nil ].
	
	choices := OrderedCollection with: #Cancel.
	choices addAll: (metaPropsForMyValue keys).
	
	"I am being carried by the hand. 
	 Disappear and let user make a choice."
	self delete.
	selection := PopUpMenu withCaption: 'Choose setter' 
						chooseFrom: choices.
	(selection = 1) ifFalse: [ "1 -> Cancel" | propName setterSym metaProp |
		propName := choices at: selection.
		setterSym := (propName , ':') asSymbol.
		metaProp := metaPropsForMyValue at: propName.
		(metaProp isKindOf: MetaPropertyMultiSelect)
			ifTrue: [myValue := metaProp encodeProc value: myValue].
		aDropTargetMorph targetMorph 
			perform: setterSym
			with: myValue ;
			triggerEvent: #propertyChanged.
	].
````

The Drag and Drop mechanics are complex, but implement a sophisticated
system to design pretty much any style of interaction you want or need.

Hopefully this introduction has given enough clues to do what interests you!

