'From Cuis 5.0 [latest update: #4958] on 31 October 2021 at 7:13:13 pm'!

!InnerTextMorph commentStamp: '<historical>' prior: 0!
InnerTextMorphs support display of text with emphasis.  They also support reasonable text-editing capabilities, as well as embedded hot links, and the ability to embed submorphs in the text. They are 'bare' in the sense that they can not clip contents to some window, or scroll it by themselves.

Text display is clipped to the innerBounds of the rectangle, and text composition is normally performed within a rectangle which is innerBounds inset by the margins parameter.

Comment about Shout specifics:
-----------------------------------------

In order to colour the text, I use an instance of SHTextStylerST80, which I store in my 'styler' instance variable.

When my setText: method is called, I use my styler to ...
	a) optionally set all assignments to ansi or leftArrow. 
	b) Colour my text (immediately, if the text is less than 4096 chars in length, or in a backgroundProcess otherwise)

	When my text is changed, my hasUnacceptedEdits: method is called with true, and I ask my styler to re-colour my text. This is performed in a background process so that typing remains responsive regardless of the length of the text.

	Just before my styler is about to format/style the text, I send #stylerAboutToStyle:  to my model. This gives my model a chance to veto the styling (by answering false), or to initialize the styler with information it needs in order to parse the text correctly (e.g. the class to which a method belongs, or the workspace in which I am contained).

	My styler informs me that it has finished styling by triggering the #shoutStyled event which I handle. I then update the textAttributes of my text and refresh the display.
 
	My 'unstyledAcceptText' instance variable is used in conjunction with my #acceptTextInModel and #correctFrom:to:with: methods to ensure that when my text is modified during a method compilation (removing unused vars etc), I do not lose those changes.!


!Morph methodsFor: 'testing' stamp: 'jmv 10/31/2021 19:06:51'!
drawsKeyboardFocusIndicator

	^false! !


!PluggableScrollPane methodsFor: 'testing' stamp: 'jmv 10/31/2021 19:05:16'!
drawsKeyboardFocusIndicator

	^drawKeyboardFocusIndicator! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 10/31/2021 19:07:57'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar scrollValue.
	
	hasUnacceptedEdits ifFalse: [ self flash. ^true ].
	hasEditingConflicts ifTrue: [
		self confirmAcceptAnyway ifFalse: [self flash. ^false]].
	
	accepted _ model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	
	"some implementors of acceptContentsFrom: answer self :("
	^accepted == true 
		ifTrue: [
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			UISupervisor whenUIinSafeState: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				(owner is: #ScrollPane) ifTrue: [
					owner verticalScrollBar internalScrollValue: prevScrollValue]].
			true]
		ifFalse: [ false ]! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:16:36'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor clickAndHalf ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:16:42'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor doubleClickAndHalf ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:17:07'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking.
	self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:17:20'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [
		^ self enterClickableRegion: aMouseMoveEvent localPosition: localEventPosition ].
	self handleInteraction: [
		editor mouseMove: aMouseMoveEvent localPosition: localEventPosition].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 10/31/2021 19:04:30'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			self showsBlinkingCursor ifTrue: [
				editor hasSelection ifFalse: [
					self startBlinking ]]]
		ifFalse: [ self stopBlinking ].
	
	(owner notNil and: [ owner drawsKeyboardFocusIndicator ])
		ifTrue: [ owner redrawNeeded ]
		ifFalse: [
			"Selection might be shown differently when focused"
			self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ]]	.! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 10/31/2021 18:18:44'!
processKeystrokeEvent: evt
	| action |

	(acceptOnCR and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.
	
	"Return - check for special action"
	evt isReturnKey ifTrue: [	
		action _ self crAction.
		action ifNotNil: [ ^action value]].
	
	"Esc - check for special action"
	evt isEsc ifTrue: [			
		action _ self escAction.
		action ifNotNil: [ ^action value]].
	
	self handleInteraction: [ editor processKeystrokeEvent: evt ].	
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 18:15:57'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	model ifNotNil: [
		wrapFlag ifTrue: [
			extent x = oldExtent x ifFalse: [
				self resetTextComposition.
				self editor recomputeSelection.	
				(owner is: #ScrollPane) ifTrue: [
					owner
						updateScrollBarsBounds;
						setScrollDeltas ]]]].! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 19:09:04'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				oldExtent _ extent.
				extent _ newExtent.
				self extentChanged: oldExtent.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	(owner is: #ScrollPane) ifTrue: [
		owner innerHeight: newExtent y ].! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 17:52:28'!
selectionChanged

	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].
	(self showsBlinkingCursor and: [ editor hasSelection not ])
		ifTrue: [ self hasKeyboardFocus ifTrue: [self startBlinking ]]
		ifFalse: [ self stopBlinking ]! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 18:16:05'!
updateFromTextComposition
	"A change has taken place in my textComposition, as a result of editing and I must be updated. "

	textComposition ifNotNil: [
		editor storeSelectionInComposition.
		needsFit ifTrue: [
			self fit.
			needsFit _ false ]].

	(owner is: #ScrollPane) ifTrue: [
		owner
			updateScrollBarsBounds;
			setScrollDeltas ].! !

!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 10/31/2021 19:07:36'!
possiblyChanged
	| embeddedMorphs |
	embeddedMorphs _ model actualContents embeddedMorphs.
	self submorphsDo: [:each| 
		(embeddedMorphs includes: each) ifFalse: [
			self privateRemove: each.
			each privateOwner: nil ]].
	embeddedMorphs do: [ :each|
		each owner == self ifFalse: [
			self addMorphFront: each.
			each hide "Show it only when properly located"]].
	(owner is: #ScrollPane) ifTrue: [
		owner possiblyChanged ]! !

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 10/31/2021 18:18:53'!
stylerStyled

	model allowStylingWithEmphasis ifTrue: [
		self textComposition composeAll ].
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!methodRemoval: TextModelMorph #drawKeyboardFocusIndicator stamp: 'jmv 10/31/2021 19:12:09'!
TextModelMorph removeSelector: #drawKeyboardFocusIndicator!
!methodRemoval: InnerTextMorph #scrollSelectionIntoView stamp: 'jmv 10/31/2021 19:12:09'!
InnerTextMorph removeSelector: #scrollSelectionIntoView!
