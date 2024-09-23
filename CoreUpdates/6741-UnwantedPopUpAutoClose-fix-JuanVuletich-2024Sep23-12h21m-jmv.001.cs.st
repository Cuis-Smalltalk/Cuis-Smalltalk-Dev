'From Cuis7.1 [latest update: #6740] on 23 September 2024 at 12:21:59 pm'!

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 9/23/2024 11:31:55'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection := self editor selectionInterval copy.
	prevScrollValue := owner verticalScrollBar scrollValue.
	
	hasUnacceptedEdits ifFalse: [ self flash. ^true ].
	hasEditingConflicts ifTrue: [
		self confirmAcceptAnyway ifFalse: [self flash. ^false]].
	
	accepted := model acceptContentsFrom: owner.
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
				"The next line would make PopUpMenu to auto close because of losing focus..."
				"self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ]."
				(owner is: #ScrollPane) ifTrue: [
					owner verticalScrollBar internalScrollValue: prevScrollValue]].
			true]
		ifFalse: [ false ]! !

