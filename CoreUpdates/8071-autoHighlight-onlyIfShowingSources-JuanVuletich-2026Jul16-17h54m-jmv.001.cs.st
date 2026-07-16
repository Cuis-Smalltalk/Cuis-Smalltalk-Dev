'From Cuis7.9 [latest update: #8070] on 16 July 2026 at 5:56:14 pm'!

!MethodSet methodsFor: 'accessing' stamp: 'jmv 16/Jul/2026, 8:49:31 pm (UTC)'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString := input string.
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class := selectedMessage actualClass.
	oldSelector := selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue: [
		oldSelector == #Comment ifTrue: [
			class comment: codeString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: codeString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false]].
	"Normal method accept"
	category := class organization categoryOfElement: oldSelector.
	selector := class compile: codeString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	shouldAutoHighlight := self showingSource.
	self triggerEvent: #annotationChanged.
	^ true! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 16/Jul/2026, 8:49:41 pm (UTC)'!
messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."

	| list |
	
	list := self messageList.
	selectedMessage := (list isInBounds: anInteger) ifTrue: [ list at: anInteger ].
	shouldAutoHighlight := self showingSource.
	self changed: #messageListIndex.	 "update my selection"
	self changed: #editSelection.
	self acceptedContentsChanged! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 16/Jul/2026, 8:49:20 pm (UTC)'!
autoHighlightString: aString allOccurrences: aBoolean
	"Set the string to be automatically highlighted."

	shouldAutoHighlight := self showingSource.
	autoHighlightString := aString.
	autoHighlightAllOccurrences := aBoolean.! !


!ParameterizedCodeMessageSet methodsFor: 'initialization' stamp: 'jmv 16/Jul/2026, 8:54:09 pm (UTC)'!
initialize

	selectedIndex := 0.
	shouldAutoHighlight := self showingSource.
	super initialize ! !

