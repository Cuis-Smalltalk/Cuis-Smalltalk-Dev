'From Cuis 6.0 [latest update: #5743] on 3 April 2023 at 10:00:47 am'!

!MessageSet methodsFor: 'accessing' stamp: 'jmv 4/3/2023 10:00:27'!
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
	shouldAutoHighlight := true.
	self triggerEvent: #annotationChanged.
	^ true! !

