'From Cuis 5.0 [latest update: #3832] on 4 August 2019 at 10:07:59 pm'!

!Debugger methodsFor: 'accessing' stamp: 'HAW 8/4/2019 22:06:34'!
contents: aText notifying: aController
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."

	| result selector classOfMethod category h ctxt newMethod |

	contextStackIndex = 0 ifTrue: [^false].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=self selectedClass parserClass selectorFrom: aText.

	selector ~= self selectedMessageName ifTrue: [
		self inform: 'Can not change the selector in the debugger'.
		^false].	
 	(classOfMethod = UndefinedObject and: [ selector = Scanner doItSelector or: [ selector = Scanner doItInSelector ]]) ifTrue: [
		self inform: 'DoIt and DoItIn: methods can not be changed'.
	 	^false].

	self selectedContext isExecutingBlock ifTrue: [
		h := self selectedContext activeHome.
		h ifNil: [
			self inform: 'Method for block not found on stack, can''t edit and continue'.
			^false].
		(self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse: [^false].
		self resetContext: h.
		(result := self contents: aText notifying: aController) ifTrue: [self acceptedContentsChanged].
		^result].
		
	selector := classOfMethod
				compile: aText
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.

	newMethod isQuick ifTrue: [
		contextStackIndex + 1 > contextStack size ifTrue: [
			self inform: 'Can not compile a quick method in the stack base context'.
			^false].
		self down.
		self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].

	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext
		ifFalse:
			[self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withNewLines]
		ifTrue:
			[newMethod isQuick ifFalse:
				[interruptedProcess
					restartTopWith: newMethod;
				 	stepToSendOrReturn].
			contextVariablesInspector object: nil].
	self resetContext: ctxt.
	^true! !

