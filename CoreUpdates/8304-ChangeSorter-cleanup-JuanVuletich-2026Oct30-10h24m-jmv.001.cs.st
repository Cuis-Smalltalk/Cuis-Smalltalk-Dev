'From Cuis7.9 [latest update: #8302] on 30 October 2026 at 10:26:58 am'!

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 30/Oct/2026, 1:08:35 pm (UTC)'!
fileOutAndKeep
	"File out the current change set."

	myChangeSet fileOut.
	self updateListsAndCode.! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 30/Oct/2026, 1:08:38 pm (UTC)'!
fileOutAndRemove
	"File out the current change set."

	myChangeSet fileOut.
	self removePrompting: false.

	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self updateListsAndCode.! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 30/Oct/2026, 1:08:42 pm (UTC)'!
remove
	"Completely destroy my change set.  Check if it's OK first"

	myChangeSet isForBaseSystem ifTrue: [ 
		self removePrompting: true.
		self showChangeSet: ChangeSet changeSetForBaseSystem.
		self updateListsAndCode ].! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 30/Oct/2026, 1:08:55 pm (UTC)'!
renameWith: newName

	"I do not use renameTo: because it is defined in Obejct with a different semantic - Hernan"
	
	(newName = myChangeSet name or: [newName isEmpty])
		ifTrue: [Smalltalk beep]
		ifFalse:
			[(ChangeSet changeSetNamed: newName) notNil
				ifTrue: [self inform: 'Sorry that name is already used']
				ifFalse:
					[myChangeSet name: newName.
					self updateListsAndCode.
					self changed: #mainButtonName.
					self changed: #relabel]].! !

!ChangeSorter methodsFor: 'message list' stamp: 'jmv 30/Oct/2026, 1:08:51 pm (UTC)'!
removeSelectedMethod
	"Remove the selected msg from the system. Real work done by the 
	parent, a ChangeSorter"
	| confirmation sel |
	currentSelector
		ifNotNil: [
			confirmation := Smalltalk
				confirmRemovalOf: (sel := self selectedMessageName)
				on: self selectedClassOrMetaClass.
			confirmation = 3
				ifTrue: [^ self].
			self selectedClassOrMetaClass removeSelector: sel.
			self updateListsAndCode.
			confirmation = 2
				ifTrue: [Smalltalk browseAllCallsOn: sel]].! !

!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 30/Oct/2026, 1:08:20 pm (UTC)'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString := input string.
	(class := self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [codeString isEmpty]) ifTrue: [ ^ false].
		(codeString count: [:char | char == $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: codeString) notEmpty ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: codeString.
		self currentSelector: nil.  "forces update with no 'unsaved chages' feedback"
		^ true].
	oldSelector := self selectedMessageName.
	category := class organization categoryOfElement: oldSelector.
	selector := class compile: codeString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self updateListsAndCode.
	^ true! !

!methodRemoval: ChangeSorter #update stamp: 'jmv 30/Oct/2026, 1:26:43 pm (UTC)'!
ChangeSorter removeSelector: #update!
