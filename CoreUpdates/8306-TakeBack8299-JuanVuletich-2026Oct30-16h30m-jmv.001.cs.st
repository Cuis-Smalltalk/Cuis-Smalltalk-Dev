'From Cuis7.9 [latest update: #8305] on 30 October 2026 at 4:31:18 pm'!

!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 25/Nov/2010, 2:36:00 pm (UTC)'!
didCodeChangeElsewhere
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass aSelector aCompiledMethod |
	currentCompiledMethod ifNil: [^ false].
	(aClass := self selectedClassOrMetaClass) ifNil: [^ false].
	(aSelector := self selectedMessageName) ifNil: [^ false].

	aSelector == #Comment ifTrue:
		[^ currentCompiledMethod ~~ aClass organization commentRemoteStr].
	^ ((aCompiledMethod := aClass compiledMethodAt: aSelector ifAbsent: [^ false]) ~~ currentCompiledMethod)
		and: [aCompiledMethod last ~= 0 "either not yet installed"
				or: [currentCompiledMethod last = 0 "or these methods don't have source pointers"]]
	! !


!CodeFileBrowser methodsFor: 'other' stamp: 'jmv 27/Sep/2016, 11:05:48 pm (UTC)'!
didCodeChangeElsewhere
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass |
	(aClass := self selectedClassOrMetaClass) ifNil: [^ false].

	(aClass is: #PseudoClass) ifTrue: [^ false]. "class not installed"
	^super didCodeChangeElsewhere! !


!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 30/Oct/2026, 7:27:22 pm (UTC)'!
updateListsAndCode
	self didCodeChangeElsewhere
		ifTrue: [
			self acceptedContentsChanged]! !

