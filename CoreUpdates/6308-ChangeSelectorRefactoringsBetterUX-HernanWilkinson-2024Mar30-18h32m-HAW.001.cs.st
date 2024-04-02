'From Cuis6.3 [latest update: #6304] on 30 March 2024 at 8:06:08 pm'!

!CodeProvider methodsFor: 'message list' stamp: 'HAW 3/30/2024 20:02:10'!
selector: oldSelector refactoredTo: newSelector

	"Used by the refactorings when a selector is renamed, a parameter is added, etc. - Hernan"

	self subclassResponsibility ! !


!Browser methodsFor: 'message list' stamp: 'HAW 3/30/2024 20:02:10'!
selector: oldSelector refactoredTo: newSelector

	self selectedMessageName = oldSelector ifTrue: [ self setSelector: newSelector ]! !


!MethodSet methodsFor: 'message list' stamp: 'HAW 3/30/2024 20:02:10'!
selector: oldSelector refactoredTo: newSelector

	self selectedMessageName = oldSelector ifTrue: [ self removeMessageFromBrowserKeepingLabel ]! !


!ChangeList methodsFor: 'message list' stamp: 'HAW 3/30/2024 20:03:41'!
selector: oldSelector refactoredTo: newSelector 
	
	! !


!ChangeSorter methodsFor: 'message list' stamp: 'HAW 3/30/2024 20:02:10'!
selector: oldSelector refactoredTo: newSelector

	! !


!Debugger methodsFor: 'context stack (message list)' stamp: 'HAW 3/30/2024 20:02:10'!
selector: oldSelector refactoredTo: newSelector

	self selectedMessageName = oldSelector ifTrue: [ self down ].
	self currentMethodRefactored ! !


!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 3/30/2024 20:02:10'!
informChangesToBrowser

	browser selector: oldSelector refactoredTo: refactoring newSelector
	! !

!methodRemoval: VersionsBrowser #selector:refactoredTo: stamp: 'HAW 3/30/2024 20:03:41'!
VersionsBrowser removeSelector: #selector:refactoredTo:!

