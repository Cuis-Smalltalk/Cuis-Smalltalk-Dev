'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 19 March 2012 at 8:58:54 am'!

!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:43'!
                  initialize
	"
	ChangeSet initialize
	"
	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	
	"Deregister any instance. The class is registered now."
	SystemChangeNotifier uniqueInstance removeActionsSatisfying: [ :action | action receiver class == self ].

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self! !

!ChangeSet class methodsFor: 'current changeset' stamp: 'jmv 3/19/2012 08:42'!
                   newChanges: aChangeSet
	"Set the system ChangeSet to be the argument, aChangeSet.  Tell the current project that aChangeSet is now its change set.  When called from Project enter:, the setChangeSet: call is redundant but harmless; when called from code that changes the current-change-set from within a project, it's vital"

	"Just for a while!!"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: current.
	current _ aChangeSet.! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:41'!
     initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue allImplemented |
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.
	
	self protected: [
		allImplemented _ Smalltalk allImplementedMessages.
		Selectors _ Trie new.
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				Selectors at: sel ifAbsentPut: [ 0 ].
				method messages do: [ :sentMsg |
					Selectors at: sentMsg put: (Selectors at: sentMsg ifAbsent: [ 0 ]) + 1 ]]].
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				self addSelector: sel method: method allImplemented: allImplemented]].
		""
	
		"The following might not be found in #messages. Give them maximum priority."
		maxSortValue _ SmallInteger maxVal.
		"From MessageNode>>#initialize"
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:) do: [ :sel |
				Selectors at: sel put: maxSortValue ].
	
		maxSortValue _ SmallInteger maxVal-1.
		"From SystemDictionary >>#recreateSpecialObjectsArray"
		(1 to: Smalltalk specialSelectorSize) do:  [ :i |
				Selectors at: (Smalltalk specialSelectorAt: i) put: maxSortValue ]]! !


!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:44'!
                         classAdded: aClass inCategory: aCategoryName

	self
		triggerEvent: #classAdded
		withArguments: { aClass . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
              classCommented: aClass
	"A class with the given name was commented in the system."

	self
		triggerEvent: #classCommented
		with: aClass! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
 classDefinitionChangedFrom: oldClass to: newClass

	self
		triggerEvent: #classDefinitionChanged
		withArguments: { oldClass . newClass }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
classRecategorized: aClass from: oldCategory to: newCategory

	self
		triggerEvent: #classRecategorized
		withArguments: { aClass . oldCategory . newCategory }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
          classRemoved: aClass fromCategory: aCategoryName

	self
		triggerEvent: #classRemoved
		withArguments: { aClass . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
        classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self
		triggerEvent: #classRenamed
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
        classReorganized: aClass

	self triggerEvent: #classReorganized with: aClass! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
                             evaluated: expression context: aContext

	self
		triggerEvent: #evaluated
		withArguments: { expression . aContext }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
                     methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor
	"A method with the given selector was added to aClass, but not put in a protocol."

	self
		triggerEvent: #methodAdded
		withArguments: { aMethod . aSymbol . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
          methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor
	"A method with the given selector was added to aClass in protocol aCategoryName."

	self
		triggerEvent: #methodAddedInProtocol
		withArguments: { aMethod . aSymbol . aCategoryName . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
                         methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	self
		triggerEvent: #methodChanged
		withArguments: { oldMethod . newMethod . aSymbol . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
                        methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass 
	"A method with the given selector was removed from the class."

	self
		triggerEvent: #methodRemoved
		withArguments: { aMethod . aSymbol . protocol . aClass }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:46'!
                            selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	self
		triggerEvent: #selectorRecategorized
		withArguments: { selector . oldCategory . newCategory . aClass }! !


!SystemChangeNotifier class methodsFor: 'instance creation' stamp: 'jmv 3/19/2012 08:52'!
                              new

	^self error: 'This is a singleton implementation, so you are not allowed to create instances yourself. Use #uniqueInstance to access the instance.'! !

!SystemChangeNotifier class methodsFor: 'public' stamp: 'jmv 3/19/2012 08:52'!
                   uniqueInstance

	UniqueInstance ifNil: [ UniqueInstance _ self  basicNew initialize ].
	^UniqueInstance! !


!SystemDictionary class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:41'!
                          startUp
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Smalltalk.
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk
	"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk! !


!Utilities class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:41'!
                     startUp
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Utilities.
	SystemChangeNotifier uniqueInstance
		when: #classCommented send: #classCommented: to: Utilities;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: Utilities;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: Utilities;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: Utilities
	"

	SystemChangeNotifier uniqueInstance
		when: #classCommented send: #classCommented: to: Utilities;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: Utilities;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: Utilities;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: Utilities! !

!methodRemoval: Utilities class #event:!
Utilities class removeSelector: #event:!
!methodRemoval: SystemDictionary #event:!
SystemDictionary removeSelector: #event:!
!methodRemoval: SystemChangeNotifier class #categoryKind!
SystemChangeNotifier class removeSelector: #categoryKind!
!methodRemoval: SystemChangeNotifier class #classKind!
SystemChangeNotifier class removeSelector: #classKind!
!methodRemoval: SystemChangeNotifier class #createInstance!
SystemChangeNotifier class removeSelector: #createInstance!
!methodRemoval: SystemChangeNotifier class #expressionKind!
SystemChangeNotifier class removeSelector: #expressionKind!
!methodRemoval: SystemChangeNotifier class #instanceCreationErrorString!
SystemChangeNotifier class removeSelector: #instanceCreationErrorString!
!methodRemoval: SystemChangeNotifier class #methodKind!
SystemChangeNotifier class removeSelector: #methodKind!
!methodRemoval: SystemChangeNotifier class #protocolKind!
SystemChangeNotifier class removeSelector: #protocolKind!

!SystemChangeNotifier class reorganize!
('instance creation' new)
('public' uniqueInstance)
!

!methodRemoval: SystemChangeNotifier #notify:ofAllSystemChangesUsing:!
SystemChangeNotifier removeSelector: #notify:ofAllSystemChangesUsing:!
!methodRemoval: SystemChangeNotifier #notify:ofEvents:using:!
SystemChangeNotifier removeSelector: #notify:ofEvents:using:!
!methodRemoval: SystemChangeNotifier #notify:ofSystemChangesOfItem:using:!
SystemChangeNotifier removeSelector: #notify:ofSystemChangesOfItem:using:!
!methodRemoval: SystemChangeNotifier #systemEventsForItem:!
SystemChangeNotifier removeSelector: #systemEventsForItem:!
!methodRemoval: SystemChangeNotifier #trigger:!
SystemChangeNotifier removeSelector: #trigger:!
!methodRemoval: SmalltalkCompleter class #classChanged:!
SmalltalkCompleter class removeSelector: #classChanged:!
!methodRemoval: SmalltalkCompleter class #methodChanged:!
SmalltalkCompleter class removeSelector: #methodChanged:!
SmalltalkCompleter initialize!
ChangeSet initialize!
!methodRemoval: ChangeSet #event:!
ChangeSet removeSelector: #event:!
!classRemoval: #AbstractEvent!
Smalltalk removeClassNamed: #AbstractEvent!
!classRemoval: #AddedEvent!
Smalltalk removeClassNamed: #AddedEvent!
!classRemoval: #CommentedEvent!
Smalltalk removeClassNamed: #CommentedEvent!
!classRemoval: #DoItEvent!
Smalltalk removeClassNamed: #DoItEvent!
!classRemoval: #ModifiedClassDefinitionEvent!
Smalltalk removeClassNamed: #ModifiedClassDefinitionEvent!
!classRemoval: #ModifiedEvent!
Smalltalk removeClassNamed: #ModifiedEvent!
!classRemoval: #RecategorizedEvent!
Smalltalk removeClassNamed: #RecategorizedEvent!
!classRemoval: #RemovedEvent!
Smalltalk removeClassNamed: #RemovedEvent!
!classRemoval: #RenamedEvent!
Smalltalk removeClassNamed: #RenamedEvent!
!classRemoval: #ReorganizedEvent!
Smalltalk removeClassNamed: #ReorganizedEvent!
!classRemoval: #WeakActionSequenceTrappingErrors!
Smalltalk removeClassNamed: #WeakActionSequenceTrappingErrors!
