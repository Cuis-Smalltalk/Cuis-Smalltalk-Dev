'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 19 March 2012 at 8:40:15 am'!

!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:16'!
                  initialize
	"
	ChangeSet initialize
	"
	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	
	"Deregister any instance. The class is registered now."
	SystemChangeNotifier uniqueInstance removeActionsSatisfying: [ :action | action receiver class == self ].

self flag: #event:.

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

!ChangeSet class methodsFor: 'current changeset' stamp: 'jmv 3/19/2012 08:25'!
                              newChanges: aChangeSet
	"Set the system ChangeSet to be the argument, aChangeSet.  Tell the current project that aChangeSet is now its change set.  When called from Project enter:, the setChangeSet: call is redundant but harmless; when called from code that changes the current-change-set from within a project, it's vital"

	"Just for a while!!"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: current.
	current _ aChangeSet.

false ifTrue: [
	SystemChangeNotifier uniqueInstance notify: aChangeSet ofAllSystemChangesUsing: #event:.
].

"
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: current;
		when: #classCommented send: #classCommented: to: current;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: current;
		when: #classRecategorized send: #classRecategorized:from:to: to: current;
		when: #classRemoved send: #classRemoved:fromCategory: to: current;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: current;
		when: #classReorganized send: #classReorganized: to: current;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: current;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: current;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: current;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: current;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: current
"! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:13'!
        classAdded: aClass inCategory: aCategoryName

	^current classAdded: aClass inCategory: aCategoryName! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:13'!
                              classCommented: aClass

	^current classCommented: aClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:13'!
          classDefinitionChangedFrom: oldClass to: newClass

	^current classDefinitionChangedFrom: oldClass to: newClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:13'!
                    classRecategorized: aClass from: oldCategory to: newCategory

	^current classRecategorized: aClass from: oldCategory to: newCategory! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:14'!
                              classRemoved: aClass fromCategory: aCategoryName

	^current classRemoved: aClass fromCategory: aCategoryName! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:14'!
                      classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	^current classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:14'!
                  classReorganized: aClass

	^current classReorganized: aClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:14'!
      methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor

	^current methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:14'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	^current methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:14'!
                methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	^current methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:14'!
                    methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	^current methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:15'!
selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	^current selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:16'!
                 initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue allImplemented |
	self flag: #event:.
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"
false ifTrue: [
	SystemChangeNotifier uniqueInstance
		notify: self ofSystemChangesOfItem: #method using: #methodChanged:;
		notify: self ofSystemChangesOfItem: #class using: #classChanged:.
].

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

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 07:56'!
                   classRemoved: aClass fromCategory: aCategoryName

	self protected: [
		aClass selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: aClass) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]].
		aClass class selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: aClass class) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]]]! !

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:01'!
                           methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor

	self protected: [
		 self addSelector: aSymbol method: aMethod allImplemented: nil ]! !

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 08:01'!
                       methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	self protected: [
		 self addSelector: aSymbol method: aMethod allImplemented: nil ]! !

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 3/19/2012 07:59'!
                               methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	self protected: [
		(Smalltalk isThereAnImplementorOf: aSymbol) not ifTrue: [
			Selectors removeKey: aSymbol ifAbsent: nil ]]! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 08:18'!
          classRemoved: aClass fromCategory: aCategoryName

	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			self logChange: 'Smalltalk removeClassNamed: #' , aClass name ]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 08:21'!
    evaluated: expression context: aContext

	self logChange: expression! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 08:22'!
     methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			self logChange: aClass name , ' removeSelector: #' , aSymbol ]! !


!SystemDictionary class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:30'!
  startUp
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Smalltalk.
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk
	"

"Volar todos los metodos no llamados en las clases involucradas. Volar la jerarquia espantosa AbstractEvent. Poner el Notifier en otro lado y volar la tonta categoria de clases"
false ifTrue: [
	SystemChangeNotifier uniqueInstance notify: Smalltalk ofAllSystemChangesUsing: #event:.
	].

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk! !


!Utilities class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:37'!
                     startUp
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Utilities.
	SystemChangeNotifier uniqueInstance
		when: #classCommented send: #classCommented: to: Utilities;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: Utilities;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: Utilities;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: Utilities
	"
false ifTrue: [
	SystemChangeNotifier uniqueInstance notify: self ofAllSystemChangesUsing: #event:.
].

	SystemChangeNotifier uniqueInstance
		when: #classCommented send: #classCommented: to: Utilities;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: Utilities;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: Utilities;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: Utilities! !

!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 3/19/2012 08:31'!
                  classCommented: aClass

	self noteMethodSubmission: #Comment forClass: aClass! !

!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 3/19/2012 08:37'!
                       methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor

	requestor ifNotNil: [
		self noteMethodSubmission: aSymbol forClass: aClass ].

	InMidstOfFileinNotification signal
		ifFalse: [
			Utilities changed: #recentMethodSubmissions ]! !

!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 3/19/2012 08:37'!
     methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	requestor ifNotNil: [
		self noteMethodSubmission: aSymbol forClass: aClass ].

	InMidstOfFileinNotification signal
		ifFalse: [
			Utilities changed: #recentMethodSubmissions ]! !

!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 3/19/2012 08:37'!
             methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor


	requestor ifNotNil: [
		self noteMethodSubmission: aSymbol forClass: aClass ].

	InMidstOfFileinNotification signal
		ifFalse: [
			Utilities changed: #recentMethodSubmissions ]! !

SmalltalkCompleter initialize!

!SmalltalkCompleter class reorganize!
('class initialization' addSelector:method:allImplemented: initialize)
('notifications' classChanged: methodChanged:)
('services' isThereAnImplementorOf: thatStartsCaseSensitive:)
('synchronization' protected:)
('system change notifications' classRemoved:fromCategory: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodRemoved:selector:inProtocol:class:)
!

ChangeSet initialize!

!ChangeSet class reorganize!
('class initialization' initialize)
('current changeset' current currentChangeSetString newChanges:)
('defaults' defaultChangeSetDirectory defaultName uniqueNameLike:)
('fileIn/Out' doWeFileOut:given:cache: superclassOrder:)
('instance creation' basicNewNamed: new)
('system change notifications' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Smalltalk.
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk.
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Utilities.
	SystemChangeNotifier uniqueInstance
		when: #classCommented send: #classCommented: to: Utilities;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: Utilities;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: Utilities;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: Utilities!

