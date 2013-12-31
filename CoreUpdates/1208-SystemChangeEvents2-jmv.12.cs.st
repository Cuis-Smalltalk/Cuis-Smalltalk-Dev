'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 18 March 2012 at 6:28:37 pm'!

!Object methodsFor: 'events-triggering' stamp: 'jmv 3/17/2012 21:23'!
                              triggerEvent: anEventSelector
	"Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action."

	^(self actionForEvent: anEventSelector) value! !

!Object methodsFor: 'events-triggering' stamp: 'jmv 3/17/2012 21:21'!
                              triggerEvent: anEventSelector with: anObject

    ^self 
		triggerEvent: anEventSelector
		withArguments: {anObject}! !

!Object methodsFor: 'events-triggering' stamp: 'jmv 3/17/2012 21:23'!
 triggerEvent: anEventSelector withArguments: anArgumentList

	^ (self actionForEvent: anEventSelector)
		valueWithArguments: anArgumentList! !


!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/17/2012 20:44'!
         event: anEvent
	"Hook for SystemChangeNotifier"

	anEvent itemKind == SystemChangeNotifier classKind ifTrue: [
		anEvent isRemoved ifTrue: [
			self noteRemovalOf: anEvent item].
		anEvent isAdded ifTrue: [
			self noteAddClass: anEvent item].
		anEvent isModified ifTrue: [
			anEvent anyChanges ifTrue: [
				self noteChangeClass: anEvent item from: anEvent oldItem]].
		anEvent isCommented ifTrue: [
			self noteCommentClass: anEvent item].
		anEvent isRenamed ifTrue: [
			self noteRenameClass: anEvent item as: anEvent newName].
		anEvent isReorganized ifTrue: [
			self noteReorganizeClass: anEvent item].
		anEvent isRecategorized ifTrue: [
			self noteChangeClassCategory: anEvent itemClass]
	].

	anEvent itemKind == SystemChangeNotifier methodKind ifTrue: [
		anEvent isAdded ifTrue: [
			self
				noteNewMethod: anEvent item
				forClass: anEvent itemClass
				selector: anEvent itemSelector
				priorMethod: nil].
		anEvent isModified ifTrue: [
			self
				noteNewMethod: anEvent item
				forClass: anEvent itemClass
				selector: anEvent itemSelector
				priorMethod: anEvent oldItem].
		anEvent isRemoved ifTrue: [
			self
				noteRemoveSelector: anEvent itemSelector
				class: anEvent itemClass
				priorMethod: anEvent item
				lastMethodInfo: {anEvent item sourcePointer. anEvent itemProtocol}].
		anEvent isRecategorized ifTrue: [
			self noteReorganizeClass: anEvent itemClass].
	].! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/17/2012 23:29'!
                           noteAddClass: class
	"Include indication that a new class was created."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #new.
	self atClass: class add: #change.
	self addCoherency: class name! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/17/2012 20:42'!
                           noteChangeClass: class from: oldClass
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	class wantsChangeSetLogging ifFalse: [^ self].
	class isMeta 
		ifFalse: [self atClass: class add: #change]	"normal"
		ifTrue: [((self classChangeAt: class theNonMetaClass name) includes: #add) 
			ifTrue: [self atClass: class add: #add] 	"When a class is defined, the metaclass
				is not recorded, even though it was added.  A further change is
				really just part of the original add."
			ifFalse: [self atClass: class add: #change]].
	self addCoherency: class name.
	(self changeRecorderFor: class) notePriorDefinition: oldClass.
	self noteClassStructure: oldClass! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/17/2012 20:43'!
                    noteChangeClassCategory: class
	"Remember that a class definition has been changed. 
	Based on #changeClass:from:, but simplified knowing that only the class category actually changed."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #change.
	self addCoherency: class name! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/17/2012 20:37'!
         noteRemoveSelector: selector class: class priorMethod: priorMethod lastMethodInfo: info
	"Include indication that a method has been forgotten.
	info is a pair of the source code pointer and message category
	for the method that was removed."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: info
! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/17/2012 20:43'!
    noteRenameClass: class as: newName 
	"Include indication that a class has been renamed."

	| recorder |
	(recorder _ self changeRecorderFor: class)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.
		
	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: class name.
	self noteClassStructure: class.

	recorder _ changeRecords at: class class name ifAbsent: [^ nil].
	changeRecords at: (newName, ' class') put: recorder.
	changeRecords removeKey: class class name.
	recorder noteNewName: newName , ' class'! !

!ChangeSet methodsFor: 'class changes' stamp: 'jmv 3/17/2012 20:44'!
                            noteCommentClass: class 
	"Include indication that a class comment has been changed."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #comment! !

!ChangeSet methodsFor: 'class changes' stamp: 'jmv 3/17/2012 20:41'!
             noteReorganizeClass: class 
	"Include indication that a class was reorganized."

	self atClass: class add: #reorganize! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:29'!
                  classAdded: aClass inCategory: aCategoryName

	self noteAddClass: aClass! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:34'!
classCommented: aClass

	self noteCommentClass: aClass! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:34'!
                  classDefinitionChangedFrom: oldClass to: newClass

	(newClass superclass ~~ oldClass superclass
		or: [ newClass instVarNames ~= oldClass instVarNames
			or: [ newClass classVarNames ~= oldClass classVarNames
				or: [ newClass sharedPools ~= oldClass sharedPools ]]])

		ifTrue: [
			self noteChangeClass: newClass from: oldClass ]! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:22'!
                            classRecategorized: aClass from: oldCategory to: newCategory! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:28'!
            classRemoved: aClass fromCategory: aCategoryName

	self noteRemovalOf: aClass! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:35'!
                           classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self noteRenameClass: aClass as: newClassName! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:35'!
      classReorganized: aClass

	self noteReorganizeClass: aClass! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:35'!
             methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor

	self
		noteNewMethod: aMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: nil! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:36'!
   methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	self
		noteNewMethod: aMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: nil! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:37'!
           methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	self
		noteNewMethod: newMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: oldMethod! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:38'!
     methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	self
		noteRemoveSelector: aSymbol
		class: aClass
		priorMethod: aMethod
		lastMethodInfo: {aMethod sourcePointer. protocol}! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 3/17/2012 23:39'!
                             selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	self noteReorganizeClass: aClass! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 3/17/2012 23:22'!
                           name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	instVars _ Smalltalk actualScannerClass new scanFieldNames: instVarString.
	classVars _ (Smalltalk actualScannerClass new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass _ Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"
	copyOfOldClass _ oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew _ self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined!! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class"
		newClass _ self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass _ oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force _ (newClass declare: classVarString) | (newClass sharing: poolString).

	"... classify ..."
	newCategory _ category asSymbol.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	newClass _ self recompile: force from: oldClass to: newClass mutate: false.

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: category].
	^newClass! !


!ClassDescription methodsFor: 'organization' stamp: 'jmv 3/17/2012 23:21'!
              category: cat 
	"Categorize the receiver under the system category, cat, removing it from 
	any previous categorization."

	| oldCat |
	oldCat := self category.
	(cat isKindOf: String)
		ifTrue: [SystemOrganization classify: self name under: cat asSymbol]
		ifFalse: [self errorCategoryName].
	SystemChangeNotifier uniqueInstance classRecategorized: self from: oldCat to: cat asSymbol! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/17/2012 23:10'!
                  classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header file oldCommentRemoteStr |
	(aString isKindOf: RemoteString) ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr _ self organization commentRemoteStr.
	(aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [(file _ SourceFiles at: 2) ifNotNil:
		[file setToEnd; newLine; nextPut: $!!.	"directly"
		"Should be saying (file command: 'H3') for HTML, but ignoring it here"
		header _ String streamContents: [:strm | strm nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm.
			strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
		file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	SystemChangeNotifier uniqueInstance classCommented: self.
! !


!AbstractEvent class methodsFor: 'accessing' stamp: 'jmv 3/17/2012 21:37'!
              eventSelectorBlock

	^ [ :itemKind :changeKind |
		itemKind, changeKind, 'Event:' ]! !

!AbstractEvent class methodsFor: 'accessing' stamp: 'jmv 3/17/2012 21:32'!
                             itemChangeCombinations

	^self supportedKinds collect: [ :itemKind |
		self eventSelectorBlock value: itemKind value: self changeKind ]! !

!AbstractEvent class methodsFor: 'accessing' stamp: 'jmv 3/17/2012 21:35'!
         systemEvents
	"Return all the possible events in the system. Make a cross product of 
	the items and the change types."
	"
	self systemEvents
	 an OrderedCollection('classRemovedEvent:' 'methodRemovedEvent:' 'categoryRemovedEvent:' 'protocolRemovedEvent:' 'classRecategorizedEvent:' 'methodRecategorizedEvent:' 'classCommentedEvent:' 'classModifiedEvent:' 'methodModifiedEvent:' 'categoryModifiedEvent:' 'protocolModifiedEvent:' 'classAddedEvent:' 'methodAddedEvent:' 'categoryAddedEvent:' 'protocolAddedEvent:' 'expressionDoItEvent:' 'classReorganizedEvent:' 'classRenamedEvent:' 'categoryRenamedEvent:' 'protocolRenamedEvent:' 'classModifiedEvent:')
	"

	^self allSubclasses
		inject: OrderedCollection new
		into: [ :allEvents :eventClass | allEvents addAll: eventClass itemChangeCombinations; yourself]! !


!ChangeSet class methodsFor: 'current changeset' stamp: 'jmv 3/18/2012 18:27'!
     newChanges: aChangeSet
	"Set the system ChangeSet to be the argument, aChangeSet.  Tell the current project that aChangeSet is now its change set.  When called from Project enter:, the setChangeSet: call is redundant but harmless; when called from code that changes the current-change-set from within a project, it's vital"

	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: current.
	current _ aChangeSet.
	SystemChangeNotifier uniqueInstance notify: aChangeSet ofAllSystemChangesUsing: #event:.

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


!ClassOrganizer methodsFor: 'private' stamp: 'jmv 3/17/2012 23:23'!
              notifyOfChangedSelector: element from: oldCategory to: newCategory
	(self hasSubject and: [(oldCategory ~= newCategory)]) ifTrue: [
		SystemChangeNotifier uniqueInstance selectorRecategorized: element from: oldCategory to: newCategory inClass: self subject
	].! !


!ModifiedClassDefinitionEvent methodsFor: 'testing' stamp: 'jmv 3/17/2012 23:17'!
    anyChanges

	^ self isSuperclassModified
		or: [self areInstVarsModified
			or: [self areClassVarsModified
				or: [self areSharedPoolsModified]]]! !


!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/17/2012 22:51'!
                              trigger: event

	self triggerEvent: event eventSelector with: event! !

!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/17/2012 22:50'!
              triggerEvent: anEventSelector

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector ]! !

!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/17/2012 22:50'!
           triggerEvent: anEventSelector withArguments: anArgumentList

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector withArguments: anArgumentList ]! !

!SystemChangeNotifier methodsFor: 'private-event lists' stamp: 'jmv 3/17/2012 21:33'!
   systemEventsForItem: itemKind

	| selectorBlock |
	selectorBlock _ AbstractEvent eventSelectorBlock.
	^AbstractEvent allChangeKinds collect: [ :changeKind |
		selectorBlock value: itemKind value: changeKind]! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 22:53'!
                          classAdded: aClass inCategory: aCategoryName

	self trigger: (AddedEvent class: aClass category: aCategoryName).

	self
		triggerEvent: #classAdded
		withArguments: { aClass . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 22:54'!
          classCommented: aClass
	"A class with the given name was commented in the system."

	self trigger: (CommentedEvent class: aClass).

	self
		triggerEvent: #classCommented
		with: aClass! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 22:54'!
                 classDefinitionChangedFrom: oldClass to: newClass

	self trigger: (ModifiedClassDefinitionEvent classDefinitionChangedFrom: oldClass to: newClass).

	self
		triggerEvent: #classDefinitionChanged
		withArguments: { oldClass . newClass }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:20'!
                              classRecategorized: aClass from: oldCategory to: newCategory

	self trigger:
		(RecategorizedEvent
			class: aClass
			category: newCategory
			oldCategory: oldCategory).

	self
		triggerEvent: #classRecategorized
		withArguments: { aClass . oldCategory . newCategory }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 22:55'!
                            classRemoved: aClass fromCategory: aCategoryName 

	self trigger: (RemovedEvent class: aClass category: aCategoryName).

	self
		triggerEvent: #classRemoved
		withArguments: { aClass . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 22:56'!
 classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName 

	self trigger: (RenamedEvent 
				class: aClass
				category: aCategoryName
				oldName: oldClassName
				newName: newClassName).

	self
		triggerEvent: #classRenamed
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 22:59'!
    classReorganized: aClass

	self trigger: (ReorganizedEvent class: aClass).

	self triggerEvent: #classReorganized with: aClass! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:00'!
           evaluated: expression context: aContext

	self trigger: (DoItEvent
				expression: expression
				context: aContext).

	self
		triggerEvent: #evaluated
		withArguments: { expression . aContext }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:05'!
       methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor
	"A method with the given selector was added to aClass, but not put in a protocol."

	self trigger: (AddedEvent
				method: aMethod 
				selector: aSymbol
				class: aClass
				requestor: requestor).

	self
		triggerEvent: #methodAdded
		withArguments: { aMethod . aSymbol . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:06'!
                      methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor
	"A method with the given selector was added to aClass in protocol aCategoryName."

	self trigger: (AddedEvent
				method: aMethod
				selector: aSymbol
				protocol: aCategoryName
				class: aClass
				requestor: requestor).

	self
		triggerEvent: #methodAddedInProtocol
		withArguments: { aMethod . aSymbol . aCategoryName . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:07'!
          methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	self trigger: (ModifiedEvent
					methodChangedFrom: oldMethod
					to: newMethod
					selector: aSymbol 
					inClass: aClass
					requestor: requestor).

	self
		triggerEvent: #methodChanged
		withArguments: { oldMethod . newMethod . aSymbol . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:08'!
                           methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass 
	"A method with the given selector was removed from the class."

	self trigger: (RemovedEvent
				method: aMethod 
				selector: aSymbol
				protocol: protocol
				class: aClass).

	self
		triggerEvent: #methodRemoved
		withArguments: { aMethod . aSymbol . protocol . aClass }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:23'!
        selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	self trigger: (RecategorizedEvent 
				selector: selector
				method: (aClass compiledMethodAt: selector ifAbsent: nil)
				protocol: newCategory
				class: aClass
				oldProtocol: oldCategory).

	self
		triggerEvent: #selectorRecategorized
		withArguments: { selector . oldCategory . newCategory . aClass }! !

!SystemChangeNotifier methodsFor: 'public' stamp: 'jmv 3/17/2012 21:35'!
           notify: anObject ofAllSystemChangesUsing: oneArgumentSelector 
	"Notifies an object of any system changes."

	self 
		notify: anObject
		ofEvents: AbstractEvent systemEvents
		using: oneArgumentSelector! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/17/2012 20:24'!
                              event: anEvent
	"Hook for SystemChangeNotifier"

	anEvent itemKind == SystemChangeNotifier classKind ifTrue: [
		anEvent isRemoved ifTrue: [
			anEvent item acceptsLoggingOfCompilation 
				ifTrue: [
					self logChange: 'Smalltalk removeClassNamed: #' , anEvent item name]].
	].

	anEvent isDoIt 
		ifTrue: [
			self logChange: anEvent item].

	anEvent itemKind == SystemChangeNotifier methodKind ifTrue: [
		anEvent isRemoved ifTrue: [
			anEvent itemClass acceptsLoggingOfCompilation 
				ifTrue: [
					self logChange: anEvent itemClass name , ' removeSelector: #' , anEvent itemSelector]].
	].! !


!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 3/17/2012 20:31'!
             event: anEvent
	"Hook for SystemChangeNotifier"
	
	anEvent itemKind == SystemChangeNotifier classKind ifTrue: [
		anEvent isCommented ifTrue: [
			self noteMethodSubmission: #Comment forClass: anEvent item].
	].

	anEvent itemKind == SystemChangeNotifier methodKind ifTrue: [
		(anEvent isAdded or: [anEvent isModified]) ifTrue: [
			anEvent itemRequestor ifNotNil: [
				self noteMethodSubmission: anEvent itemSelector forClass: anEvent itemClass]].

		(anEvent isAdded or: [anEvent isModified]) ifTrue:[
			InMidstOfFileinNotification signal
				ifFalse: [
					Utilities changed: #recentMethodSubmissions]]
	]! !


!WeakActionSequence methodsFor: 'evaluating' stamp: 'jmv 3/17/2012 20:06'!
           value
	"Do the same as my parent, but make sure that all actions that do not  
	give errors are evaluated before resignaling the ones that gave errors  
	(giving the chance to clients to handle them)."

	^self valueStartingFrom: 1! !

!WeakActionSequence methodsFor: 'evaluating' stamp: 'jmv 3/17/2012 20:05'!
          valueStartingFrom: startIndex
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	"Note: I currently trap Halt, Error so that I am sure to get a Halt event in case of a Halt. This is being fixed in the exception system - when the fix is done it will be enough to capture only Error."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each value ]
			on: Halt, Error
			do: [:exc | 
				self valueStartingFrom: index + 1.
				exc pass]].
	^ answer! !

!WeakActionSequence methodsFor: 'evaluating' stamp: 'jmv 3/17/2012 20:06'!
                  valueWithArguments: anArray 
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	^self valueWithArguments: anArray startingFrom: 1! !

!WeakActionSequence methodsFor: 'evaluating' stamp: 'jmv 3/17/2012 20:06'!
                              valueWithArguments: anArray startingFrom: startIndex
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	"Note: I currently trap Halt, Error so that I am sure to get a Halt event in case of a Halt. This is being fixed in the exception system - when the fix is done it will be enough to capture only Error."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each valueWithArguments: anArray ]
			on: Halt, Error
			do: [:exc | 
				self valueWithArguments: anArray startingFrom: index + 1.
				exc pass]].
	^ answer! !

!methodRemoval: WeakMessageSend #asActionSequenceTrappingErrors!
WeakMessageSend removeSelector: #asActionSequenceTrappingErrors!
!methodRemoval: WeakActionSequenceTrappingErrors #asActionSequenceTrappingErrors!
WeakActionSequenceTrappingErrors removeSelector: #asActionSequenceTrappingErrors!
!methodRemoval: WeakActionSequenceTrappingErrors #value!
WeakActionSequenceTrappingErrors removeSelector: #value!
!methodRemoval: WeakActionSequenceTrappingErrors #valueStartingFrom:!
WeakActionSequenceTrappingErrors removeSelector: #valueStartingFrom:!
!methodRemoval: WeakActionSequenceTrappingErrors #valueWithArguments:!
WeakActionSequenceTrappingErrors removeSelector: #valueWithArguments:!
!methodRemoval: WeakActionSequenceTrappingErrors #valueWithArguments:startingFrom:!
WeakActionSequenceTrappingErrors removeSelector: #valueWithArguments:startingFrom:!

!WeakActionSequenceTrappingErrors reorganize!
('as yet unclassified')
!

!methodRemoval: WeakActionSequence #asActionSequenceTrappingErrors!
WeakActionSequence removeSelector: #asActionSequenceTrappingErrors!
!methodRemoval: SystemChangeNotifier class #resetUniqueInstance!
SystemChangeNotifier class removeSelector: #resetUniqueInstance!
!methodRemoval: SystemChangeNotifier #actionSequenceForEvent:!
SystemChangeNotifier removeSelector: #actionSequenceForEvent:!
!methodRemoval: SystemChangeNotifier #allSystemEvents!
SystemChangeNotifier removeSelector: #allSystemEvents!
!methodRemoval: SystemChangeNotifier #class:recategorizedFrom:to:!
SystemChangeNotifier removeSelector: #class:recategorizedFrom:to:!
!methodRemoval: SystemChangeNotifier #classCommented:inCategory:!
SystemChangeNotifier removeSelector: #classCommented:inCategory:!
!methodRemoval: SystemChangeNotifier #evaluated:!
SystemChangeNotifier removeSelector: #evaluated:!
!methodRemoval: SystemChangeNotifier #methodAdded:selector:inClass:!
SystemChangeNotifier removeSelector: #methodAdded:selector:inClass:!
!methodRemoval: SystemChangeNotifier #methodAdded:selector:inProtocol:class:!
SystemChangeNotifier removeSelector: #methodAdded:selector:inProtocol:class:!
!methodRemoval: SystemChangeNotifier #methodChangedFrom:to:selector:inClass:!
SystemChangeNotifier removeSelector: #methodChangedFrom:to:selector:inClass:!
!methodRemoval: SystemChangeNotifier #methodRemoved:selector:class:!
SystemChangeNotifier removeSelector: #methodRemoved:selector:class:!
!methodRemoval: SystemChangeNotifier #notify:ofSystemChangesOfChange:using:!
SystemChangeNotifier removeSelector: #notify:ofSystemChangesOfChange:using:!
!methodRemoval: SystemChangeNotifier #notify:ofSystemChangesOfItem:change:using:!
SystemChangeNotifier removeSelector: #notify:ofSystemChangesOfItem:change:using:!
!methodRemoval: SystemChangeNotifier #selector:recategorizedFrom:to:inClass:!
SystemChangeNotifier removeSelector: #selector:recategorizedFrom:to:inClass:!
!methodRemoval: SystemChangeNotifier #setBroadcasting!
SystemChangeNotifier removeSelector: #setBroadcasting!
!methodRemoval: SystemChangeNotifier #systemEventsForChange:!
SystemChangeNotifier removeSelector: #systemEventsForChange:!
!methodRemoval: SystemChangeNotifier #systemEventsForItem:change:!
SystemChangeNotifier removeSelector: #systemEventsForItem:change:!

!SystemChangeNotifier reorganize!
('initialize' initialize)
('private' notify:ofEvents:using: trigger: triggerEvent: triggerEvent:withArguments:)
('private-event lists' systemEventsForItem:)
('system triggers' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: evaluated:context: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('public' doSilently: isBroadcasting notify:ofAllSystemChangesUsing: notify:ofSystemChangesOfItem:using:)
!

!methodRemoval: ModifiedEvent class #classDefinitionChangedFrom:to:!
ModifiedEvent class removeSelector: #classDefinitionChangedFrom:to:!
!methodRemoval: ChangeSet #addClass:!
ChangeSet removeSelector: #addClass:!
!methodRemoval: ChangeSet #changeClass:from:!
ChangeSet removeSelector: #changeClass:from:!
!methodRemoval: ChangeSet #changeClassCategory:!
ChangeSet removeSelector: #changeClassCategory:!
!methodRemoval: ChangeSet #commentClass:!
ChangeSet removeSelector: #commentClass:!
!methodRemoval: ChangeSet #removeSelector:class:priorMethod:lastMethodInfo:!
ChangeSet removeSelector: #removeSelector:class:priorMethod:lastMethodInfo:!
!methodRemoval: ChangeSet #renameClass:as:!
ChangeSet removeSelector: #renameClass:as:!
!methodRemoval: ChangeSet #reorganizeClass:!
ChangeSet removeSelector: #reorganizeClass:!

!ChangeSet reorganize!
('initialize-release' clear initialize isMoribund wither)
('change logging' event: noteAddClass: noteChangeClass:from: noteChangeClassCategory: noteNewMethod:forClass:selector:priorMethod: noteRemoveSelector:class:priorMethod:lastMethodInfo: noteRenameClass:as:)
('isolation layers' compileAll:from:)
('accessing' author classRemoves editPostscript hasPostscript methodChanges methodInfoFromRemoval: name name: printOn: removePostscript structures superclasses)
('testing' containsMethodAtPosition: isEmpty methodsWithoutClassifications okayToRemove okayToRemoveInforming:)
('method changes' adoptSelector:forClass: atSelector:class:put: changedMessageList changedMessageListAugmented hasAnyChangeForSelector: messageListForChangesWhich:ifNone: methodChangesAtClass: removeSelectorChanges:class: selectorsInClass:)
('class changes' changedClassCategories changedClassNames changedClasses classChangeAt: containsClass: fatDefForClass: noteClassForgotten: noteClassStructure: noteCommentClass: noteRemovalOf: noteReorganizeClass: trimHistory)
('moving changes' absorbClass:from: absorbMethod:class:from: absorbStructureOfClass:from: assimilateAllChangesFoundIn: expungeEmptyClassChangeEntries forgetAllChangesFoundIn: forgetChangesForClass:in: hasPreamble methodsWithAnyInitialsOtherThan: methodsWithInitialsOtherThan: methodsWithoutComments removeClassAndMetaClassChanges: removeClassChanges: removePreamble)
('fileIn/Out' askAddedInstVars: askRemovedInstVars: askRenames:addTo:using: assurePostscriptExists assurePreambleExists checkForConversionMethods checkForSlips defaultChangeSetDirectory fileOut fileOutChangesFor:on: fileOutMethodRemovalsFor:on: fileOutOn: fileOutPSFor:on: fileOutPostscriptOn: fileOutPreambleOn: objectForDataStream: postscriptString postscriptString: preambleString preambleString: preambleTemplate setPreambleToSay: summaryString summaryStringDelta: verboseFileOut)
('private' addCoherency: atClass:add: atClass:includes: atSelector:class: changeRecorderFor: fileOutClassDefinition:on: oldNameFor:)
('system change notifications' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
!

!methodRemoval: AbstractEvent #isCategoryKnown!
AbstractEvent removeSelector: #isCategoryKnown!
!methodRemoval: AbstractEvent #isProtocolKnown!
AbstractEvent removeSelector: #isProtocolKnown!
!methodRemoval: AbstractEvent #itemCategory!
AbstractEvent removeSelector: #itemCategory!
!methodRemoval: AbstractEvent #itemExpression!
AbstractEvent removeSelector: #itemExpression!
!methodRemoval: AbstractEvent #itemExpression:!
AbstractEvent removeSelector: #itemExpression:!
!methodRemoval: AbstractEvent #itemMethod!
AbstractEvent removeSelector: #itemMethod!
!methodRemoval: AbstractEvent #itemMethod:!
AbstractEvent removeSelector: #itemMethod:!
!methodRemoval: AbstractEvent #trigger:!
AbstractEvent removeSelector: #trigger:!
