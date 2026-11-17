'From Cuis7.9 [latest update: #8373] on 17 November 2026 at 9:49:58 am'!

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 17/Nov/2026, 12:36:44 pm (UTC)'!
classRemoved: aClass fromCategory: aCategoryName priorDefinition: priorClassDefinitionString

	self
		triggerEvent: #classRemoved
		withArguments: { aClass . aCategoryName. priorClassDefinitionString }! !


!ChangeSet methodsFor: 'class changes' stamp: 'jmv 17/Nov/2026, 12:38:27 pm (UTC)'!
noteRemovalOf: class fromCategory: aCategoryName priorDefinition: priorClassDefinitionString
	"The class is about to be removed from the system.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteRemoved: class
		fromCategory: aCategoryName
		priorDefinition: priorClassDefinitionString.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true.! !


!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 17/Nov/2026, 12:35:09 pm (UTC)'!
classRemoved: aClass fromCategory: aCategoryName priorDefinition: priorClassDefinitionString

	| packageOrNil |
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	packageOrNil := CodePackage packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			noteRemovalOf: aClass
			fromCategory: aCategoryName
			priorDefinition: priorClassDefinitionString ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ].! !


!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 17/Nov/2026, 12:35:12 pm (UTC)'!
classRemoved: aClass fromCategory: aCategoryName priorDefinition: priorClassDefinitionString

	self protected: [
		aClass selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: { aClass . aClass class }) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]].
		aClass class selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: { aClass . aClass class }) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]]]! !


!ClassChangeRecord methodsFor: 'all changes' stamp: 'jmv 17/Nov/2026, 12:37:45 pm (UTC)'!
noteRemoved: class fromCategory: aCategoryName priorDefinition: priorClassDefinitionString

	priorDefinition := priorClassDefinitionString.
	self noteChangeType: #remove fromClass: class
! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 17/Nov/2026, 12:44:33 pm (UTC)'!
forgetClass: aClass logged: aBool 
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	| priorClassCategory priorClassDefinition |
	
	priorClassCategory := aClass category.
	priorClassDefinition := aClass definition.
	
	"Log to .changes is done before the action."
	aBool ifTrue: [Smalltalk logClassRemoval: aClass].
	
	SystemOrganization removeElement: aClass name.
	
	"Notifications are triggered after the action."
	aBool ifTrue: [
		SystemChangeNotifier uniqueInstance
			classRemoved: aClass
			fromCategory: priorClassCategory
			priorDefinition: priorClassDefinition ].
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: nil.
	self flushClassNameCache.! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 17/Nov/2026, 12:45:45 pm (UTC)'!
initializeNotifications
	"
	ChangeSet initializeNotifications
	"

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to:oldClassDefinition: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory:priorDefinition: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self.! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 17/Nov/2026, 12:46:08 pm (UTC)'!
initializeNotifications
	"
	SmalltalkCompleter initializeNotifications
	"
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory:priorDefinition: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	ChangeSet initializeNotifications.
	SmalltalkCompleter initializeNotifications.!

