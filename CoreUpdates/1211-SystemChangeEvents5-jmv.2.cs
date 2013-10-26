'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 19 March 2012 at 9:23:56 am'!
!classDefinition: #SystemChangeNotifier category: #'System-Support'!
ActiveModel subclass: #SystemChangeNotifier
	instanceVariableNames: 'silenceLevel'
	classVariableNames: 'UniqueInstance'
	poolDictionaries: ''
	category: 'System-Support'!

!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 09:00'!
 initialize
	"
	ChangeSet initialize
	"
	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.

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

!ChangeSet class methodsFor: 'current changeset' stamp: 'jmv 3/19/2012 09:01'!
                         newChanges: aChangeSet
	"Set the system ChangeSet to be the argument, aChangeSet."

	current _ aChangeSet.! !

ChangeSet initialize!
