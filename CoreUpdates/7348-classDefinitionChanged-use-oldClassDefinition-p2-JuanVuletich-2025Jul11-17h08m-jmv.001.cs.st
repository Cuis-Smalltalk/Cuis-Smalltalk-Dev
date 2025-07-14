'From Cuis7.5 [latest update: #7347] on 11 July 2025 at 5:16:26 pm'!

!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 7/11/2025 17:15:40'!
initialize
	"
	ChangeSet initialize
	"
	AllChangeSets := OrderedCollection new.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to:oldClassDefinition: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self.! !

!methodRemoval: ClassChangeRecord #notePriorDefinition: stamp: 'jmv 7/11/2025 17:13:47'!
ClassChangeRecord removeSelector: #notePriorDefinition:!
!methodRemoval: ChangeSet class #classDefinitionChangedFrom:to: stamp: 'jmv 7/11/2025 17:12:14'!
ChangeSet class removeSelector: #classDefinitionChangedFrom:to:!
!methodRemoval: ChangeSet #classDefinitionChangedFrom:to: stamp: 'jmv 7/11/2025 17:12:21'!
ChangeSet removeSelector: #classDefinitionChangedFrom:to:!
!methodRemoval: ChangeSet #noteChangeClass:from: stamp: 'jmv 7/11/2025 17:13:45'!
ChangeSet removeSelector: #noteChangeClass:from:!
!methodRemoval: Inspector #classDefinitionChangedFrom:to: stamp: 'jmv 7/11/2025 17:08:02'!
Inspector removeSelector: #classDefinitionChangedFrom:to:!
ChangeSet initialize!
