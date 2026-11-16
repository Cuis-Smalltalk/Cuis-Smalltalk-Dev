'From Cuis7.9 [latest update: #8357] on 13 November 2026 at 4:50:58 pm'!

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 13/Nov/2026, 7:47:58 pm (UTC)'!
removeSelector: selector 
	| priorMethod priorProtocol | 
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there."

	priorMethod := self compiledMethodAt: selector ifAbsent: [^ self].
	(self theNonMetaClass isOkToRemoveMethod: selector isMeta: self isMeta)
		ifFalse: [self error: 'Method removal not allowed'].
	priorProtocol := self whichCategoryIncludesSelector: selector.
	SystemChangeNotifier uniqueInstance doSilently: [
		self organization removeElement: selector ].
	super removeSelector: selector.
	CodeHistory current addMethodRemoval: priorMethod.
	SystemChangeNotifier uniqueInstance 
			methodRemoved: priorMethod selector: selector inProtocol: priorProtocol class: self.! !

!methodRemoval: CodeHistory #methodChangedFrom:to:selector:inClass: stamp: 'jmv 13/Nov/2026, 7:50:12 pm (UTC)'!
CodeHistory removeSelector: #methodChangedFrom:to:selector:inClass:!
!methodRemoval: CodeHistory #methodAdded:selector:inProtocol:class:methodTimeStamp: stamp: 'jmv 13/Nov/2026, 7:50:13 pm (UTC)'!
CodeHistory removeSelector: #methodAdded:selector:inProtocol:class:methodTimeStamp:!
!methodRemoval: CodeHistory #methodRemoved:selector:inProtocol:class: stamp: 'jmv 13/Nov/2026, 7:50:10 pm (UTC)'!
CodeHistory removeSelector: #methodRemoved:selector:inProtocol:class:!

!CodeHistory reorganize!
('initialization' initialize)
('add / update' addLibraryIndex:positionIndex:prevItem:nextItem: addMethodRemoval: addMethodVersion:libraryIndex:positionIndex:oldVersionOrNil: at:updateNextItem: at:updatePrevItem:)
('querying' libraryIndexAt: nextItemIndexAt: positionIndexAt: prevItemIndexAt:)
('services' methodSourceAndMetadataFor:into:)
!

