'From Cuis6.0 [latest update: #6108] on 30 November 2023 at 3:49:33 pm'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 11/30/2023 15:20:00'!
updatesPrefix
	^updatesPrefix ifNil: ['']! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 11/30/2023 15:48:20'!
newUpdates: updatesFileDirectory do: aBlock
	| prefix updates |
	prefix := SystemVersion current updatesPrefix.
	updates := SortedCollection sortBlock: [ :a :b | a name < b name ].
	updatesFileDirectory filesDo: [ :fileEntry |
		(fileEntry name beginsWith: prefix) ifTrue: [
			(fileEntry name integerOrNilAt: prefix size + 1) ifNotNil: [ :updateNumber |
				updateNumber > SystemVersion current highestUpdate ifTrue: [
					updates add: fileEntry ]]]].
	updates do: aBlock.! !

!methodRemoval: SystemVersion #updatesPrefixesDo: stamp: 'jmv 11/30/2023 15:48:40'!
SystemVersion removeSelector: #updatesPrefixesDo:!
