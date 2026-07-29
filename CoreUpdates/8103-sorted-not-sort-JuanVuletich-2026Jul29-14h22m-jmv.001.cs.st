'From Cuis7.9 [latest update: #8098] on 29 July 2026 at 2:24:01 pm'!

!CommonScopeNodeFinder methodsFor: 'initialization' stamp: 'JEC 16/Jan/2026, 11:08:24 am (UTC)'!
sortedByIntervalPositions: aCollectionOfNodes 

	^ aCollectionOfNodes asSortedCollection: [ :leftParseNode :rightParseNode | 
		leftParseNode value first < rightParseNode value first ]! !


!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'HAW 20/Dec/2018, 5:44:13 pm (UTC)'!
sortedByLastUsedTime: selectorsToShow

	^selectorsToShow asArray sort: [ :leftSelectorAndTime :rightSelectorAndTime |
		leftSelectorAndTime second > rightSelectorAndTime second ]! !


!CommonScopeNodeFinder methodsFor: 'initialization' stamp: 'JEC 16/Jan/2026, 11:07:53 am (UTC)'!
initializeForAll: aCollectionOfNodes in: aMethodNode

	parseNodesToFindCommonScope := self sortedByIntervalPositions: aCollectionOfNodes.
	methodNode := aMethodNode.
	actualScopeIndex := 0.
	nodesWithScopeFound := OrderedCollection new.
	scopesCollection := OrderedCollection new.
! !


!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'HAW 20/Dec/2018, 5:49:34 pm (UTC)'!
add: selector and: lastUsedTime whenNotFullTo: selectorsToShow
			
	selectorsToShow add: { selector . lastUsedTime }.
	
	^selectorsToShow size = EntriesLimit 
		ifTrue: [ self sortedByLastUsedTime: selectorsToShow ]
		ifFalse: [ selectorsToShow ]
	! !

!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'NPM 17/Mar/2020, 2:09:03 am (UTC)'!
computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: aCollection

	| selectorsToShow |
	selectorsToShow := OrderedCollection new.
	
	self class protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :selector :lastUsedTime |
				selectorsToShow := self add: selector and: lastUsedTime to: selectorsToShow.
				(Object canUnderstand: selector) ifFalse: [ aCollection add: selector ]]].
				
	selectorsToShow size < EntriesLimit ifTrue: [ selectorsToShow :=  self sortedByLastUsedTime: selectorsToShow ].
	
	" To see the timestamps in the menu (need to tweak #insertCompletion: to activate. Right now, just for debugging)
	entries := selectorsToShow collect: [ :ary | ary first, '(', ((DateAndTime fromString: '01/01/1996 00:00') + ary second minutes) printString,')' ]
	"
	^ selectorsToShow collect: [ :selectorAndTime | selectorAndTime first ]
	
! !

!methodRemoval: SmalltalkCompleter #sortByLastUsedTime: stamp: 'jmv 29/Jul/2026, 5:23:38 pm (UTC)'!
SmalltalkCompleter removeSelector: #sortByLastUsedTime:!
!methodRemoval: CommonScopeNodeFinder #sortByIntervalPositions: stamp: 'jmv 29/Jul/2026, 5:23:00 pm (UTC)'!
CommonScopeNodeFinder removeSelector: #sortByIntervalPositions:!
