'From Cuis7.9 [latest update: #8045] on 3 July 2026 at 4:00:14 pm'!

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 5/Feb/2026, 12:23:55 am (UTC)'!
addToEquivalentsNodesPartiallyOf: aCascadeNode 

	| intervalToAdd cascadeNodeSourceRange |
	
	cascadeNodeSourceRange := self completeSourceRangeOf: aCascadeNode.
	
	intervalToAdd := (cascadeNodeSourceRange includesAnyOf: selectionToKeep)
		ifTrue: [ selectionToKeep ]
		ifFalse: [
			(cascadeNodeSourceRange first 
				to: (cascadeNodeSourceRange first + selectionToKeep size - 1))
				asSourceCodeInterval ].

	foundNodes add: intervalToAdd! !

!methodRemoval: EquivalentNodesFinder #visitCascadeNode: stamp: 'jmv 3/Jul/2026, 7:59:44 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitCascadeNode:!
