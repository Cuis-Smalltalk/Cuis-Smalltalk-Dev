'From Cuis7.5 [latest update: #7597] on 22 September 2025 at 9:39:17 am'!

!ParseNode methodsFor: 'testing' stamp: 'jmv 9/22/2025 09:35:30'!
areNodesOf: leftNodes equivalentTo: rightNodes

	leftNodes size = rightNodes size ifFalse: [ ^false ].
	leftNodes with: rightNodes do: [ :leftNode :rightNode |
		leftNode isNil == rightNode isNil ifFalse: [ ^false ].
		(leftNode == rightNode "i.e. both nil or same" or: [
			leftNode equivalentTo: rightNode]) ifFalse: [
				^false ]].
	^true! !

