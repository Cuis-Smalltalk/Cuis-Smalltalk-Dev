'From Cuis 5.0 [latest update: #4061] on 29 March 2020 at 4:05:51 pm'!
"Change Set:		4062-CuisCore-AuthorName-2020Mar29-15h59m
Date:			29 March 2020
Author:			Nahuel Garbezza

Add visitor protocol to temporary declaration nodes"!


!TemporariesDeclarationNode methodsFor: 'enumerating' stamp: 'RNG 3/29/2020 16:03:34'!
temporaryDeclarationNodesDo: aBlock

	self temporaryDeclarationNodes do: aBlock! !

!TemporariesDeclarationNode methodsFor: 'visiting' stamp: 'RNG 3/29/2020 15:59:33'!
accept: aVisitor

	^ aVisitor visitTemporariesDeclarationNode: self! !


!TemporaryDeclarationNode methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:00:46'!
accept: aVisitor

	^ aVisitor visitTemporaryDeclarationNode: self! !


!ParseNodeVisitor methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:02:42'!
visitTemporariesDeclarationNode: aTemporariesDeclarationNode

	aTemporariesDeclarationNode temporaryDeclarationNodesDo:
		[ :temporaryDeclarationNode | temporaryDeclarationNode accept: self ]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:01:42'!
visitTemporaryDeclarationNode: aTemporaryDeclarationNode! !


!TemporariesDeclarationNode reorganize!
('testing' declaresAnyVariable declaresVariable: isTemporariesDeclaration)
('enumerating' temporaryDeclarationNodesDo:)
('accessing' allDeclaredVariableNodes temporaryDeclarationNodes)
('printing' printEachTempVarDeclarationOn: printOn:indent:)
('initialization' initializeWithAll:)
('visiting' accept:)
!


!TemporaryDeclarationNode reorganize!
('testing' declaresVariable: isTemporariesDeclaration)
('printing' printOn:indent:)
('visiting' accept:)
('accessing' variableName variableNode)
('initialization' initializeVariableNode:)
!

