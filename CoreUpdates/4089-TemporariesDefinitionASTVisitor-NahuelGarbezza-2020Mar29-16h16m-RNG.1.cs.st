'From Cuis 5.0 [latest update: #4062] on 29 March 2020 at 4:26:28 pm'!
"Change Set:		4063-CuisCore-AuthorName-2020Mar29-16h16m
Date:			29 March 2020
Author:			Nahuel Garbezza

Allow ParseNodeVisitor to visit temporary declaration nodes"!


!ParseNodeVisitor methodsFor: 'private - visiting' stamp: 'RNG 3/29/2020 16:23:39'!
visitIfNotNil: aParseNode
	"RNG: this was implemented to support unexpected nil temporary declarations
	(coming from instances of MethodNode and BlockNode that were living in the image
	before the new parse nodes were introduced)"

	aParseNode ifNotNil: [ aParseNode accept: self ]! !


!ParseNodeVisitor methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:22:29'!
visitBlockNode: aBlockNode

	self visitIfNotNil: aBlockNode temporariesDeclaration.
	aBlockNode statements do:
		[ :statement| statement accept: self ]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:25:47'!
visitMethodNode: aMethodNode

	self visitIfNotNil: aMethodNode temporariesDeclaration.
	aMethodNode block accept: self! !


!ParseNodeVisitor reorganize!
('visiting' visitAssignmentNode: visitBlockNode: visitBraceNode: visitCascadeNode: visitInstanceVariableNode: visitLiteralNode: visitLiteralVariableNode: visitMessageNode: visitMessageNodeInCascade: visitMethodNode: visitNewArrayNode: visitRemoteTempVectorNode: visitReturnNode: visitSelectorNode: visitTempVariableNode: visitTemporariesDeclarationNode: visitTemporaryDeclarationNode: visitUndeclaredVariableNode: visitVariableNode:)
('private - visiting' visitIfNotNil:)
!

