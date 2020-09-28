'From Cuis 5.0 [latest update: #4382] on 27 September 2020 at 10:30:51 pm'!
"Change Set:		4383-CuisCore-AuthorName-2020Sep27-22h11m
Date:			27 September 2020
Author:			Nahuel Garbezza

Add missing visitor protocol (temporaries declaration) for ParseNodeEnumerator"!


!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'RNG 9/27/2020 22:25:46'!
visitTemporariesDeclarationNode: aTemporariesDeclarationNode

	(theSelectBlock isNil or: [theSelectBlock value: aTemporariesDeclarationNode]) ifFalse:
		[^nil].
	theBlock value: aTemporariesDeclarationNode.
	^ super visitTemporariesDeclarationNode: aTemporariesDeclarationNode! !

!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'RNG 9/27/2020 22:28:33'!
visitTemporaryDeclarationNode: aTemporaryDeclarationNode

	(theSelectBlock isNil or: [theSelectBlock value: aTemporaryDeclarationNode]) ifFalse:
		[^nil].
	theBlock value: aTemporaryDeclarationNode.
	^ super visitTemporaryDeclarationNode: aTemporaryDeclarationNode! !

