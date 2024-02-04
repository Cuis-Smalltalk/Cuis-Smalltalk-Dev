'From Cuis6.3 [latest update: #6205] on 3 February 2024 at 9:30:40 pm'!

!InlineMethod methodsFor: 'initialization' stamp: 'jmv 2/3/2024 21:30:21'!
methodToInline: aCompiledMethod usages: aCollectionOfMesageNodeReferences removingMethod: removingMethod

	methodToInline := aCompiledMethod.
	messageSendsToInline := aCollectionOfMesageNodeReferences.
	replacementsByMessageSend := Dictionary new.
	temporariesDeclarationsByNode := Dictionary new.
	temporariesToDeclareByInsertionPoint := Dictionary new.
	updatedSendersCode := Dictionary new: ((messageSendsToInline groupBy: [:aMessageNodeReference | aMessageNodeReference methodReference]) keys) size.
	methodNodeToInline := methodToInline notOptimizedMethodNode.
	implementorCompleteSourceRanges := methodNodeToInline completeSourceRanges.
	removeMethod := removingMethod.! !

