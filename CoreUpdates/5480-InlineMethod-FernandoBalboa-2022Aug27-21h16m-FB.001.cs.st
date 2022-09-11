'From Cuis 6.0 [latest update: #5466] on 27 August 2022 at 10:36:39 pm'!

!InlineMethodUsagesStepWindow class methodsFor: 'instance creation' stamp: 'FB 8/27/2022 22:32:15'!
openFrom: anInlineMethodApplier

	| window messageSendsSet |

	messageSendsSet := InlineMethodMessageSendsSet applier: anInlineMethodApplier.
	
	messageSendsSet autoSelectString: anInlineMethodApplier implementors first selector.

	window := self open: messageSendsSet label: 'Message sends of #', anInlineMethodApplier selectorToInline, ' to Refactor'.
	window initializeFrom: anInlineMethodApplier.

	^window.! !


!MessageNodeReference methodsFor: 'accesing' stamp: 'FB 8/27/2022 22:24:30'!
messageSendsRangesOf: aSelector

	"To use polimorphysm with MethodReference - Fernando"

	^Array with: completeSourceRange! !

!methodRemoval: InlineMethodMessageSendsSet #messageSendsRangesOf: stamp: 'FB 8/27/2022 22:33:28'!
InlineMethodMessageSendsSet removeSelector: #messageSendsRangesOf:!
