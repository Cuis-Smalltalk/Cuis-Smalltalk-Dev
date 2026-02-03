'From Cuis7.5 [latest update: #7663] on 11 January 2026 at 10:34:17 am'!

!MethodNode methodsFor: 'source mapping' stamp: 'JEC 10/27/2025 18:21:41'!
parseNodeIntervalIncluding: aPosition ifAbsent: aBlockClosure 
	
	"This message returns the interval associated with the parse node"
	
	| parseNode |
	
	parseNode := self parseNodeIncluding: aPosition ifAbsent: [ ^ aBlockClosure value ].
	^ parseNode value! !


!ChangeKeywordsSelectorOrder methodsFor: 'rename senders - private' stamp: 'JEC 10/27/2025 19:41:02'!
addBlanksOf: oldArgument to: newArgument

	| blanksPrefix blanksSuffix |

	"At least one blank must exist at the beginning and at the end. Otherwise, is part of the keyword 
	and not part of the argument"
	blanksPrefix := oldArgument copyFrom: 1 to: oldArgument firstNonSeparator - 1.
	blanksSuffix := oldArgument copyFrom: oldArgument lastNonSeparator + 1 to: oldArgument size.
	^ blanksPrefix , newArgument withBlanksTrimmed , blanksSuffix! !

!ChangeKeywordsSelectorOrder methodsFor: 'rename senders - private' stamp: 'JEC 10/27/2025 19:41:51'!
addNewRangesToArgumentsOf: originalRangesToArguments to: rangesToNewStrings

	originalRangesToArguments withIndexDo: [ :rangeToArgument :index | 
		| newArgument newArgumentIndex |

		newArgumentIndex := changedOrder at: index.
		
		"Adds the same blanks as the argument in that position had"
		newArgument :=  self 
			addBlanksOf: rangeToArgument value 
			to: (originalRangesToArguments at: newArgumentIndex) value.
		
		rangesToNewStrings add: (rangeToArgument key ->  newArgument) ]! !

!ChangeKeywordsSelectorOrder methodsFor: 'rename senders - private' stamp: 'JEC 11/4/2025 16:48:19'!
addRangesToArgumentsOf: sourceCodeToEdit to: rangesToNewStrings using: aMethodNode

	| originalRangesToArguments |

	originalRangesToArguments := self 
		originalRangesToArgumentsOf: sourceCodeToEdit 
		using: rangesToNewStrings
		and: aMethodNode.
	self addNewRangesToArgumentsOf: originalRangesToArguments to: rangesToNewStrings.! !

!ChangeKeywordsSelectorOrder methodsFor: 'rename senders - private' stamp: 'JEC 11/4/2025 16:49:01'!
originalRangesToArgumentsOf: sourceCodeToEdit using: rangesToNewStrings and: aMethodNode

	| originalRangesToArguments parseNodeInterval |

	originalRangesToArguments := SortedCollection sortBlock: [ :left :right | left key first < right key first ].
	parseNodeInterval := aMethodNode 
		parseNodeIntervalIncluding: rangesToNewStrings first key first 
		ifAbsent: [ self shouldNotHappen ].
	
	rangesToNewStrings withNextDo: [ :rangeToKeyword :prevRangeToKeyword |  
		| range argumentRange |
		
		range := rangeToKeyword key.
		argumentRange := prevRangeToKeyword
			ifNil: [ range last + 1 to: parseNodeInterval last ]
			ifNotNil: [ range last + 1 to: prevRangeToKeyword key first - 1 ].
			
		originalRangesToArguments add: argumentRange -> (sourceCodeToEdit atAll: argumentRange) ].
	
	^ originalRangesToArguments ! !

!ChangeKeywordsSelectorOrder methodsFor: 'rename senders - private' stamp: 'JEC 10/29/2025 18:08:35'!
rangesToKeywordsUsing: messageSendSelectorKeywordRanges

	| rangesToKeywords |

	rangesToKeywords := SortedCollection sortBlock: [ :left :right | left key first < right key first ].
	
	"This is needed because this message expects an OrderedCollection of a Collection"
	self 
		addRangesOf: (OrderedCollection new add: messageSendSelectorKeywordRanges; yourself) 
		to: rangesToKeywords.
	
	^ rangesToKeywords! !

!ChangeKeywordsSelectorOrder methodsFor: 'rename senders - private' stamp: 'JEC 10/28/2025 18:07:24'!
renameSendersIn: aMethod

	| sourceCodeToEdit |

	sourceCodeToEdit := self sourceCodeWithRenamedSendersOf: aMethod notOptimizedMethodNode.
	aMethod methodClass compile: sourceCodeToEdit.

	^changes add: (MethodReference method: aMethod)! !

!ChangeKeywordsSelectorOrder methodsFor: 'rename senders - private' stamp: 'JEC 11/4/2025 16:47:23'!
sourceCodeWithRenamedSendersOf: aMethodNode

	| sendSelectorPositions sourceCodeToEdit |

	sendSelectorPositions := self messageSendSelectorKeywordPositionsIn: aMethodNode.
	sourceCodeToEdit := aMethodNode sourceText.

	"sendSelectorPositions is ordered taking the first value of the first interval. The problem using that
	is that a method collaboration could be an argument of itself. Therefore, iterating the other way around
	will replace the collaborations inside of another collaborations of same method if any."
	sendSelectorPositions reverseDo: [ :messageSendSelectorKeywordRanges | | rangesToNewStrings |
		rangesToNewStrings := self rangesToKeywordsUsing: messageSendSelectorKeywordRanges.
		self addRangesToArgumentsOf: sourceCodeToEdit to: rangesToNewStrings using: aMethodNode.
		sourceCodeToEdit := sourceCodeToEdit copyReplacing: rangesToNewStrings ].

	^ sourceCodeToEdit! !


!RefactoringApplier class methodsFor: 'appliers - registering' stamp: 'JEC 1/11/2026 10:25:35'!
registerChangeKeywordsSelectorOrderApplier: aChangeKeywordsSelectorOrderApplierClass

	self 
		registerApplierAt: self changeKeywordsSelectorOrderApplierId 
		with: aChangeKeywordsSelectorOrderApplierClass! !

!RefactoringApplier class methodsFor: 'appliers - getting' stamp: 'JEC 1/11/2026 10:25:53'!
changeKeywordsSelectorOrderApplier
	
	^self 
		applierAt: self changeKeywordsSelectorOrderApplierId 
		ifAbsent: [ ChangeKeywordsSelectorOrderApplier ]! !

!RefactoringApplier class methodsFor: 'appliers - id' stamp: 'JEC 1/11/2026 10:26:03'!
changeKeywordsSelectorOrderApplierId

	^ #changeKeywordsSelectorOrderApplier! !

!RefactoringApplier class methodsFor: 'appliers - resetting' stamp: 'JEC 1/11/2026 10:26:22'!
resetChangeKeywordsSelectorOrderApplier
	
	self resetApplierAt: self changeKeywordsSelectorOrderApplierId! !


!Browser methodsFor: 'refactorings' stamp: 'JEC 1/11/2026 10:27:02'!
changeKeywordOrder

	RefactoringApplier changeKeywordsSelectorOrderApplier applyFrom: self! !


!MethodSet methodsFor: 'refactorings' stamp: 'JEC 1/11/2026 10:27:44'!
changeKeywordOrder
	
	RefactoringApplier changeKeywordsSelectorOrderApplier applyFrom: self! !

!methodRemoval: ChangeKeywordsSelectorOrder #originalRangesToArgumentsOf:Using: stamp: 'JEC 10/25/2025 09:20:21'!
ChangeKeywordsSelectorOrder removeSelector: #originalRangesToArgumentsOf:Using:!
!methodRemoval: ChangeKeywordsSelectorOrder #addRangesToArgumentsOf:to: stamp: 'JEC 11/4/2025 16:47:39'!
ChangeKeywordsSelectorOrder removeSelector: #addRangesToArgumentsOf:to:!
!methodRemoval: ChangeKeywordsSelectorOrder #originalRangesToArgumentsUsing: stamp: 'JEC 10/25/2025 09:20:04'!
ChangeKeywordsSelectorOrder removeSelector: #originalRangesToArgumentsUsing:!
!methodRemoval: ChangeKeywordsSelectorOrder #rangesToArgumentsOf:using: stamp: 'JEC 10/22/2025 19:35:49'!
ChangeKeywordsSelectorOrder removeSelector: #rangesToArgumentsOf:using:!
!methodRemoval: ChangeKeywordsSelectorOrder #originalRangesToArgumentsOf:using: stamp: 'JEC 11/10/2025 20:52:16'!
ChangeKeywordsSelectorOrder removeSelector: #originalRangesToArgumentsOf:using:!
!methodRemoval: ChangeKeywordsSelectorOrder #rangesToArgumentsOf: stamp: 'JEC 10/22/2025 19:32:25'!
ChangeKeywordsSelectorOrder removeSelector: #rangesToArgumentsOf:!

!ChangeKeywordsSelectorOrder reorganize!
('create new implementors - private' addImplementorSelectorRanges:at:to:)
('initialization' initializeChangedOrder:)
('rename senders - private' addBlanksOf:to: addNewRangesToArgumentsOf:to: addRangesToArgumentsOf:to:using: originalRangesToArgumentsOf:using:and: rangesToKeywordsUsing: renameSendersIn: sourceCodeWithRenamedSendersOf:)
!

