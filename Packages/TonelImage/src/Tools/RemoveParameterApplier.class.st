Class {
	#name : 'RemoveParameterApplier',
	#superclass : 'ChangeSelectorApplier',
	#instVars : [
		'parameterToRemoveIndex',
		'parameterToRemoveName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RemoveParameterApplier class >> assertCanApplyRefactoringFor: aSelector in: aSelectedClass [

	RemoveParameter assertCanRemoveParameterFrom: aSelector.


	
]

{ #category : 'refactoring - parameters request' }
RemoveParameterApplier >> askKeywordToRemove [

	| keywords |

	keywords := oldSelector keywords.

	keywords size = 1
		ifTrue: [ parameterToRemoveIndex := 1 ]
		ifFalse: [ parameterToRemoveIndex := self selectKeywordIndexToRemoveFrom: keywords ].

	"Because I do not know the parameter name, I'll use this one as explanation - Hernan"
	parameterToRemoveName := 'Parameter related to keyword ', (keywords at: parameterToRemoveIndex) 
]

{ #category : 'refactoring - parameters request' }
RemoveParameterApplier >> askParameterToRemove [

	| methodNode parameterNames selectedMethod |

	"If the compiled method does not exist it means that the remove is being executed from the
	editor, in a message send therefore we can not ask for the parameter name unless we look for implementors or
	use LiveTyping to look for actual implementors, etc.
	To make it simpler, when we can know the parameter names, we use that. When we can not, we use the keyword
	names. I tried to used only keyword names but it is not so intuitive. I decided to use two different ways of asking
	instead of one (asking for keyword names) becuase I think the programmer prefers to see parameter names.

	It could happen that the selected class implements the message to remove the parameter but that the remove
	is executed from the editor (not sending to self), in that case the parameters of selected class implementation
	will be use... it is a rare case and I think it will not confuse the programmer  - Hernan"

	selectedMethod := selectedClass
		compiledMethodAt: oldSelector
		ifAbsent: [ ^self askKeywordToRemove ].

	methodNode := selectedMethod notOptimizedMethodNode.
	parameterNames := methodNode argumentNames.

	parameterToRemoveIndex := parameterNames size = 1
		ifTrue: [ 1 ]
		ifFalse: [ self selectParameterIndexToRemoveFrom: parameterNames ].

	parameterToRemoveName := parameterNames at: parameterToRemoveIndex.


	
]

{ #category : 'refactoring - creation' }
RemoveParameterApplier >> createRefactoring [

	^self refactoringClass
		atIndex: parameterToRemoveIndex
		named: parameterToRemoveName
		from: oldSelector
		implementors: implementors
		senders: senders 
]

{ #category : 'refactoring - creation' }
RemoveParameterApplier >> refactoringClass [

	^RemoveParameter
]

{ #category : 'refactoring - parameters request' }
RemoveParameterApplier >> requestRefactoringParameters [

	self askParameterToRemove
		
		
]

{ #category : 'refactoring - parameters request' }
RemoveParameterApplier >> selectKeywordIndexToRemoveFrom: keywords [

	| keywordIndex |

	keywordIndex := (PopUpMenu labelArray: keywords) startUpWithCaption: 'Select keyword related to parameter to remove'.
	keywordIndex = 0 ifTrue: [self endRequest ].

	^keywordIndex
]

{ #category : 'refactoring - parameters request' }
RemoveParameterApplier >> selectParameterIndexToRemoveFrom: parameterNames [

	| parameterIndex |

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Select parameter to remove'.
	parameterIndex = 0 ifTrue: [self endRequest ].

	^parameterIndex
]
