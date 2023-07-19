'From Cuis 6.0 [latest update: #5935] on 19 July 2023 at 7:47:12 pm'!

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:45:17'!
buildInitialKeywordSelectorAnswer: parseNodesToParameterize

	^ Text streamContents: [ :stream |
		parseNodesToParameterize 
			withIndexDo: [ :parseNode :index | 
				stream
					nextPutAll: (self formatAsKeyword: 'm', index printString, ': ');
					nextPutAll: (self formatAsMethodArgument: parseNode name) ]
			separatedBy: [ stream space ]]! !


!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/19/2023 19:43:16'!
requestNewMessage

	| parseNodesToParameterize initialAnswer userAnswer  caption |

	parseNodesToParameterize := self parseNodesToParameterize.
	initialAnswer := self buildInitialSelectorAnswer: parseNodesToParameterize.
	caption := `'New method name:' , String newLineString, '(Reorder parameters with same names or rename them with new names)'`.
	userAnswer := self request: caption initialAnswer: initialAnswer.

	parseNodesToParameterize
		ifEmpty: [ self saveUnarySelector: userAnswer ]
		ifNotEmpty: [ self saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize ].

	^self buildNewMessage.
	! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 7/19/2023 19:44:55'!
buildInitialSelectorAnswer: parseNodesToParameterize
	"builds a selector with the shape of #m1 if unary, or #m1: something m2: else if it has args"

	^ parseNodesToParameterize
		ifEmpty: [ self formatAsKeyword: 'm1' ]
		ifNotEmpty: [ self buildInitialKeywordSelectorAnswer: parseNodesToParameterize]! !

