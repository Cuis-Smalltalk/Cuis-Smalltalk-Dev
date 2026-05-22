Class {
	#name : 'TestResultWindow',
	#superclass : 'MethodSetWindow',
	#instVars : [
		'testResult'
	],
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'instance creation' }
TestResultWindow class >> methodReferencesOf: tests [

	^tests collect: [:aTest | MethodReference class: aTest class selector: aTest selector].

]

{ #category : 'instance creation' }
TestResultWindow class >> openFor: aTestResult [

	| window |

	window := self openMessageList: (self methodReferencesOf: aTestResult defects) label: aTestResult printString.
	window initializeFor: aTestResult.
	
	^window 


]

{ #category : 'GUI building' }
TestResultWindow >> addButton: button to: row color: buttonColor [

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10
]

{ #category : 'GUI building' }
TestResultWindow >> addButtonsTo: row color: buttonColor [

	self addButton: self createDebugButton to: row color: buttonColor.
	self addButton: self createReRunButton to: row color: buttonColor.
	
]

{ #category : 'GUI building' }
TestResultWindow >> buildLowerPanes [

	| codeAndButtons  |

	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons 
]

{ #category : 'GUI building' }
TestResultWindow >> buttonsRow [

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	
]

{ #category : 'button creation' }
TestResultWindow >> createDebugButton [

	^PluggableButtonMorph 
		model: self
		stateGetter: #isMessageSelected
		action: #debug
		label: 'Debug'.

]

{ #category : 'button creation' }
TestResultWindow >> createReRunButton [

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #runSuite
		label: 'Run Suite'.

]

{ #category : 'actions' }
TestResultWindow >> debug [

	model selection ifNotNil: [ :selection | | test |
		test := selection actualClass selector: selection selector.
		test debug.
		testResult removeFromDefectsAndAddToPassed: test.
		model removeMessageFromBrowserKeepingLabel. 
		self setLabel: testResult printString ]
]

{ #category : 'initialization' }
TestResultWindow >> initializeFor: aTestResult  [

	testResult := aTestResult 
]

{ #category : 'testing' }
TestResultWindow >> isMessageSelected [

	^model selection notNil 
]

{ #category : 'actions' }
TestResultWindow >> runSuite [

	| suite |
	 
	suite := TestSuite new.
	suite addTests: testResult tests.
	self delete.
	(ProgressiveTestRunner for: suite) value.
	
]
