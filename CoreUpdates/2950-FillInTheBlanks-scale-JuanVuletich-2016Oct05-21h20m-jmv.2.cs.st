'From Cuis 4.2 of 25 July 2013 [latest update: #2949] on 5 October 2016 at 10:21:49 pm'!

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/5/2016 22:18:26'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"

	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	result morphExtent: 18@5 * self sizeUnit.
	self addMorph: result position: 1@2 * self sizeUnit.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/5/2016 22:06:44'!
setQuery: queryString initialAnswer: initialAnswer acceptOnCR: acceptBoolean 
	response := initialAnswer.
	done := false.
	self removeAllMorphs.
	self createQueryTextMorph: queryString.
	self createAcceptButton.
	self createCancelButton.
	textPane := self createTextPaneAcceptOnCR: acceptBoolean! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/5/2016 22:11:20'!
sizeUnit
	^AbstractFont default height! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/5/2016 22:07:14'!
request: queryString initialAnswer: defaultAnswer centerAt: aPoint onCancelReturn: returnOnCancel acceptOnCR: acceptBoolean
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts.   If the user cancels, answer returnOnCancel."
	"
	FillInTheBlankMorph
		request: 'Type something, then type [Return].'
		initialAnswer: 'yo ho ho!!'
		centerAt: Display center
	"

	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		setQuery: queryString
		initialAnswer: defaultAnswer
		acceptOnCR: acceptBoolean.
	aFillInTheBlankMorph responseUponCancel: returnOnCancel.
	self runningWorld addMorph: aFillInTheBlankMorph centeredNear: aPoint.
	^ aFillInTheBlankMorph getUserResponse! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/5/2016 22:20:36'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	result morphExtent: 6@2 * self sizeUnit.
	self addMorph: result position: 2@7.5 * self sizeUnit // 1.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/5/2016 22:20:53'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	result morphExtent: 6@2 * self sizeUnit.
	self addMorph: result position: 12@7.5 * self sizeUnit // 1.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/5/2016 22:21:17'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	result morphExtent: 24@2 * self sizeUnit.
	self addMorph: result position: 2@0.5 * self sizeUnit // 1.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 10/5/2016 22:11:55'!
initialize

	super initialize.
	extent _ 20@10 * self sizeUnit.
	responseUponCancel _ ''! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/5/2016 22:06:58'!
initializedInstance
	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		setQuery: 'queryString'
		initialAnswer: 'defaultAnswer'
		acceptOnCR: true.
	aFillInTheBlankMorph responseUponCancel: 'returnOnCancel'.
	^aFillInTheBlankMorph! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/5/2016 22:07:21'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"
	FillInTheBlankMorph request: 'What is your favorite color?'
	"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: self runningWorld activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/5/2016 22:07:51'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"
	FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'
		"

	^ self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: self runningWorld activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/5/2016 22:08:02'!
request: queryString initialAnswer: defaultAnswer onCancel: aBlock
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Evaluate aBlock if the user cancels."
	"
	FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'
		onCancel: [ 'plase play the game!!' ]
		"

	^ (self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: self runningWorld activeHand morphPosition
		onCancelReturn: nil
		acceptOnCR: true)
			ifNil: aBlock! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/5/2016 22:08:09'!
request: queryString onCancel: aBlock
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Evaluate aBlock if the user cancels."
	"
	FillInTheBlankMorph 
		request: 'What is your favorite color?'
		onCancel: [ 'plase play the game!!' ]
	"

	^ (self
		request: queryString
		initialAnswer: ''
		centerAt: self runningWorld activeHand morphPosition
		onCancelReturn: nil
		acceptOnCR: true)
			ifNil: aBlock! !

!methodRemoval: FillInTheBlankMorph class #defaultAnswerExtent!
FillInTheBlankMorph class removeSelector: #defaultAnswerExtent!
!methodRemoval: FillInTheBlankMorph class #request:initialAnswer:centerAt:onCancelReturn:acceptOnCR:answerExtent:!
FillInTheBlankMorph class removeSelector: #request:initialAnswer:centerAt:onCancelReturn:acceptOnCR:answerExtent:!
!methodRemoval: FillInTheBlankMorph #createTextPaneExtent:acceptBoolean:!
FillInTheBlankMorph removeSelector: #createTextPaneExtent:acceptBoolean:!
!methodRemoval: FillInTheBlankMorph #setQuery:initialAnswer:answerExtent:acceptOnCR:!
FillInTheBlankMorph removeSelector: #setQuery:initialAnswer:answerExtent:acceptOnCR:!
