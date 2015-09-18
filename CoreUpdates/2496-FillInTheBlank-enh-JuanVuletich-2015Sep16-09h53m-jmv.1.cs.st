'From Cuis 4.2 of 25 July 2013 [latest update: #2495] on 16 September 2015 at 10:02:19.803481 am'!

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/16/2015 09:55'!
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
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent)
			ifNil: aBlock! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/16/2015 09:56'!
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
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent)
			ifNil: aBlock! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 9/16/2015 10:00'!
askForCategoryIn: aClass default: aString
	| categories index category |
	categories := OrderedCollection with: 'new ...'. 
	categories addAll: (aClass allMethodCategoriesIntegratedThrough: Object).	
	index := PopUpMenu withCaption: 'Please provide a good category for the new method!!'
						chooseFrom: categories.
	index = 0 ifTrue: [^ aString].
	category := index = 1 ifTrue: [FillInTheBlankMorph request: 'Enter category name:']
						ifFalse: [categories at: index].
	^ category isEmpty ifTrue: [aString] ifFalse: [category]! !

