'From Cuis7.1 [latest update: #6714] on 12 September 2024 at 12:11:15 pm'!

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 9/12/2024 12:01:00'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"
	StringRequestMorph request: 'What is your favorite color?'
	"

	^ self request: queryString initialAnswer: '' orCancel: nil! !

