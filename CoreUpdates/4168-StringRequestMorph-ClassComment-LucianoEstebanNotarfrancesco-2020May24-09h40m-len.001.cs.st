'From Cuis 5.0 [latest update: #4166] on 24 May 2020 at 9:41:25 am'!

!StringRequestMorph commentStamp: '<historical>' prior: 0!
This is a simple morph that allows the user to input a string. The user has the option to cancel (with the Esc key), or input a string and then accept (pressing Enter). For example:

	StringRequestMorph
		request: 'What''s your name?'
		initialAnswer: 'Jose'
		do: [:aString| PopUpMenu inform: 'Hello ', aString, '!!']

Note that the request is not modal and a handler block must be provided. When the user accepts the input, the handler block is called with the user-provided string as argument. There's also the option to handle cancellation:

	StringRequestMorph
		request: 'What''s your name?'
		initialAnswer: 'Jose'
		do: [:aString| PopUpMenu inform: 'Hello ', aString, '!!']
		orCancel: [PopUpMenu inform: 'Ok, nevermind']

And there's the option to provide a validation block that prevents from accepting invalid input:

	StringRequestMorph
		request: 'Guess an even number between 1 and 3'
		initialAnswer: '42'
		verifying: [:aString| aString size > 0 and: [aString allSatisfy: [:each| each isDigit]]]
		do: [:aString| PopUpMenu inform: (aString asInteger = 2 ifTrue: ['Yeah!!'] ifFalse: ['Nope'])]
!

