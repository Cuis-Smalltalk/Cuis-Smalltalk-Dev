'From Cuis 4.0 of 3 April 2012 [latest update: #1241] on 4 April 2012 at 2:27:23 pm'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 4/4/2012 14:27'!
                               fileInAnnouncing: announcement 
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: 
			[:bar | 
			[self atEnd] whileFalse: 
					[bar value: self position.
					self skipSeparators.
					
					[val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, because next line is a doit that does the stuff"
									(chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Smalltalk actualCompilerClass evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Smalltalk actualCompilerClass evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex resume: true ]
										]] 
							on: InMidstOfFileinNotification
							do: [:ex | ex resume: true].
					self skipStyleChunk].
			self close].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !

