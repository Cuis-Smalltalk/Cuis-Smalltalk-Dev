'From Cuis 5.0 [latest update: #3926] on 23 October 2019 at 11:21:36 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 10/23/2019 11:20:23'!
methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	aClass acceptsLoggingOfCompilation 
		ifTrue: [ self 
			logChange: aMethod sourceCode 
			preamble: 'methodRemoval: ', aClass printString, '>>#', aSymbol, ' stamp: ', Utilities changeStamp printString ]. ! !

