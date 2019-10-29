'From Cuis 5.0 [latest update: #3926] on 23 October 2019 at 11:07:03 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 10/23/2019 11:06:10'!
classRemoved: aClass fromCategory: aCategoryName

	aClass acceptsLoggingOfCompilation
		ifTrue: [
			self
				logChange: aClass definition
				preamble: 'classRemoval: ', aClass name printString, ' stamp: ', Utilities changeStamp printString ]! !
