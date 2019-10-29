'From Cuis 5.0 [latest update: #3935] on 28 October 2019 at 1:24:17 am'!

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 10/28/2019 01:23:36'!
computeMessageEntriesForClass: aClass withPreviousBinaryMessageSend: possibleBinaryMessageSendRange

	selectorsClasses := Array with: aClass.
	
	entries := (AutoCompleterSelectorsCollector for: prefix) 
		addUnaryAndBinarySelectorsOf: aClass;
		entriesToShow! !

