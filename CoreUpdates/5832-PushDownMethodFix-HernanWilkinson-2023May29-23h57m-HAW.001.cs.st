'From Cuis 6.0 [latest update: #5831] on 30 May 2023 at 12:00:31 am'!

!PushDownMethod methodsFor: 'applying - private' stamp: 'HAW 5/29/2023 23:57:19'!
subclassResponsibilitySourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: method selectorAndArgumentsAsString;
			newLine; newLine;
			tab;
			nextPutAll: 'self ';
			nextPutAll: #subclassResponsibility asString ]! !

