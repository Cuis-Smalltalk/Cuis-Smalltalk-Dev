'From Cuis 4.1 of 12 December 2012 [latest update: #1761] on 17 July 2013 at 10:03:28.730807 am'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 7/17/2013 10:01'!
writeFeatureSpecOn: aStream
	| provides |
	provides _ featureSpec provides.
	aStream
		nextPut: $!!; 
		nextChunkPut: 'provides: ', provides name asString printString, ' ', provides version printString, ' ', provides revision printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!!; 
			nextChunkPut: 'requires: ', requires name asString printString, ' ', requires version printString, ' ', requires revision printString;
			newLine ]! !

