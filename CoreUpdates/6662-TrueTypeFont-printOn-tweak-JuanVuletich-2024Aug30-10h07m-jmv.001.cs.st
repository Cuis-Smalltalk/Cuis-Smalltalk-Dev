'From Cuis7.1 [latest update: #6628] on 30 August 2024 at 10:09:58 am'!

!TrueTypeFont methodsFor: 'printing' stamp: 'jmv 8/30/2024 10:08:51'!
printOn: aStream
	aStream
		nextPutAll: 'TrueTypeFont(''';
		nextPutAll: self familyName;
		nextPutAll: ''' ';
		space;
		print: self pointSize;
		space;
		nextPutAll:  ttFontDescription subfamilyName;
		nextPut: $).! !

