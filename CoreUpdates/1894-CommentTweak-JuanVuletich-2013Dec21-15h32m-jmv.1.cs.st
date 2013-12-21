'From Cuis 4.2 of 25 July 2013 [latest update: #1893] on 21 December 2013 at 3:32:39.602346 pm'!

!DisplayObject methodsFor: 'fileIn/Out' stamp: 'jmv 12/21/2013 15:28'!
writeOnFileNamed: fileName 
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."
	| file |
	file _ FileStream newFileNamed: fileName.
	file binary.
	file nextPut: 2.  "file code = 2"
	self writeOn: file.
	file close
"
 | f |
[(f _ Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"! !

