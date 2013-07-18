'From Cuis 4.1 of 12 December 2012 [latest update: #1602] on 20 February 2013 at 7:43:49 pm'!
!classDefinition: #UnicodeTest category: #'Unicode-Tests'!
TestCase subclass: #UnicodeTest
	instanceVariableNames: 'bytesOfExample1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Unicode-Tests'!

!UnicodeTest methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 19:27'!
setUp
	"self new setUp"

  	| stream |
	
	
     bytesOfExample1 := #[16r61 16r62 16r63 16r20 16rC3 16rA0 16rC3 16rA8 16rE2 
	                              16r82 16rAC 16r20 16rCE 16rB1 16rCE 16rB2 16rCE 16rB3].
	
	"see UnicodeNotes.md"
	
     "write the bytes of an UFT8 encoded string in binary mode to a file"
     stream := (FileStream  forceNewFileNamed:  self class fileName) binary.
     stream nextPutAll: bytesOfExample1.
     stream close.! !

!UnicodeTest methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 21:29'!
test1ReadBinary
	
	"see UnicodeNotes.md"
	
     "self new test1ReadBinary"
     | stream content |

	stream := (FileStream  fileNamed:  self class fileName) binary.
      content := stream contentsOfEntireFile.
	self assert: content = bytesOfExample1.
      stream close.! !

!UnicodeTest methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 21:30'!
test2ReadWithOutBinary
	
	"see UnicodeNotes.md"
	
     "self new test2ReadWithOutBinary"
     | stream content |

	stream := (FileStream  fileNamed:  self class fileName).
      content := stream contentsOfEntireFile.
	self deny: content = bytesOfExample1.
      stream close.! !

!UnicodeTest methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 21:30'!
test3ReadUtf8
	
	"see UnicodeNotes.md"
	
     "self new test3ReadUtf8"
     | stream content byteArray |

	stream := (FileStream  fileNamed:  self class fileName) binary.
	byteArray := stream contentsOfEntireFile.
      content := String fromUtf8: byteArray.
	self assert: content = 'abc ‡Ë§ &#945;&#946;&#947;'.
      stream close.! !

!UnicodeTest methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 21:30'!
test4BackConversion
	
	"see UnicodeNotes.md"
	
     "self new test4BackConversion"
     | stream contentInternalString contentByteArray |

	stream := (FileStream  fileNamed:  self class fileName) binary.
      contentInternalString := String fromUtf8: stream contentsOfEntireFile.
      stream close.

	stream := (FileStream  fileNamed:  self class fileName) binary.
	contentByteArray := stream contentsOfEntireFile.
	self assert:   contentByteArray = (contentInternalString asUtf8: true).
	stream close.
! !

!UnicodeTest methodsFor: 'as yet unclassified' stamp: 'hjh 2/13/2013 16:12'!
test5ReadWriteUtf8
	
	"see UnicodeNotes.md"
	
     "self new test5ReadWriteUtf8"
     | stream content byteArray byteArray2 |

	"read UTF8 Unicode file into internal string with NCRs"
	"for NCR see http://en.wikipedia.org/wiki/Numeric_character_reference"
	
	stream := (FileStream  fileNamed:  self class fileName) binary.
	byteArray := stream contentsOfEntireFile.
      content := String fromUtf8: byteArray.
	"NCRs were added to 'content' as needed"

	"write internal string back to UTF8 file with NCRs converted back to UTF8 chars"
	stream := (FileStream  forceNewFileNamed:  self class fileName2) binary.
	stream nextPutAll: (content asUtf8: true).  "true means: convert NCRs back to UTF8"
	stream close.

      "compare the two versions: what is in file 'fileName' with what is in file 'fileName2'"
	stream := (FileStream  fileNamed:  self class fileName) binary.
	byteArray := stream contentsOfEntireFile.
	stream close.

	stream := (FileStream  fileNamed:  self class fileName2) binary.
	byteArray2 := stream contentsOfEntireFile.
	stream close.

	self assert: byteArray = byteArray2.
     ! !


!UnicodeTest class methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 19:25'!
fileName
	^'UTF8abc-test.txt'! !

!UnicodeTest class methodsFor: 'as yet unclassified' stamp: 'hjh 2/12/2013 21:35'!
fileName2
	^'UTF8abc-test2.txt'! !

