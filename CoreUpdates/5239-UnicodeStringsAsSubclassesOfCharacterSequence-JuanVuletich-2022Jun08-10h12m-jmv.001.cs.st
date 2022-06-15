'From Cuis 6.0 [latest update: #5237] on 8 June 2022 at 10:22:09 am'!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
!classDefinition: #Utf32String category: #'Kernel-Text'!
CharacterSequence subclass: #Utf32String
	instanceVariableNames: 'codePoints hash '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
!classDefinition: #Utf8String category: #'Kernel-Text'!
CharacterSequence subclass: #Utf8String
	instanceVariableNames: 'bytes count strideIndex hash '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
!classDefinition: #Utf8Symbol category: #'Kernel-Text'!
Utf8String subclass: #Utf8Symbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
!classDefinition: #Utf32String category: #'Kernel-Text'!
CharacterSequence subclass: #Utf32String
	instanceVariableNames: 'codePoints hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
!classDefinition: #Utf8String category: #'Kernel-Text'!
CharacterSequence subclass: #Utf8String
	instanceVariableNames: 'bytes count strideIndex hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
!classRemoval: #EncodedString stamp: 'jmv 6/8/2022 10:21:06'!
Smalltalk removeClassNamed: #EncodedString!

