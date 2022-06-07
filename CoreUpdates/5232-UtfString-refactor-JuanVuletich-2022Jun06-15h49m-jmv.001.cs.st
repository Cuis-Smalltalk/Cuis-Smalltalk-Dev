'From Cuis 6.0 [latest update: #5230] on 6 June 2022 at 3:50:59 pm'!
!classDefinition: #EncodedString category: #'System-Text'!
SequenceableCollection subclass: #EncodedString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #Utf32String category: #'System-Text'!
EncodedString subclass: #Utf32String
	instanceVariableNames: 'codePoints hash '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #Utf8String category: #'System-Text'!
EncodedString subclass: #Utf8String
	instanceVariableNames: 'bytes count strideIndex hash '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #Utf32String category: #'System-Text'!
EncodedString subclass: #Utf32String
	instanceVariableNames: 'codePoints hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #Utf8String category: #'System-Text'!
EncodedString subclass: #Utf8String
	instanceVariableNames: 'bytes count strideIndex hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
