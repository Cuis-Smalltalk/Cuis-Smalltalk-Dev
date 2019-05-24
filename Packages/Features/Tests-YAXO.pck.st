'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 5:16:42 pm'!
'Description '!
!provides: 'Tests-YAXO' 1 1!
!requires: 'YAXO' 1 15 nil!
SystemOrganization addCategory: #'Tests-YAXO'!


!classDefinition: #YAXOTest category: #'Tests-YAXO'!
TestCase subclass: #YAXOTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-YAXO'!
!classDefinition: 'YAXOTest class' category: #'Tests-YAXO'!
YAXOTest class
	instanceVariableNames: ''!


!YAXOTest methodsFor: 'tests' stamp: 'jmv 6/7/2018 10:58:14'!
test01
	| xmlDocument element subElements heading |
	xmlDocument _ XMLDOMParser parseDocumentFrom: self exampleString01 readStream.
	self assert: xmlDocument version = '1.0'.
	self assert: xmlDocument encoding = 'UTF-8' .
	self assert: xmlDocument dtd isNil.
	self assert: xmlDocument elements size = 1.
	element _ xmlDocument elements first.
	self assert: element name = #note.
	subElements _ element elements.
	self assert: subElements size = 4.
	heading _ subElements third.
	self assert: heading name = #heading.
	self assert: heading contents first string = 'Reminder'! !

!YAXOTest methodsFor: 'tests' stamp: 'jmv 6/7/2018 11:42:02'!
test02
	| xmlDocument element subElements heading |
	xmlDocument _ XMLDOMParser parseDocumentFrom: self exampleString02 readStream.
	self assert: xmlDocument version = '1.0'.
	self assert: xmlDocument encoding = 'UTF-8' .
	self assert: xmlDocument dtd isNil.
	self assert: xmlDocument elements size = 1.
	element _ xmlDocument elements first.
	self assert: element name = #note.
	subElements _ element elements.
	self assert: subElements size = 4.
	heading _ subElements third.
	self assert: heading name = #heading.
	self assert: heading contents first string = '&#8222;Sicherheitshinweise&#8220;'! !

!YAXOTest methodsFor: 'examples' stamp: 'jmv 6/7/2018 11:14:23'!
exampleString01
	"https://www.w3schools.com/xml/note.xml"
	^
'<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don''t forget me this weekend!!</body>
</note>'! !

!YAXOTest methodsFor: 'examples' stamp: 'jmv 6/7/2018 11:41:58'!
exampleString02
	"https://www.w3schools.com/xml/note.xml"
	^
'<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>&#8222;Sicherheitshinweise&#8220;</heading>
  <body>Don''t forget me this weekend!!</body>
</note>'! !
