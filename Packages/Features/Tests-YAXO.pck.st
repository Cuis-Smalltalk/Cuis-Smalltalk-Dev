'From Cuis 6.0 [latest update: #5622] on 4 January 2023 at 1:00:51 pm'!
'Description '!
!provides: 'Tests-YAXO' 1 2!
!requires: 'YAXO' 1 15 nil!
SystemOrganization addCategory: 'Tests-YAXO'!


!classDefinition: #YAXOTest category: 'Tests-YAXO'!
TestCase subclass: #YAXOTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-YAXO'!
!classDefinition: 'YAXOTest class' category: 'Tests-YAXO'!
YAXOTest class
	instanceVariableNames: ''!


!YAXOTest methodsFor: 'tests' stamp: 'jmv 1/4/2023 12:53:32'!
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
	self assert: heading contents first string = 'Reminder'.
	
	self assert: xmlDocument printString = 
'<?xml  version="1.0" encoding="UTF-8" ?>
<note>
<to>Tove</to>
<from>Jani</from>
<heading>Reminder</heading>
<body>Don''t forget me this weekend!!</body></note>'.! !

!YAXOTest methodsFor: 'tests' stamp: 'jmv 1/4/2023 12:55:54'!
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
	self assert: heading contents first string = '„Sicherheitshinweise“'.
	
	self assert: xmlDocument printString = 
'<?xml  version="1.0" encoding="UTF-8" ?>
<note>
<to>Tove</to>
<from>Jani</from>
<heading>„Sicherheitshinweise“</heading>
<body>Don''t forget me this weekend!!</body></note>' ! !

!YAXOTest methodsFor: 'tests' stamp: 'jmv 1/4/2023 12:56:56'!
test03
	| xmlDocument element subElements |
	xmlDocument _ XMLDOMParser parseDocumentFrom: self exampleString03 readStream.
	self assert: xmlDocument version = '1.0'.
	self assert: xmlDocument encoding = 'UTF-8' .
	self assert: xmlDocument dtd isNil.
	self assert: xmlDocument elements size = 1.
	
	element _ xmlDocument elements first.
	self assert: element name = #breakfast_menu.
	subElements _ element elements.	
	self assert: subElements size = 5.

	element _ subElements third.
	self assert: element name = #food.
	subElements _ element elements.
	self assert: subElements size = 4.

	element _ subElements first.
	self assert: element name = #name.
	self assert: element contents first string = 'Berry-Berry Belgian Waffles'.

	element _ subElements second.
	self assert: element name = #price.
	self assert: element contents first string = '$8.95'.

	element _ subElements third.
	self assert: element name = #description.
	self assert: element contents first string =
		'Light Belgian waffles covered with an assortment of fresh berries and whipped cream'.

	element _ subElements fourth.
	self assert: element name = #calories.
	self assert: element contents first string = '900'.
	
	self assert: xmlDocument printString =
'<?xml  version="1.0" encoding="UTF-8" ?>
<breakfast_menu>               
<food>
<name>Belgian Waffles</name>
<price>$5.95</price>
<description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>
<calories>650</calories></food>
<food>
<name>Strawberry Belgian Waffles</name>
<price>$7.95</price>
<description>Light Belgian waffles covered with strawberries and whipped cream</description>
<calories>900</calories></food>
<food>
<name>Berry-Berry Belgian Waffles</name>
<price>$8.95</price>
<description>Light Belgian waffles covered with an assortment of fresh berries and whipped cream</description>
<calories>900</calories></food>
<food>
<name>French Toast</name>
<price>$4.50</price>
<description>Thick slices made from our homemade sourdough bread</description>
<calories>600</calories></food>
<food>
<name>Homestyle Breakfast</name>
<price>$6.95</price>
<description>Two eggs, bacon or sausage, toast, and our ever-popular hash browns</description>
<calories>950</calories></food></breakfast_menu>' .! !

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

!YAXOTest methodsFor: 'examples' stamp: 'jmv 1/4/2023 12:45:42'!
exampleString03
	"https://www.w3schools.com/xml/note.xml"
	^
'<?xml version="1.0" encoding="UTF-8"?>
<breakfast_menu>
   <food>
       <name>Belgian Waffles</name>
       <price>$5.95</price>
       <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>
       <calories>650</calories> </food>
   <food>
       <name>Strawberry Belgian Waffles</name>
       <price>$7.95</price>
       <description>Light Belgian waffles covered with strawberries and whipped cream</description>
       <calories>900</calories> </food>
   <food>
       <name>Berry-Berry Belgian Waffles</name>
       <price>$8.95</price>
       <description>Light Belgian waffles covered with an assortment of fresh berries and whipped cream</description>
       <calories>900</calories> </food>
   <food>
       <name>French Toast</name>
       <price>$4.50</price>
       <description>Thick slices made from our homemade sourdough bread</description>
       <calories>600</calories> </food>
   <food>
       <name>Homestyle Breakfast</name>
       <price>$6.95</price>
       <description>Two eggs, bacon or sausage, toast, and our ever-popular hash browns</description>
       <calories>950</calories> </food>
</breakfast_menu>'! !
