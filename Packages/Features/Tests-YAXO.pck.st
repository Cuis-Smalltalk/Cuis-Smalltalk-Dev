'From Cuis 6.0 [latest update: #5625] on 8 January 2023 at 9:00:15 pm'!
'Description '!
!provides: 'Tests-YAXO' 1 3!
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

!YAXOTest methodsFor: 'tests' stamp: 'jmv 1/8/2023 16:16:28'!
test04
	| xmlDocument element subElements |
	xmlDocument _ XMLDOMParser parseDocumentFrom: self exampleString04 readStream.
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
		'ლორემ იფსუმ დოლორ სით ამეთ, ყუო უთ დიცათ ალიყუიდ, ად ლიბრის ცეთეროს რეფრეჰენდუნთ ესთ.'.

	element _ subElements fourth.
	self assert: element name = #calories.
	self assert: element contents first string = '900'.
	
	self assert: xmlDocument printString =
'<?xml  version="1.0" encoding="UTF-8" ?>
<breakfast_menu>               
<food>
<name>Belgian Waffles</name>
<price>$5.95</price>
<description>¿Querés agüita, ñandú?</description>
<calories>650</calories></food>
<food>
<name>Strawberry Belgian Waffles</name>
<price>$7.95</price>
<description>Лорем ипсум долор сит амет, дебет салутанди меи еи.</description>
<calories>900</calories></food>
<food>
<name>Berry-Berry Belgian Waffles</name>
<price>$8.95</price>
<description>ლორემ იფსუმ დოლორ სით ამეთ, ყუო უთ დიცათ ალიყუიდ, ად ლიბრის ცეთეროს რეფრეჰენდუნთ ესთ.</description>
<calories>900</calories></food>
<food>
<name>French Toast</name>
<price>$4.50</price>
<description>Εξ σεα ελιτ δισσεντιετ, μαλισ τριτανι vισ ιδ.</description>
<calories>600</calories></food>
<food>
<name>Homestyle Breakfast</name>
<price>$6.95</price>
<description>¿Querés agüita, ñandú?</description>
<calories>950</calories></food></breakfast_menu>' .! !

!YAXOTest methodsFor: 'tests' stamp: 'jmv 1/8/2023 16:35:43'!
test05
	| xmlDocument element subElements |
	xmlDocument _ XMLDOMParser parseDocumentFrom: self exampleString05 readStream.
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
	self assert: element name = 'agüita'.
	self assert: element contents first string = 'Berry-Berry Belgian Waffles'.

	element _ subElements second.
	self assert: element name = 'ñandú'.
	self assert: element contents first string = '$8.95'.

	element _ subElements third.
	self assert: element name = 'Лорем'.
	self assert: element contents first string =
		'ლორემ იფსუმ დოლორ სით ამეთ, ყუო უთ დიცათ ალიყუიდ, ად ლიბრის ცეთეროს რეფრეჰენდუნთ ესთ.'.

	element _ subElements fourth.
	self assert: element name = 'ελιτ'.
	self assert: element contents first string = '900'.
	
	self assert: xmlDocument printString =
'<?xml  version="1.0" encoding="UTF-8" ?>
<breakfast_menu>               
<food>
<agüita>Belgian Waffles</agüita>
<ñandú>$5.95</ñandú>
<Лорем>¿Querés agüita, ñandú?</Лорем>
<ελιτ>650</ελιτ></food>
<food>
<agüita>Strawberry Belgian Waffles</agüita>
<ñandú>$7.95</ñandú>
<Лорем>Лорем ипсум долор сит амет, дебет салутанди меи еи.</Лорем>
<ελιτ>900</ελιτ></food>
<food>
<agüita>Berry-Berry Belgian Waffles</agüita>
<ñandú>$8.95</ñandú>
<Лорем>ლორემ იფსუმ დოლორ სით ამეთ, ყუო უთ დიცათ ალიყუიდ, ად ლიბრის ცეთეროს რეფრეჰენდუნთ ესთ.</Лорем>
<ελιτ>900</ελιτ></food>
<food>
<agüita>French Toast</agüita>
<ñandú>$4.50</ñandú>
<Лорем>Εξ σεα ελιτ δισσεντιετ, μαλισ τριτανι vισ ιδ.</Лорем>
<ελιτ>600</ελιτ></food>
<food>
<agüita>Homestyle Breakfast</agüita>
<ñandú>$6.95</ñandú>
<Лорем>¿Querés agüita, ñandú?</Лорем>
<ελιτ>950</ελιτ></food></breakfast_menu>' .! !

!YAXOTest methodsFor: 'tests' stamp: 'jmv 1/8/2023 20:25:51'!
test06
	| xmlDocument element subElements heading |
	xmlDocument _ XMLDOMParser parseDocumentFrom: self exampleString06 readStream.
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
<to>Tovep̂</to>
<from>Jani</from>
<heading>Reminder</heading>
<body>Don''t forget me this weekend!!</body></note>'.! !

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

!YAXOTest methodsFor: 'examples' stamp: 'jmv 1/8/2023 20:47:51'!
exampleString02
	"https://www.w3schools.com/xml/note.xml"
	^
'<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>&#8222;Sicherheitshinweise&#x201C;</heading>
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

!YAXOTest methodsFor: 'examples' stamp: 'jmv 1/8/2023 16:14:58'!
exampleString04
	"Exercise a bit Unicode."
	^
'<?xml version="1.0" encoding="UTF-8"?>
<breakfast_menu>
   <food>
       <name>Belgian Waffles</name>
       <price>$5.95</price>
       <description>¿Querés agüita, ñandú?</description>
       <calories>650</calories> </food>
   <food>
       <name>Strawberry Belgian Waffles</name>
       <price>$7.95</price>
       <description>Лорем ипсум долор сит амет, дебет салутанди меи еи.</description>
       <calories>900</calories> </food>
   <food>
       <name>Berry-Berry Belgian Waffles</name>
       <price>$8.95</price>
       <description>ლორემ იფსუმ დოლორ სით ამეთ, ყუო უთ დიცათ ალიყუიდ, ად ლიბრის ცეთეროს რეფრეჰენდუნთ ესთ.</description>
       <calories>900</calories> </food>
   <food>
       <name>French Toast</name>
       <price>$4.50</price>
       <description>Εξ σεα ελιτ δισσεντιετ, μαλισ τριτανι vισ ιδ.</description>
       <calories>600</calories> </food>
   <food>
       <name>Homestyle Breakfast</name>
       <price>$6.95</price>
       <description>¿Querés agüita, ñandú?</description>
       <calories>950</calories> </food>
</breakfast_menu>'! !

!YAXOTest methodsFor: 'examples' stamp: 'jmv 1/8/2023 16:19:32'!
exampleString05
	"Exercise a bit Unicode."
	^
'<?xml version="1.0" encoding="UTF-8"?>
<breakfast_menu>
   <food>
       <agüita>Belgian Waffles</agüita>
       <ñandú>$5.95</ñandú>
       <Лорем>¿Querés agüita, ñandú?</Лорем>
       <ελιτ>650</ελιτ> </food>
   <food>
       <agüita>Strawberry Belgian Waffles</agüita>
       <ñandú>$7.95</ñandú>
       <Лорем>Лорем ипсум долор сит амет, дебет салутанди меи еи.</Лорем>
       <ελιτ>900</ελιτ> </food>
   <food>
       <agüita>Berry-Berry Belgian Waffles</agüita>
       <ñandú>$8.95</ñandú>
       <Лорем>ლორემ იფსუმ დოლორ სით ამეთ, ყუო უთ დიცათ ალიყუიდ, ად ლიბრის ცეთეროს რეფრეჰენდუნთ ესთ.</Лорем>
       <ελιτ>900</ελιτ> </food>
   <food>
       <agüita>French Toast</agüita>
       <ñandú>$4.50</ñandú>
       <Лорем>Εξ σεα ελιτ δισσεντιετ, μαλισ τριτανι vισ ιδ.</Лорем>
       <ελιτ>600</ελιτ> </food>
   <food>
       <agüita>Homestyle Breakfast</agüita>
       <ñandú>$6.95</ñandú>
       <Лорем>¿Querés agüita, ñandú?</Лорем>
       <ελιτ>950</ελιτ> </food>
</breakfast_menu>'! !

!YAXOTest methodsFor: 'examples' stamp: 'jmv 1/8/2023 20:25:42'!
exampleString06
	"Exercise a bit Unicode."
	^
'<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tovep̂</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don''t forget me this weekend!!</body>
</note>'! !
