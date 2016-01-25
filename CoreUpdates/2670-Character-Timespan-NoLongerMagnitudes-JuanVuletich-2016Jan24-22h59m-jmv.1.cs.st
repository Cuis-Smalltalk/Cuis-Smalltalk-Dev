'From Cuis 4.2 of 25 July 2013 [latest update: #2669] on 24 January 2016 at 11:01:57.902396 pm'!
!classDefinition: #Character category: #'Kernel-Text'!
Object subclass: #Character
	instanceVariableNames: 'value '
	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Magnitude commentStamp: '<historical>' prior: 0!
I'm the abstract class Magnitude that provides common protocol for objects that have
the ability to be compared along a linear dimension, such as dates or times.
Subclasses of Magnitude include Date, ArithmeticValue, and Time, as well as LookupKey.
 
 
My subclasses should implement
  < aMagnitude 
  = aMagnitude 
  hash

Here are some example of my protocol:
     3 > 4
     5 = 6
     100 max: 9
	7 between: 5 and: 10 
!

!classDefinition: #Timespan category: #'Kernel-Chronology'!
Object subclass: #Timespan
	instanceVariableNames: 'start duration '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #Character category: #'Kernel-Text'!
Object subclass: #Character
	instanceVariableNames: 'value'
	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit'
	poolDictionaries: ''
	category: 'Kernel-Text'!
!classDefinition: #Timespan category: #'Kernel-Chronology'!
Object subclass: #Timespan
	instanceVariableNames: 'start duration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
