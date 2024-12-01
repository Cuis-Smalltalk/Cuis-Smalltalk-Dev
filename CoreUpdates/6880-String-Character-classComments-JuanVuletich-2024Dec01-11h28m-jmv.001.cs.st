'From Cuis7.1 [latest update: #6879] on 1 December 2024 at 11:30:23 am'!

!String commentStamp: 'jmv 12/1/2024 11:28:03' prior: 0!
A String is an indexed collection of Characters. In Cuis, Characters are Unicode Characters, each one associated with a numerical Unicode Code Point. In instances of String, all the Characters must be in the two Unicode blocks ("Basic Latin" and "Latin-1 Supplement", meaning the code point is <= 255.
See also UnicodeString, that doesn't have this limitation.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

See UnicodeString.!


!Character commentStamp: '<historical>' prior: 0!
I represent elements in a writing system. In Cuis, instances of Characters are Unicode Characters, each one associated with a numerical Unicode Code Point.

Characters are the elements of the CharacterSequence hierarchy, including String and UnicodeString.!

